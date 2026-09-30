;;; Generate rollback-capable HCL event actions from the Scheme Grammar IR.
(import (only-in :std/misc/ports read-all-as-string)
        (only-in :gerbil-parser/languages/hcl/v2-24/grammar
                 hcl-v2-24-parser-ir hcl-v2-24-parser)
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-grammar-digest))
(def (rule-procedure name)
  (string->symbol (string-append "events-" (symbol->string name))))

;;; This closed ASCII subset is safe only while the HCL lexical declarations
;;; retain these exact meanings and precedence relationships. A new lexical
;;; rule must be reviewed before the generated fast path is regenerated.
(def +fast-lexical-contract+
  '((horizontal-whitespace (horizontal-whitespace+))
    (newline (newline+))
    (comment (line-comment "#" "//"))
    (block-comment-token (block-comment "/*" "*/"))
    (heredoc (heredoc))
    (string (quoted-string "\""))
    (number (number))
    (identifier (identifier))
    (punctuation
     (literals "==" "!=" "<=" ">=" "&&" "||"
               "{" "}" "[" "]" "(" ")" "=" "," "." ":"
               "+" "-" "*" "/" "%" "!" "<" ">" "?"))
    (unknown (fallback))))

;;; An emitted expression returns the next significant-token index or #f.
;;; Zero is a valid position, so even an empty match is distinct from failure.
(def (make-expression-compiler)
  (let (counter 0)
    (def (fresh prefix)
      (let (name (string->symbol
                  (string-append prefix (number->string counter))))
        (set! counter (fx+ counter 1))
        name))
    (def (compile-sequence-steps remaining pos mark)
      (if (null? remaining)
        pos
        (let (next (fresh "next"))
          `(let (,next ,(compile-expression (car remaining) pos))
             (if ,next
               ,(compile-sequence-steps (cdr remaining) next mark)
               (begin (rollback! ,mark) #f))))))
    (def (compile-choice-steps choices pos mark)
      (if (null? choices)
        #f
        (let (next (fresh "next"))
          `(let (,next ,(compile-expression (car choices) pos))
             (if ,next
               ,next
               (begin
                 (rollback! ,mark)
                 ,(compile-choice-steps (cdr choices) pos mark)))))))
    (def (compile-terminal expected literal? pos)
      (let (test (if literal?
                  `(equal? (token-lexeme input) ,expected)
                  `(eq? (token-kind input) ',expected)))
        `(if (< ,pos limit)
           (let (input (vector-ref significant ,pos))
             (if ,test
               (begin
                 (emit-event! 'token input 0)
                 (fx+ ,pos 1))
               #f))
           #f)))
    (def (compile-repeat child required? pos)
      (let ((loop-name (fresh "repeat-loop"))
            (next (fresh "next")) (count (fresh "count"))
            (mark (fresh "mark")) (after (fresh "after")))
        `(let ,loop-name ((,next ,pos) (,count 0))
           (let (,mark event-count)
             (let (,after ,(compile-expression child next))
               (if ,after
                 (begin
                   (when (= ,after ,next)
                     (error "zero-width repeated grammar expression"))
                   (,loop-name ,after (fx+ ,count 1)))
                 (begin
                   (rollback! ,mark)
                   ,(if required?
                      `(if (> ,count 0) ,next #f)
                      next))))))))
    (def (compile-wrapper name child field? pos)
      (let ((mark (fresh "mark")) (open (fresh "open"))
            (next (fresh "next")))
        `(let (,mark event-count)
           (emit-event! ',(if field? 'open-field 'open-node)
                        ',name (offset ,pos))
           (let (,open event-count)
             (let (,next ,(compile-expression child pos))
               (if ,next
                 (begin
                   ,(if field?
                      `(if (= event-count ,open)
                         (rollback! ,mark)
                         (emit-event! 'close-field ',name
                                      (range-end ,pos ,next)))
                      `(emit-event! 'close-node ',name
                                    (range-end ,pos ,next)))
                   ,next)
                 (begin (rollback! ,mark) #f)))))))
    (def (compile-expression expr pos)
      (match expr
        (['empty] pos)
        (['literal expected] (compile-terminal expected #t pos))
        (['token expected] (compile-terminal expected #f pos))
        (['reference name] `(,(rule-procedure name) ,pos))
        (['sequence . children]
         (let (mark (fresh "mark"))
           `(let (,mark event-count)
              ,(compile-sequence-steps children pos mark))))
        (['choice . choices]
         (let (mark (fresh "mark"))
           `(let (,mark event-count)
              ,(compile-choice-steps choices pos mark))))
        (['optional child]
         (let ((mark (fresh "mark")) (next (fresh "next")))
           `(let (,mark event-count)
              (let (,next ,(compile-expression child pos))
                (if ,next ,next
                  (begin (rollback! ,mark) ,pos))))))
        (['repeat child] (compile-repeat child #f pos))
        (['repeat1 child] (compile-repeat child #t pos))
        (['field name child] (compile-wrapper name child #t pos))
        (['alias name child] (compile-wrapper name child #f pos))
        (['precedence _ _ child] (compile-expression child pos))
        (else (error "unsupported grammar expression" expr))))
    compile-expression))
(def (module-expression)
  (let ((compile-expression (make-expression-compiler))
        (rules (cdr (assq 'rules hcl-v2-24-parser-ir))))
    (unless (equal? (cdr (assq 'lexical-rules hcl-v2-24-parser-ir))
                    +fast-lexical-contract+)
      (error "HCL fast lexer requires lexical contract review"))
    `(begin
     (import (only-in :gerbil-parser/src/compiler/machine
                      parser-machine-trivia)
             (only-in :gerbil-parser/src/runtime/lexer lex-source)
             (only-in :gerbil-parser/src/runtime/significant
                      parser-significant-tokens)
             (only-in :gerbil-parser/src/runtime/token
                      make-token token-kind token-lexeme token-start token-end)
             (only-in :gerbil-parser/src/runtime/artifact
                      make-success-parse-artifact/raw-event-tape))
     (export direct-parse-hcl direct-lex-hcl direct-hcl-grammar-digest)
     (def direct-hcl-grammar-digest
       ,(parser-machine-grammar-digest hcl-v2-24-parser))
     (def (ascii-alpha? ch)
       (let (code (char->integer ch))
         (or (<= 65 code 90) (<= 97 code 122) (= code 95))))
     (def (ascii-digit? ch)
       (let (code (char->integer ch)) (<= 48 code 57)))
     (def (ascii-ident-rest? ch)
       (or (ascii-alpha? ch) (ascii-digit? ch) (char=? ch #\-)))
     (def (direct-lex-hcl source)
       (let (length (string-length source))
         (let loop ((start 0) (tokens '()))
           (if (= start length)
             (reverse tokens)
             (let* ((ch (string-ref source start))
                    (kind
                     (cond
                      ((ascii-alpha? ch) 'identifier)
                      ((ascii-digit? ch) 'number)
                      ((or (char=? ch #\space) (char=? ch #\tab))
                       'horizontal-whitespace)
                      ((or (char=? ch #\newline) (char=? ch #\return))
                       'newline)
                      ((char=? ch #\=) 'punctuation)
                      (else #f))))
               (if (not kind)
                 #f
                 (let scan ((end (fx+ start 1)))
                   (if (and (< end length)
                            (let (next (string-ref source end))
                              (case kind
                                ((identifier) (ascii-ident-rest? next))
                                ((number) (ascii-digit? next))
                                ((horizontal-whitespace)
                                 (or (char=? next #\space)
                                     (char=? next #\tab)))
                                ((newline)
                                 (or (char=? next #\newline)
                                     (char=? next #\return)))
                                (else #f))))
                     (scan (fx+ end 1))
                     (if (and (eq? kind 'number) (< end length)
                              (let (next (string-ref source end))
                                (or (char=? next #\.) (char=? next #\e)
                                    (char=? next #\E))))
                       #f
                       (if (and (eq? kind 'punctuation) (< end length)
                                (char=? (string-ref source end) #\=))
                         #f
                         (loop end
                               (cons (make-token kind
                                                 (substring source start end)
                                                 start end)
                                     tokens))))))))))))
     (def (direct-parse-hcl machine source)
       (let* ((tokens (or (direct-lex-hcl source)
                          (lex-source machine source)))
              (significant
               (list->vector (parser-significant-tokens machine tokens)))
              (limit (vector-length significant))
              (source-bytes (string->utf8 source))
              (byte-length (u8vector-length source-bytes))
              (events (make-vector (* 3 (max 64 (* 6 (length tokens)))) #f))
              (event-count 0))
         (def (offset pos)
           (if (< pos limit)
             (token-start (vector-ref significant pos))
             byte-length))
         (def (range-end start next)
           (if (= start next) (offset start)
             (token-end (vector-ref significant (fx- next 1)))))
         (def (emit-event! operation name byte-offset)
           (when (= (* 3 event-count) (vector-length events))
             (let* ((old events)
                    (grown (make-vector (* 2 (vector-length old)) #f)))
               (let loop ((i 0))
                 (when (< i (* 3 event-count))
                   (vector-set! grown i (vector-ref old i))
                   (loop (fx+ i 1))))
               (set! events grown)))
           (let (base (* 3 event-count))
             (vector-set! events base operation)
             (vector-set! events (fx+ base 1) name)
             (vector-set! events (fx+ base 2) byte-offset))
           (set! event-count (fx+ event-count 1)))
         (def (rollback! mark)
           (set! event-count mark))
         (letrec
             ,(map (lambda (row)
                     `(,(rule-procedure (car row))
                       (lambda (pos)
                         ,(compile-expression (cadr row) 'pos))))
                   rules)
           (let (end (events-config-file 0))
             (if (and end (= end limit))
               (make-success-parse-artifact/raw-event-tape
                direct-hcl-grammar-digest source tokens events event-count
                (parser-machine-trivia machine) source-bytes)
               #f))))))))
;;; Write the generated syntax as source, with one readable form per line of
;;; structure. `write` still owns escaping of symbols, strings, and literals.
;;; Keep the expanded module below the native source-policy limit of 1000 lines.
(def +generated-line-width+ 190)
(def (fits-on-line? form indent)
  (<= (+ indent
         (string-length
          (call-with-output-string (lambda (port) (write form port)))))
      +generated-line-width+))

(def (write-generated-form form port (indent 0))
  (cond
   ((or (not (pair? form)) (fits-on-line? form indent))
    (write form port))
   (else
    (display "(" port)
    (write-generated-form (car form) port (fx+ indent 2))
    (let loop ((rest (cdr form)) (first? #t))
      (cond
       ((null? rest) (display ")" port))
       ((pair? rest)
        (if (and first?
                 (symbol? (car form))
                 (memq (car form) '(def lambda let let* letrec if)))
          (display " " port)
          (if (pair? (car rest))
            (begin
              (newline port)
              (display (make-string (fx+ indent 2) #\space) port))
            (display " " port)))
        (write-generated-form (car rest) port (fx+ indent 2))
        (loop (cdr rest) #f))
       (else
        (display " . " port)
        (write-generated-form rest port (fx+ indent 2))
        (display ")" port)))))))

(def (emit-module port)
  (display ";;; -*- Gerbil -*-\n" port)
  (display ";;; Generated by languages/hcl/v2-24/generate-direct.ss from Grammar IR.\n" port)
  (display ";;; Edit the generator, then regenerate this module.\n\n" port)
  (let loop ((forms (cdr (module-expression))))
    (unless (null? forms)
      (write-generated-form (car forms) port)
      (newline port)
      (when (pair? (cdr forms)) (newline port))
      (loop (cdr forms)))))
(def (main . args)
  (cond
   ((and (= (length args) 2) (equal? (car args) "module"))
    (call-with-output-file (cadr args) emit-module))
   ((and (= (length args) 2) (equal? (car args) "check"))
    (let ((expected (call-with-output-string emit-module))
          (repeated (call-with-output-string emit-module))
          (actual (call-with-input-file (cadr args) read-all-as-string)))
      (unless (string=? expected repeated)
        (error "generated HCL parser depends on prior compiler state"))
      (unless (string=? expected actual)
        (error "generated HCL event-action parser is stale" (cadr args)))
      (displayln "GENERATED-HCL-RECURSIVE-OK")))
   (else (error "expected module|check source path" args))))
(export main)

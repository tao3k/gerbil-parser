;;; Generate rollback-capable HCL event actions from the Scheme Grammar IR.
(import (only-in :std/misc/ports read-all-as-string)
        (only-in :gerbil-parser/languages/hcl/v2-24/grammar
                 hcl-v2-24-parser-ir hcl-v2-24-parser)
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-grammar-digest))
(def (rule-procedure name)
  (string->symbol (string-append "events-" (symbol->string name))))

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
                 (emit-raw! input)
                 (fx+ ,pos 1))
               #f))
           #f)))
    (def (compile-repeat child required? pos)
      (let ((loop-name (fresh "repeat-loop"))
            (next (fresh "next")) (count (fresh "count"))
            (mark (fresh "mark")) (after (fresh "after")))
        `(let ,loop-name ((,next ,pos) (,count 0))
           (let (,mark tail)
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
        `(let (,mark tail)
           (emit-raw!
            (make-raw-parse-event ',(if field? 'open-field 'open-node)
                                  ',name (offset ,pos)))
           (let (,open tail)
             (let (,next ,(compile-expression child pos))
               (if ,next
                 (begin
                   ,(if field?
                      `(if (eq? tail ,open)
                         (rollback! ,mark)
                         (emit-raw!
                          (make-raw-parse-event
                           'close-field ',name (range-end ,pos ,next))))
                      `(emit-raw!
                        (make-raw-parse-event
                         'close-node ',name (range-end ,pos ,next))))
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
           `(let (,mark tail)
              ,(compile-sequence-steps children pos mark))))
        (['choice . choices]
         (let (mark (fresh "mark"))
           `(let (,mark tail)
              ,(compile-choice-steps choices pos mark))))
        (['optional child]
         (let ((mark (fresh "mark")) (next (fresh "next")))
           `(let (,mark tail)
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
    `(begin
     (import (only-in :gerbil-parser/src/compiler/machine
                      parser-machine-trivia)
             (only-in :gerbil-parser/src/runtime/lexer lex-source)
             (only-in :gerbil-parser/src/runtime/significant
                      parser-significant-tokens)
             (only-in :gerbil-parser/src/runtime/token
                      token-kind token-lexeme token-start token-end)
             (only-in :gerbil-parser/src/runtime/artifact
                      make-raw-parse-event
                      make-success-parse-artifact/raw-events))
     (export direct-parse-hcl direct-hcl-grammar-digest)
     (def direct-hcl-grammar-digest
       ,(parser-machine-grammar-digest hcl-v2-24-parser))
     (def (direct-parse-hcl machine source)
       (let* ((tokens (lex-source machine source))
              (significant
               (list->vector (parser-significant-tokens machine tokens)))
              (limit (vector-length significant))
              (source-bytes (string->utf8 source))
              (byte-length (u8vector-length source-bytes))
              (head (cons #f '()))
              (tail head))
         (def (offset pos)
           (if (< pos limit)
             (token-start (vector-ref significant pos))
             byte-length))
         (def (range-end start next)
           (if (= start next) (offset start)
             (token-end (vector-ref significant (fx- next 1)))))
         (def (emit-raw! raw)
           (let (cell (cons raw '()))
             (set-cdr! tail cell)
             (set! tail cell)))
         (def (rollback! mark)
           (set-cdr! mark '())
           (set! tail mark))
         (letrec
             ,(map (lambda (row)
                     `(,(rule-procedure (car row))
                       (lambda (pos)
                         ,(compile-expression (cadr row) 'pos))))
                   rules)
           (let (end (events-config-file 0))
             (if (and end (= end limit))
               (make-success-parse-artifact/raw-events
                direct-hcl-grammar-digest source tokens (cdr head)
                (parser-machine-trivia machine) source-bytes)
               #f))))))))
;;; Write the generated syntax as source, with one readable form per line of
;;; structure. `write` still owns escaping of symbols, strings, and literals.
(def (fits-on-line? form indent)
  (<= (+ indent
         (string-length
          (call-with-output-string (lambda (port) (write form port)))))
      96))

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

;;; -*- Gerbil -*-
;;; Experimental fused Scheme LR generator for the arithmetic benchmark.
;;; This consumes the compiled Parser IR; the grammar remains the sole author.
(import (only-in :gerbil-parser/languages/arithmetic/grammar
                 arithmetic-parser-ir arithmetic-parser)
        (only-in :std/misc/ports read-all-as-string)
        (only-in :gerbil-parser/src/compiler/machine
                 parser-machine-grammar-digest)
        (only-in :gerbil-parser/src/compiler/lr
                 lr-spec-ref production-table production-lhs production-rhs
                 compute-nullable)
        (only-in :gerbil-parser/src/compiler/fused-reduction fused-production-expression)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-prepare lr-runtime-lexical-mode-catalog
                 lr-lexical-mode-terminals))

(def spec (cdr (assq 'lr-spec arithmetic-parser-ir)))
(def actions (lr-spec-ref spec 'actions))
(def gotos (lr-spec-ref spec 'gotos))
(def table (production-table (lr-spec-ref spec 'productions)))
(def nullable-index
  (let-values (((_names index) (compute-nullable (lr-spec-ref spec 'productions)))) index))
(def mode-catalog
  (lr-runtime-lexical-mode-catalog (lr-prepare spec)))

(def (state-mode-id state)
  (let (terminals (map car (vector-ref actions state)))
    (let loop ((index 0))
      (if (= index (vector-length mode-catalog))
        (error "LR lexical mode missing from prepared catalog" state)
        (if (equal? terminals
                    (lr-lexical-mode-terminals
                     (vector-ref mode-catalog index)))
          index
          (loop (fx+ index 1)))))))

(def (nth-tail name count)
  (let loop ((n count) (value name))
    (if (zero? n) value (loop (- n 1) `(cdr ,value)))))

(def (goto-expression lhs)
  (let ((clauses '()))
    (let loop ((i 0))
      (when (< i (vector-length gotos))
        (let (entry (assoc lhs (vector-ref gotos i)))
          (when entry
            (set! clauses (cons `((,i) ,(cdr entry)) clauses))))
        (loop (+ i 1))))
    `(case (car remaining-states)
       ,@(reverse clauses)
       (else #f))))

(def (reduce-expression production-id (stream? #f))
  (let* ((production (vector-ref table production-id))
         (count (length (production-rhs production)))
         (bindings
          (let loop ((i 0) (acc '()))
            (if (= i count)
              (reverse acc)
              (loop (+ i 1)
                    (cons (list (string->symbol
                                 (string-append "v" (number->string i)))
                                `(car ,(nth-tail 'semantic-values (- count i 1))))
                          acc)))))
         (remaining-states (nth-tail 'states count))
         (remaining-values (nth-tail 'semantic-values count)))
    `(let* ((remaining-states ,remaining-states)
            (remaining-values ,remaining-values)
            ,@bindings
            (offset (if (pair? rest) (token-start (car rest))
                      input-end-offset))
            (value ,(fused-production-expression production nullable-index))
            (target ,(goto-expression (production-lhs production))))
       (if target
         (loop (cons target remaining-states)
               (cons value remaining-values) rest
               ,@(if stream? '((fx+ actions 1) shifts) '()))
         (fallback)))))

(def (action-expression entry)
  (case (cadr entry)
    ((shift)
     `(if (pair? rest)
        (loop (cons ,(caddr entry) states)
              (cons (list (make-recognition-child #f (car rest)))
                    semantic-values)
              (cdr rest))
        (fallback)))
    ((reduce) (reduce-expression (caddr entry)))
    ((accept)
     '(let (children
            (and (pair? semantic-values)
                 (recognition-sequence->list (car semantic-values))))
        (if (and (pair? children)
                 (null? (cdr children))
                 (not (recognition-child-field (car children))))
          (values (recognition-child-value (car children)) rest)
          (fallback))))
    (else '(fallback))))

(def (terminal-predicate terminal)
  (case (cadr terminal)
    ((eof) '(null? rest))
    ((literal)
     `(and (pair? rest)
           (string? (token-lexeme (car rest)))
           (string=? (token-lexeme (car rest)) ,(caddr terminal))))
    ((token)
     `(and (pair? rest) (eq? (token-kind (car rest)) ',(caddr terminal))))
    (else (error "unsupported terminal" terminal))))

(def (state-expression state)
  (let* ((row (vector-ref actions state))
         (literals (filter (lambda (entry)
                             (eq? (cadr (car entry)) 'literal)) row))
         (tokens (filter (lambda (entry)
                           (eq? (cadr (car entry)) 'token)) row))
         (eof (filter (lambda (entry)
                        (eq? (cadr (car entry)) 'eof)) row)))
    `(cond
       ,@(map (lambda (entry)
                 `(,(terminal-predicate (car entry))
                   ,(action-expression entry)))
               (append literals eof tokens))
       (else (fallback)))))

(def (stream-action-expression entry)
  (case (cadr entry)
    ((shift)
     `(if (pair? rest)
        (let ((next-states (cons ,(caddr entry) states))
              (next-values
               (cons (list (make-recognition-child #f (car rest)))
                     semantic-values))
              (next-actions (fx+ actions 1))
              (next-shifts (fx+ shifts 1)))
          (after-shift (car rest) next-states next-values
                       next-actions next-shifts)
          (loop next-states next-values (cdr rest)
                next-actions next-shifts))
        (fallback)))
    ((reduce) (reduce-expression (caddr entry) #t))
    ((accept)
     '(let (children
            (and (pair? semantic-values)
                 (recognition-sequence->list (car semantic-values))))
        (if (and (pair? children)
                 (null? (cdr children))
                 (not (recognition-child-field (car children))))
          (values 'accepted
                  (list (recognition-child-value (car children)) rest))
          (fallback))))
    (else '(fallback))))

(def (stream-state-expression state)
  (let* ((row (vector-ref actions state))
         (literals (filter (lambda (entry)
                             (eq? (cadr (car entry)) 'literal)) row))
         (tokens (filter (lambda (entry)
                           (eq? (cadr (car entry)) 'token)) row))
         (eof (filter (lambda (entry)
                        (eq? (cadr (car entry)) 'eof)) row)))
    `(let (rest
           (if (null? rest)
             (let (input-token
                   (next-input (vector-ref modes ,(state-mode-id state))))
               (if input-token
                 (begin
                   (set! input-end-offset
                         (max input-end-offset (token-end input-token)))
                   (list input-token))
                 '()))
             rest))
       (cond
        ,@(map (lambda (entry)
                  `(,(terminal-predicate (car entry))
                    ,(stream-action-expression entry)))
                (append literals eof tokens))
        (else (fallback))))))

(def (stream-drive-expression)
  `(def (direct-drive runtime next-input after-shift)
     (let ((modes (lr-runtime-lexical-mode-catalog runtime))
           (input-end-offset 0))
       (def (fallback) (values 'fallback #f))
       (let loop ((states '(0)) (semantic-values '()) (rest '())
                  (actions 0) (shifts 0))
         (case (car states)
           ,@(let loop ((i 0) (acc '()))
               (if (= i (vector-length actions))
                 (reverse acc)
                 (loop (fx+ i 1)
                       (cons `((,i) ,(stream-state-expression i)) acc))))
           (else (fallback)))))))

(def (module-expression)
  `(begin
     (import (only-in :gerbil-parser/languages/arithmetic/grammar
                      arithmetic-parser)
             (only-in :gerbil-parser/src/compiler/machine
                      parser-machine-runtime)
             (only-in :gerbil-parser/src/runtime/lr-parser
                      lr-parse/prepared lr-runtime-lexical-mode-catalog)
             (only-in :gerbil-parser/src/runtime/recognition
                      make-recognition-child make-recognition-fragment
                      recognition-child-field recognition-child-value)
             (only-in :gerbil-parser/src/runtime/reduce
                      recognition-children-field recognition-children-alias)
             (only-in :gerbil-parser/src/runtime/funcs
                      recognition-sequence->list recognition-sequence-for-action recognition-sequence-append
                      recognition-sequence-start)
             (only-in :gerbil-parser/src/runtime/token
                      token-kind token-lexeme token-start token-end))
     (export direct-parse direct-drive)
     (def (direct-parse tokens (strict? #f))
       (let* ((runtime (parser-machine-runtime arithmetic-parser))
              (input-end-offset
               (fold (lambda (input-token offset)
                       (max offset (token-end input-token)))
                     0 tokens)))
         (def (fallback)
           (if strict?
             (error "generated LR left the deterministic fast path")
             (lr-parse/prepared runtime tokens)))
         (let loop ((states '(0)) (semantic-values '()) (rest tokens))
           (case (car states)
             ,@(let loop ((i 0) (acc '()))
                 (if (= i (vector-length actions))
                   (reverse acc)
                   (loop (+ i 1)
                         (cons `((,i) ,(state-expression i)) acc))))
             (else (fallback))))))
     ,(stream-drive-expression)))

(def (production-module-expression)
  `(begin
     (import (only-in :gerbil-parser/src/runtime/lr-parser
                      lr-runtime-lexical-mode-catalog)
             (only-in :gerbil-parser/src/runtime/recognition
                      make-recognition-child make-recognition-fragment
                      recognition-child-field recognition-child-value)
             (only-in :gerbil-parser/src/runtime/reduce
                      recognition-children-field recognition-children-alias)
             (only-in :gerbil-parser/src/runtime/funcs
                      recognition-sequence->list recognition-sequence-for-action recognition-sequence-append
                      recognition-sequence-start)
             (only-in :gerbil-parser/src/runtime/token
                      token-kind token-lexeme token-start token-end))
     (export direct-drive direct-grammar-digest)
     (def direct-grammar-digest
       ,(parser-machine-grammar-digest arithmetic-parser))
     ,(stream-drive-expression)))

(def (emit-generated port module?)
  (display ";;; Generated from arithmetic Parser IR; regenerate with generate-fused-lr.ss.\n" port)
  (write
   (if module?
     (production-module-expression)
     (module-expression)) port)
  (newline port)
  (unless module?
    (call-with-input-file
     "t/benchmarks/arithmetic-scale/fused-lr-benchmark-body.ss"
     (lambda (source)
       (let loop ((character (read-char source)))
         (unless (eof-object? character)
           (write-char character port)
           (loop (read-char source))))))))

(def (main . args)
  (cond
   ((= (length args) 1)
    (call-with-output-file (car args)
      (lambda (port) (emit-generated port #f))))
   ((and (= (length args) 2) (equal? (car args) "module"))
    (call-with-output-file (cadr args)
      (lambda (port) (emit-generated port #t))))
   ((and (= (length args) 2) (equal? (car args) "check"))
    (let ((expected
           (call-with-output-string
            (lambda (port) (emit-generated port #t))))
          (actual (call-with-input-file (cadr args) read-all-as-string)))
      (unless (string=? expected actual)
        (error "generated arithmetic LR source is stale" (cadr args)))
      (displayln "GENERATED-LR-OK")))
   (else (error "expected [module|check] generated source path" args))))
(export main)

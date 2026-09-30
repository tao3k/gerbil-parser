;;; -*- Gerbil -*-
;;; Experimental fused Scheme LR generator for the arithmetic benchmark.
;;; This consumes the compiled Parser IR; the grammar remains the sole author.
(import (only-in :gerbil-parser/languages/arithmetic/v1/grammar arithmetic-parser-ir)
        (only-in :gerbil-parser/src/compiler/lr
                 lr-spec-ref production-table production-lhs production-rhs
                 production-action operand-actions))

(def spec (cdr (assq 'lr-spec arithmetic-parser-ir)))
(def actions (lr-spec-ref spec 'actions))
(def gotos (lr-spec-ref spec 'gotos))
(def table (production-table (lr-spec-ref spec 'productions)))

(def (nth-tail name count)
  (let loop ((n count) (value name))
    (if (zero? n) value (loop (- n 1) `(cdr ,value)))))

(def (operand-expression value operand)
  (foldl (lambda (action current)
           (case (car action)
             ((field)
              `(recognition-children-field ',(cadr action)
                 (recognition-sequence->list ,current) offset
                 make-recognition-fragment))
             ((alias)
              `(recognition-children-alias ',(cadr action)
                 (recognition-sequence->list ,current) offset))
             (else (error "unsupported operand action" action))))
         value (operand-actions operand)))

(def (semantic-expression production count)
  (let ((rhs (production-rhs production))
        (action (production-action production)))
    (cond
     ((and (eq? action 'pass) (= count 1))
      (operand-expression 'v0 (car rhs)))
     ((memq action '(pass concat))
      (let loop ((operands rhs) (i 0) (combined ''()))
        (if (null? operands)
          combined
          (loop (cdr operands) (+ i 1)
                `(recognition-sequence-append
                  ,combined
                  ,(operand-expression
                    (string->symbol (string-append "v" (number->string i)))
                    (car operands)))))))
     (else (error "unsupported semantic action" action)))))

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

(def (reduce-expression production-id)
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
            (value ,(semantic-expression production count))
            (target ,(goto-expression (production-lhs production))))
       (if target
         (loop (cons target remaining-states)
               (cons value remaining-values) rest)
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

(def (module-expression)
  `(begin
     (import (only-in :gerbil-parser/languages/arithmetic/v1/grammar
                      arithmetic-parser)
             (only-in :gerbil-parser/src/compiler/machine
                      parser-machine-runtime)
             (only-in :gerbil-parser/src/runtime/lr-parser
                      lr-parse/prepared)
             (only-in :gerbil-parser/src/runtime/recognition
                      make-recognition-child make-recognition-fragment
                      recognition-child-field recognition-child-value)
             (only-in :gerbil-parser/src/runtime/reduce
                      recognition-children-field recognition-children-alias)
             (only-in :gerbil-parser/src/runtime/funcs
                      recognition-sequence->list recognition-sequence-append)
             (only-in :gerbil-parser/src/runtime/token
                      token-kind token-lexeme token-start token-end))
     (export direct-parse)
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
             (else (fallback))))))))

(def (main . args)
  (unless (= (length args) 1)
    (error "expected generated benchmark source path" args))
  (let (output-path (car args))
    (call-with-output-file output-path
      (lambda (port)
        (display ";;; Generated from arithmetic Parser IR for bounded AOT experiment.\n" port)
        (write (module-expression) port)
        (newline port)
        (call-with-input-file
         "t/benchmarks/arithmetic-scale/fused-lr-benchmark-body.ss"
         (lambda (source)
           (let loop ((character (read-char source)))
             (unless (eof-object? character)
               (write-char character port)
               (loop (read-char source))))))))))
(export main)

;;; -*- Gerbil -*-
;;; Contextual declarations lower to one closed parser product before runtime.

(import (only-in ./contextual-dispatch
                 compile-contextual-dispatch contextual-dispatch-ref)
        (only-in ./contextual-scanner-ir compile-contextual-scanner)
        (only-in ./parser-ir parser-ir-ref)
        (only-in ../modules/parser/contextual-objects
                 make-contextual-method make-contextual-role
                 make-contextual-scan-rule)
        (only-in ../runtime/contextual-ir make-contextual-parser-ir)
        (only-in ../runtime/identity sha256-text))
(export compile-contextual-parser
        compile-contextual-parser/declaration)

(def (canonical value)
  (call-with-output-string (lambda (port) (write value port))))

(def (unique-symbols? values)
  (and (list? values)
       (andmap symbol? values)
       (let loop ((rest values) (seen '()))
         (or (null? rest)
             (and (not (memq (car rest) seen))
                  (loop (cdr rest) (cons (car rest) seen)))))))

(def (subset? left right)
  (andmap (lambda (value) (member value right)) left))

(def (lookahead? value known)
  (match value
    (['token name]
     (and (symbol? name) (member (list 'terminal 'token name) known)))
    (['literal text]
     (and (string? text) (member (list 'terminal 'literal text) known)))
    (['eof] (member '(terminal eof) known))
    (else #f)))

(def (unique-lookaheads? values known)
  (and (list? values)
       (andmap (lambda (value) (lookahead? value known)) values)
       (let loop ((rest values) (seen '()))
         (or (null? rest)
             (and (not (member (car rest) seen))
                  (loop (cdr rest) (cons (car rest) seen)))))))

(def (valid-position-clause? clause positions terminals)
  (match clause
    ([name required forbidden]
     (and (memq name positions)
          (unique-lookaheads? required terminals)
          (unique-lookaheads? forbidden terminals)
          (not (any (lambda (term) (member term forbidden)) required))))
    (else #f)))

(def (clause-matches? clause expected)
  (and (subset? (cadr clause) expected)
       (not (any (lambda (term) (member term expected))
                 (caddr clause)))))

(def (clause-dominates? left right)
  (and (subset? (cadr right) (cadr left))
       (subset? (caddr right) (caddr left))
       (or (> (length (cadr left)) (length (cadr right)))
           (> (length (caddr left)) (length (caddr right))))))

(def (state-position state expected clauses)
  (let* ((matching (filter (lambda (clause)
                             (clause-matches? clause expected))
                           clauses))
         (maximal
          (filter
           (lambda (clause)
             (not (any (lambda (other)
                         (clause-dominates? other clause))
                       matching)))
           matching)))
    (unless (and (pair? maximal)
                 (andmap (lambda (clause)
                           (eq? (car clause) (caar maximal)))
                         maximal))
      (error "unresolved contextual LR position" state expected maximal))
    (cons state (caar maximal))))

(def (compile-position-table parser-ir positions clauses)
  (let* ((actions (cdr (assq 'actions
                            (parser-ir-ref parser-ir 'lr-spec))))
         (terminals
          (and (vector? actions)
               (apply append (map (lambda (row) (map car row))
                                  (vector->list actions))))))
    (unless (and (vector? actions)
                 (pair? clauses) (list? clauses)
                 (andmap (lambda (clause)
                           (valid-position-clause?
                            clause positions terminals))
                         clauses))
      (error "invalid contextual LR position declaration" clauses))
    (let loop ((index 0) (result '()))
      (if (= index (vector-length actions))
        (reverse result)
        (let (expected
              (map (lambda (row) (cdr (car row)))
                   (vector-ref actions index)))
          (loop (+ index 1)
                (cons (state-position index expected clauses)
                      result)))))))

(def (validate-results dispatch parser-ir)
  (let (terminals (map car (parser-ir-ref parser-ir 'terminals)))
    (for-each
     (lambda (cell)
       (when (list-ref cell 3)
         (unless (memq (car (list-ref cell 3)) terminals)
           (error "contextual result is not a grammar terminal" cell))))
     (contextual-dispatch-ref dispatch 'cells))))

;;; Clauses have the closed shape (position required-lookaheads
;;; forbidden-lookaheads), with (token name), (literal text), and (eof) rows.
;;; A more specific clause must add a requirement or
;;; exclusion; incomparable matches with different positions fail compilation.
(def (compile-contextual-parser parser-ir grammar-digest roles
                                modes positions forms scan-rules
                                initial-mode position-clauses)
  (unless (and (equal? (parser-ir-ref parser-ir 'schema)
                       "gerbil-parser.parser-ir.v1")
               (string? grammar-digest)
               (unique-symbols? modes)
               (unique-symbols? positions)
               (unique-symbols? forms))
    (error "invalid contextual parser declaration"))
  (let* ((dispatch
          (compile-contextual-dispatch roles modes positions forms))
         (_checked (validate-results dispatch parser-ir))
         (scanner
          (compile-contextual-scanner scan-rules dispatch initial-mode
                                      grammar-digest))
         (state-positions
          (compile-position-table parser-ir positions position-clauses))
         (construction (list (cons 'syntax-kinds (parser-ir-ref parser-ir 'syntax-kinds))
                             (cons 'terminals (parser-ir-ref parser-ir 'terminals))
                             (cons 'projections (parser-ir-ref parser-ir 'rules)))))
    (make-contextual-parser-ir
     (list (cons 'dialect 'lr) (cons 'program parser-ir)) scanner
     (list (cons 'state-positions state-positions)) construction
     (parser-ir-ref parser-ir 'root-kind)
     (list (cons 'base-grammar-digest grammar-digest)
           (cons 'parser-ir-digest (sha256-text (canonical parser-ir)))))))

;;; Datum projection used only by deflanguage-grammar expansion. The language
;;; file declares rows; POO objects exist solely while compiling the product.
(def (compile-contextual-parser/declaration
      parser-ir grammar-digest role-rows modes positions forms scan-rows
      initial-mode position-clauses)
  (let ((roles
         (map
          (lambda (row)
            (match row
              ([name methods]
               (make-contextual-role
                name
                (map
                 (lambda (method)
                   (match method
                     ([method-name mode position form result]
                      (make-contextual-method
                       method-name mode position form result))
                     (else (error "invalid contextual method row" method))))
                 methods)))
              (else (error "invalid contextual role row" row))))
          role-rows))
        (rules
         (map
          (lambda (row)
            (match row
              ([name mode form matcher rank action]
               (make-contextual-scan-rule
                name mode form matcher rank action))
              (else (error "invalid contextual scan row" row))))
          scan-rows)))
    (compile-contextual-parser
     parser-ir grammar-digest roles modes positions forms rules
     initial-mode position-clauses)))

;;; -*- Gerbil -*-
;;; Closed contextual scanner rules and a digest bound to compiler dispatch.

(import (only-in ../modules/parser/contextual-objects
                 contextual-scan-rule? contextual-scan-rule-name
                 contextual-scan-rule-mode contextual-scan-rule-form
                 contextual-scan-rule-matcher contextual-scan-rule-rank
                 contextual-scan-rule-action)
        (only-in ./contextual-dispatch contextual-dispatch-ref)
        (only-in ../runtime/contextual-scanner
                 +contextual-scanner-opcode-contract+)
        (only-in ../runtime/identity sha256-text))
(export compile-contextual-scanner contextual-scanner-ir-ref)

(def +contextual-scanner-ir-schema+ "gerbil-parser.contextual-scanner-ir.v1")

(def (contextual-scanner-ir-ref ir key)
  (let (row (assq key ir)) (and row (cdr row))))

(def (nonempty-strings? values)
  (and (pair? values) (list? values)
       (andmap (lambda (value)
                 (and (string? value) (> (string-length value) 0)))
               values)))

(def (unique-by? values project)
  (let loop ((rest values) (seen '()))
    (or (null? rest)
        (let (key (project (car rest)))
          (and (not (member key seen))
               (loop (cdr rest) (cons key seen)))))))

(def (matcher? expression)
  (match expression
    (['literal value]
     (and (string? value) (> (string-length value) 0)))
    (['literals values] (nonempty-strings? values))
    (['horizontal-whitespace+] #t)
    (['newline] #t)
    (['newline-one] #t)
    (['identifier] #t)
    (['marker-line] #t)
    (['body-line] #t)
    (['quoted-string delimiters] (nonempty-strings? delimiters))
    (['balanced-word stops quotes pairs]
     (and (nonempty-strings? stops)
          (nonempty-strings? quotes)
          (andmap (lambda (quote) (= (string-length quote) 1)) quotes)
          (unique-by? quotes (lambda (quote) quote))
          (list? pairs)
          (andmap
           (lambda (pair)
             (match pair
               ([prefix opening closing]
                (and (string? prefix) (> (string-length prefix) 0)
                     (char? opening) (char? closing)
                     (char=? (string-ref prefix
                                         (- (string-length prefix) 1))
                             opening)))
               (else #f)))
           pairs))
          (unique-by? pairs car))
    (else #f)))

(def (action-matcher-compatible? action matcher)
  (let (opcode (car matcher))
    (match action
      ('keep (not (eq? opcode 'marker-line)))
      (['expect-marker _]
       (memq opcode '(literal literals)))
      (['enqueue-if-expecting _]
       (memq opcode '(balanced-word identifier quoted-string)))
      (['activate-next _] (eq? opcode 'newline-one))
      (['finish-marker _ _] (eq? opcode 'marker-line))
      (else #f))))

(def (action? expression modes)
  (match expression
    ('keep #t)
    (['expect-marker strip-tabs?] (boolean? strip-tabs?))
    (['enqueue-if-expecting policy]
     (memq policy '(raw shell-quote-removal)))
    (['activate-next body-mode] (memq body-mode modes))
    (['finish-marker base-mode body-mode]
     (and (memq base-mode modes) (memq body-mode modes)))
    (else #f)))

(def (scan-rule-row rule)
  (list (contextual-scan-rule-name rule)
        (contextual-scan-rule-mode rule)
        (contextual-scan-rule-form rule)
        (contextual-scan-rule-matcher rule)
        (contextual-scan-rule-rank rule)
        (contextual-scan-rule-action rule)))

(def (canonical value)
  (call-with-output-string (lambda (port) (write value port))))

(def (valid-dispatch-digest? dispatch)
  (and (list? dispatch)
       (string? (contextual-dispatch-ref dispatch 'digest))
       (equal?
        (sha256-text
         (canonical
          (filter (lambda (row) (not (eq? (car row) 'digest))) dispatch)))
        (contextual-dispatch-ref dispatch 'digest))))

(def (same-candidate? left right)
  (and (eq? (contextual-scan-rule-mode left)
            (contextual-scan-rule-mode right))
       (equal? (contextual-scan-rule-matcher left)
               (contextual-scan-rule-matcher right))
       (= (contextual-scan-rule-rank left)
          (contextual-scan-rule-rank right))))

(def (validate-deterministic-rules rules)
  (let loop ((remaining rules))
    (when (pair? remaining)
      (for-each
       (lambda (other)
         (when (same-candidate? (car remaining) other)
           (error "duplicate contextual scan candidate"
                  (contextual-scan-rule-name (car remaining))
                  (contextual-scan-rule-name other))))
       (cdr remaining))
      (loop (cdr remaining)))))

(def (index-dispatch-cells cells)
  (let ((index (make-table test: equal?))
        (missing (gensym 'missing-cell)))
    (for-each
     (lambda (cell)
       (let (key (take cell 3))
         (unless (eq? (table-ref index key missing) missing)
           (error "duplicate contextual dispatch cell" key))
         (table-set! index key (list-ref cell 3))))
     cells)
    index))

(def (cell-result index mode position form)
  (table-ref index (list mode position form) #f))

(def (validate-rule rule modes forms)
  (unless (and (contextual-scan-rule? rule)
               (symbol? (contextual-scan-rule-name rule))
               (memq (contextual-scan-rule-mode rule) modes)
               (memq (contextual-scan-rule-form rule) forms)
               (matcher? (contextual-scan-rule-matcher rule))
               (integer? (contextual-scan-rule-rank rule))
               (action? (contextual-scan-rule-action rule) modes)
               (action-matcher-compatible?
                (contextual-scan-rule-action rule)
                (contextual-scan-rule-matcher rule)))
    (error "invalid contextual scan rule" rule)))

;;; The dispatch table resolves token kinds for every scanner rule and parser
;;; position. No POO method or language callback enters the scanner runtime.
(def (compile-contextual-scanner rules dispatch initial-mode
                                 (grammar-digest #f))
  (let ((modes (contextual-dispatch-ref dispatch 'modes))
        (positions (contextual-dispatch-ref dispatch 'positions))
        (forms (contextual-dispatch-ref dispatch 'forms)))
    (unless (and (list? rules) (pair? rules)
                 (memq initial-mode modes)
                 (equal? (contextual-dispatch-ref dispatch 'schema)
                         "gerbil-parser.contextual-dispatch.v1")
                 (valid-dispatch-digest? dispatch)
                 (or (not grammar-digest) (string? grammar-digest)))
      (error "invalid contextual scanner declaration"))
    (def cell-index
      (index-dispatch-cells (contextual-dispatch-ref dispatch 'cells)))
    (for-each (lambda (rule) (validate-rule rule modes forms)) rules)
    (unless (unique-by? rules contextual-scan-rule-name)
      (error "duplicate contextual scan rule identity"))
    (validate-deterministic-rules rules)
    (for-each
     (lambda (mode)
       (unless (any (lambda (rule)
                      (eq? (contextual-scan-rule-mode rule) mode))
                    rules)
         (error "contextual scanner mode has no rules" mode)))
     modes)
    (for-each
     (lambda (rule)
       (for-each
        (lambda (position)
          (unless (cell-result cell-index
                               (contextual-scan-rule-mode rule)
                               position
                               (contextual-scan-rule-form rule))
            (error "scan rule has no contextual result"
                   (contextual-scan-rule-name rule) position)))
        positions))
     rules)
    (let* ((body
            (list (cons 'schema +contextual-scanner-ir-schema+)
                  (cons 'opcode-contract +contextual-scanner-opcode-contract+)
                  (cons 'initial-mode initial-mode)
                  (cons 'dispatch-digest
                        (contextual-dispatch-ref dispatch 'digest))
                  (cons 'grammar-digest grammar-digest)
                  (cons 'modes modes)
                  (cons 'positions positions)
                  (cons 'rules (map scan-rule-row rules))
                  (cons 'cells (contextual-dispatch-ref dispatch 'cells))))
           (digest
            (sha256-text (canonical body))))
      (append body (list (cons 'digest digest))))))

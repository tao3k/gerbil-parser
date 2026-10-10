;;; -*- Gerbil -*-
;;; Generated lexer plans and conservative mode certificates.

(import (only-in ../runtime/funcs
                 vector-intern-map make-value-interner value-interner-intern)
        (only-in :std/vector/vector
                 vector-map/index)
        (only-in ../runtime/lr-parser
                 lr-lexical-mode-id lr-lexical-mode-terminals)
        (only-in ../runtime/scan
                 make-ranked-literal-scanner make-ranked-regular-scanner scan-emit)
        (only-in ../runtime/token
                 token-end))
(import (only-in ./lexical-expression generated-lexical-rule
                 lexical-rule-certificate-template prefer-generated-match))
(export generated-lexer prepare-generated-lexer current-lexical-plan-sharing-enabled?)

(def current-lexical-plan-sharing-enabled? (make-parameter #t))

;;; Quotient conservative first-transition masks, then full admission masks.
;;; A certificate retains every admitted rule that could match the input's
;;; first character. Rule ordinal preserves kind, precedence, and tie order;
;;; equal candidate sets and the common all-rule fallback prove equal scans.
;;; Non-ASCII and unknown lexical expressions keep their conservative path.
(def (prepare-lexical-certificates rules mode-keys all-key metadata)
  (let ((interner (make-value-interner)) (rule-vector (list->vector rules))
        (certificate-vector (list->vector metadata)))
    (def (normalize key mask)
      (let* ((firsts (vector-ref mask 0)) (guarantees (vector-ref mask 1))
             (candidates (vector-map/index
                          (lambda (ordinal admitted) (and admitted (vector-ref firsts ordinal))) key))
             ;; Empty projections fall through to the common all-rule scanner.
             (effective (if (any (lambda (candidate) candidate) (vector->list candidates)) candidates
                          (vector-map/index (lambda (ordinal admitted) (and admitted (vector-ref firsts ordinal))) all-key)))
             (dominator
              (let loop ((ordinal 0) (best #f))
                (if (= ordinal (vector-length effective)) best
                  (let* ((rule (vector-ref rule-vector ordinal))
                         (rank (vector-ref rule 4))
                         (eligible (and (vector-ref effective ordinal)
                                        (vector-ref guarantees ordinal)
                                        (not (vector-ref (vector-ref certificate-vector ordinal) 0)))))
                    (loop (+ ordinal 1)
                      (if (and eligible
                               (or (not best) (> rank (car best))))
                        (cons rank ordinal) best)))))))
        ;; A pure fallback consumes exactly one character. The best guaranteed
        ;; nonfallback rule consumes at least one, and can make it unable to
        ;; win by priority and declaration order. Keep all other competitors.
        (vector-map/index
         (lambda (ordinal admitted)
           (and admitted
                (let (rule (vector-ref rule-vector ordinal))
                  (not (and dominator (vector-ref (vector-ref certificate-vector ordinal) 0)
                            (or (> (car dominator) (vector-ref rule 4))
                                (and (= (car dominator) (vector-ref rule 4))
                                     (< (cdr dominator) ordinal)))))))) effective)))
    (let-values (((classes masks)
                  (vector-intern-map
                   (vector-map/index (lambda (code _) code) (make-vector 128 #f))
                   (lambda (code)
                     (let (ch (integer->char code))
                       (vector
                        (list->vector (map (lambda (rule) ((vector-ref rule 5) ch)) rules))
                        (list->vector (map (lambda (certificate) ((vector-ref certificate 1) ch)) metadata)))))
                   (lambda (key id) (cons id key)))))
      (let-values (((modes ignored)
                    (vector-intern-map mode-keys (lambda (key) key)
                      (lambda (key _id)
                        (vector-map/index
                         (lambda (_index mask)
                           (let (candidate (normalize key (cdr mask)))
                             (value-interner-intern interner candidate (lambda () candidate)))) masks)))))
        (vector (vector-map/index (lambda (_index entry) (car entry)) classes) modes)))))


;;; Private hygienic loop: all inputs are stable bindings at this call site.
;;; Preserve their context without a per-token callback or procedure boundary.
(defrules scan-ranked-candidates ()
  ((_ candidates source offset)
   (let loop ((remaining candidates) (selected #f))
     (if (null? remaining) selected
       (let (candidate ((caar remaining) source offset))
         (loop (cdr remaining)
               (prefer-generated-match selected candidate)))))))

;;; Generates only the source traversal shell; token construction remains scan-owned.
;;; Declaration order is observable only after length and precedence tie.
;; generated-lexer
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       `generated-lexer` expands lexical rows into a source-to-token procedure.
;;
;;       # Examples
;;
;;       ```scheme
;;       (generated-lexer (lexical-rules (Identifier (identifier))))
;;       ;; => source-to-token procedure
;;       ```
;;     %
(defrules generated-lexer
  (lexical-rules extras)
  ((_ (lexical-rules row ...) (extras extra-name ...) case-insensitive?
      mode-catalog)
   (prepare-generated-lexer
    (list (generated-lexical-rule row '(extra-name ...) case-insensitive?) ...)
    (lambda () (list (lexical-rule-certificate-template row) ...))
    mode-catalog)))

;;; Plan construction and scanning are shared native engine code. Language
;;; expansion contributes only rule scanners and conservative certificates.
(def (prepare-generated-lexer bare-rules metadata-thunk mode-catalog)
   (let* ((rules
           (let loop ((remaining bare-rules) (ordinal 0) (found '()))
             (if (null? remaining)
               (reverse found)
               (let* ((rule (car remaining))
                      (scanner (vector-ref rule 1))
                      (ranked-scanner
                       (and scanner
                            (lambda (source offset)
                              (let (match (scanner source offset))
                                (and match
                                     (list (car match) (cadr match)
                                           (caddr match) ordinal)))))))
                 (loop (cdr remaining) (+ ordinal 1)
                       (cons
                        (vector (vector-ref rule 0) ranked-scanner
                                (vector-ref rule 2) (vector-ref rule 3)
                                (vector-ref rule 4) (vector-ref rule 5)
                                (vector-ref rule 6))
                        found))))))
          (literal-entries
           (let loop ((remaining rules) (ordinal 0) (entries '()))
             (if (null? remaining)
               entries
               (let ((rule (car remaining)))
                 (loop
                  (cdr remaining) (+ ordinal 1)
                  (if (vector-ref rule 2)
                    (fold
                     (lambda (literal found)
                       (cons
                        (list literal (vector-ref rule 3)
                              (vector-ref rule 4) ordinal)
                        found))
                     entries (vector-ref rule 2))
                    entries))))))
          (literal-ascii-starts
           (let (starts (make-vector 128 #f))
             (for-each
              (lambda (entry)
                (let* ((literal (car entry))
                       (code (and (positive? (string-length literal))
                                  (char->integer (string-ref literal 0)))))
                  (when (and code (< code 128))
                    (vector-set! starts code #t))))
              literal-entries)
             starts))
          (literal-scanner
           (and (pair? literal-entries)
                (make-ranked-literal-scanner literal-entries)))
          (prepare-scanners
           (lambda (admissions)
             (let ((admitted-literals (make-vector (length rules) #f)))
               (let loop ((remaining rules) (ordinal 0)
                          (has-literals? #f) (scanners '())
                          (regular-entries '())
                          (regular-first-predicates '()))
                 (if (null? remaining)
                   (let* ((regular-scanner
                           (and (pair? regular-entries)
                                (make-ranked-regular-scanner regular-entries)))
                          (regular-ascii-starts
                           (and regular-scanner
                                (vector-map/index
                                 (lambda (index _)
                                   (any (lambda (predicate)
                                          (predicate (integer->char index)))
                                        regular-first-predicates))
                                 (make-vector 128))))
                          (ascii-scanners
                           (vector-map/index
                            (lambda (index _)
                              (filter
                               (lambda (entry)
                                 ((cdr entry) (integer->char index)))
                               scanners))
                            (make-vector 128))))
                     (lambda (source offset)
                       (let* ((ch (string-ref source offset))
                              (code (char->integer ch))
                              (candidates
                               (if (< code 128)
                                 (vector-ref ascii-scanners code)
                                 scanners))
                              (selected
                               (scan-ranked-candidates candidates source offset)))
                         (let (selected
                               (if (and regular-scanner
                                        (or (>= code 128)
                                            (vector-ref regular-ascii-starts
                                                        code)))
                                 (prefer-generated-match
                                  selected (regular-scanner source offset))
                                 selected))
                           (if (and literal-scanner has-literals?
                                    (or (>= code 128)
                                        (vector-ref literal-ascii-starts
                                                    code)))
                             (prefer-generated-match
                              selected
                              (literal-scanner source offset admitted-literals))
                             selected)))))
                   (let* ((rule (car remaining))
                          (admitted? (vector-ref admissions ordinal))
                          (literals (and admitted? (vector-ref rule 2)))
                          (regular-kind (and admitted? (vector-ref rule 6))))
                     (cond
                      ((pair? literals)
                       (vector-set! admitted-literals ordinal #t)
                       (loop
                        (cdr remaining) (+ ordinal 1) #t scanners
                        regular-entries regular-first-predicates))
                      (literals
                       (loop (cdr remaining) (+ ordinal 1)
                             has-literals? scanners regular-entries
                             regular-first-predicates))
                      (regular-kind
                       (loop (cdr remaining) (+ ordinal 1)
                             has-literals? scanners
                             (cons (list regular-kind (vector-ref rule 3)
                                         (vector-ref rule 4) ordinal)
                                   regular-entries)
                             (cons (vector-ref rule 5)
                                   regular-first-predicates)))
                      (admitted?
                       (loop
                        (cdr remaining) (+ ordinal 1) has-literals?
                        (cons (cons (vector-ref rule 1)
                                    (vector-ref rule 5))
                              scanners)
                        regular-entries regular-first-predicates))
                      (else
                       (loop (cdr remaining) (+ ordinal 1)
                             has-literals? scanners regular-entries
                             regular-first-predicates)))))))))
          (admission-key
           (lambda (terminals)
             (list->vector (map (lambda (rule) (and ((vector-ref rule 0) terminals) #t)) rules))))
          (all-scanners (prepare-scanners (admission-key #f)))
          (mode-keys (vector-map/index
                      (lambda (_index mode) (admission-key (lr-lexical-mode-terminals mode))) mode-catalog))
          ;; Normal parsing uses full plans only. Materialize ASCII certificates
          ;; once, on the first request that needs the stronger quotient.
          (certificates (delay (prepare-lexical-certificates rules mode-keys (admission-key #f)
                                 (metadata-thunk))))
          (mode-scanners
           (if (current-lexical-plan-sharing-enabled?)
             (let-values (((plans unique)
                           (vector-intern-map mode-keys
                             (lambda (key) key)
                             (lambda (key _id) (prepare-scanners key))))) plans)
             (vector-map/index
              (lambda (_index key) (prepare-scanners key))
              mode-keys))))
     (letrec
       ((scan-one
         (lambda (source offset byte-offset mode)
           (let (match
                 (or (if mode
                       ((vector-ref mode-scanners
                                    (lr-lexical-mode-id mode))
                        source offset)
                       (all-scanners source offset))
                     ;; A mode miss must still materialize the offending token
                     ;; for the LR failure frontier and lossless diagnostics.
                     ;; Successful directed scans never enter this cold path.
                     (and mode
                          (all-scanners source offset))))
             (unless match
               (error "no lexical rule matched parser-directed source"
                      offset mode))
             (let (output-token
                   (scan-emit source (car match) offset (cadr match)
                              byte-offset))
               (values output-token (cadr match))))))
        (scan-from
         (lambda (source initial-offset initial-byte-offset)
           (let (length (string-length source))
             (let loop ((offset initial-offset)
                        (byte-offset initial-byte-offset)
                        (tokens '()))
               (if (= offset length)
                 (reverse tokens)
                 (let-values (((output-token end)
                               (scan-one source offset byte-offset #f)))
                   (loop end (token-end output-token)
                         (cons output-token tokens)))))))))
     (values (case-lambda
      ((source) (scan-from source 0 0))
      ((source offset byte-offset)
       (scan-from source offset byte-offset))
      ((source offset byte-offset mode)
       (scan-one source offset byte-offset mode))) mode-scanners certificates))))


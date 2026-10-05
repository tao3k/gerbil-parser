#!/usr/bin/env gxi
(import :std/test
        :gerbil-parser/src/language/grammar
        (only-in :gerbil-parser/src/compiler/machine
                 current-lexical-plan-sharing-enabled? parser-machine-prepare-lexer
                 parser-machine-lexical-plans parser-machine-lexical-modes-compatible? parser-machine-runtime)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-runtime-lexical-mode-catalog)
        (only-in :gerbil-parser/src/runtime/incremental
                 current-lr-lexical-plan-reuse-enabled? current-lr-source-index-enabled? make-incremental-session parse-incremental-session
                 incremental-session-artifact incremental-session-project-artifact apply-edit make-edit edit-start-byte)
        (only-in :gerbil-parser/languages/hcl/parser hcl-parser parse-hcl)
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-parser))
(deflanguage lexical-fallback-priority
  (identity "lexical-fallback-priority" "v1" "lexical-fallback-priority.v1")
  (root source-file)
  (lex (first FirstToken (literals "x"))
       (second SecondToken (precedence 9 (fallback)))
       (space Space (whitespace+)))
  (rules (source-file (node SourceFile (seq (field first first) (field second second)))))
  (extras space) (keywords) (recoveries) (conflicts reject) (case-insensitive #f))
(deflanguage lexical-fallback-order
  (identity "lexical-fallback-order" "v1" "lexical-fallback-order.v1")
  (root source-file)
  (lex (second SecondToken (fallback))
       (first FirstToken (literals "x"))
       (space Space (whitespace+)))
  (rules (source-file (node SourceFile (seq (field first first) (field second second)))))
  (extras space) (keywords) (recoveries) (conflicts reject) (case-insensitive #f))

(def (unique-count plans)
  (length (foldl (lambda (plan found) (if (memq plan found) found (cons plan found))) '() (vector->list plans))))
(def (scan lexer text mode)
  (with-catch (lambda (condition) (list 'rejected (error-message condition) (error-irritants condition)))
    (lambda () (call-with-values (lambda () (lexer text 0 0 mode)) list))))
(def lexical-plan-test
  (test-suite "compiler-proved lexical scanner plans"
    (test-case "plan sharing changes construction count but preserves every mode scan"
      (for-each
       (lambda (machine)
         (let-values (((shared plans shared-certificates) (parameterize ((current-lexical-plan-sharing-enabled? #t)) (parser-machine-prepare-lexer machine)))
                      ((separate controls control-certificates) (parameterize ((current-lexical-plan-sharing-enabled? #f)) (parser-machine-prepare-lexer machine))))
           (check (< (unique-count plans) (vector-length plans)) => #t)
           (check (unique-count controls) => (vector-length controls))
           (for-each
            (lambda (mode)
              (for-each (lambda (text) (check (scan shared text mode) => (scan separate text mode)))
                        '("value" "λ中" "001" ".2" "**" "+" "/* λ */" "\n" "\"λ中😀\"" "@")))
            (vector->list (lr-runtime-lexical-mode-catalog (parser-machine-runtime machine))))))
       (list hcl-parser arithmetic-parser)))
    (test-case "equivalent plans preserve token output across distinct LR modes"
      (let* ((plans (parser-machine-lexical-plans hcl-parser))
             (catalog (lr-runtime-lexical-mode-catalog (parser-machine-runtime hcl-parser))))
        (let-values (((lexer ignored certificates) (parser-machine-prepare-lexer hcl-parser)))
          (for-each
           (lambda (i)
             (for-each
              (lambda (j)
                (when (and (not (= i j)) (eq? (vector-ref plans i) (vector-ref plans j)))
                  (check (parser-machine-lexical-modes-compatible? hcl-parser i j) => #t)
                  (check (scan lexer "value" (vector-ref catalog i)) => (scan lexer "value" (vector-ref catalog j)))))
              (iota (vector-length plans))))
           (iota (vector-length plans))))))
    (test-case "first-character certificates preserve competition across distinct modes"
      (for-each
       (lambda (machine)
         (let* ((catalog (lr-runtime-lexical-mode-catalog (parser-machine-runtime machine)))
                (n (vector-length catalog)) (refined 0) (vetoed 0))
           (let-values (((lexer plans certificates) (parser-machine-prepare-lexer machine)))
             (for-each
              (lambda (i)
                (for-each
                 (lambda (j)
                   (for-each
                    (lambda (text)
                      (when (and (eq? machine hcl-parser) (equal? text "\n"))
                        (check (parser-machine-lexical-modes-compatible? machine i j #\newline) => #t))
                      (let ((compatible (parser-machine-lexical-modes-compatible? machine i j (string-ref text 0)))
                            (left (scan lexer text (vector-ref catalog i)))
                            (right (scan lexer text (vector-ref catalog j))))
                        (when compatible
                          ;; Rejection mode objects differ; compare outcome, while
                          ;; successful scans compare complete token and offset.
                          (check (if (eq? (car left) 'rejected) 'rejected left)
                                 => (if (eq? (car right) 'rejected) 'rejected right))
                          (unless (parser-machine-lexical-modes-compatible? machine i j)
                            (set! refined (+ refined 1))))
                        (when (and (not compatible) (not (equal? left right)))
                          (set! vetoed (+ vetoed 1)))))
                    '("\n" " " "x" "value" "001" ".2" "**" "*" "+" "/* λ */" "\"λ中😀\"" "<<EOF\na\nEOF\n" "λ中" "@" "\r\n" "\"" "/*" "." "999e")))
                 (iota n))) (iota n)))
           (check (> refined 0) => #t)
           (unless (eq? machine arithmetic-parser) (check (> vetoed 0) => #t))))
       (list hcl-parser arithmetic-parser
             lexical-fallback-priority-parser lexical-fallback-order-parser)))
    (test-case "Unicode byte shifts preserve certified probes and indexed mode provenance"
      (for-each
       (lambda (indexed?)
         (parameterize ((current-lr-source-index-enabled? indexed?))
           (let* ((source (apply string-append (make-list 80 "value = 001\n")))
                  (session (make-incremental-session hcl-parser source #t)))
             (for-each
              (lambda (edit)
                (let-values (((next receipt)
                              (parameterize ((current-lr-lexical-plan-reuse-enabled? #t))
                                (parse-incremental-session session edit))))
                  (let-values (((control ignored)
                                (parameterize ((current-lr-lexical-plan-reuse-enabled? #f))
                                  (parse-incremental-session session edit))))
                    (check (incremental-session-artifact next) => (incremental-session-artifact control)))
                  (set! source (apply-edit source edit))
                  (check (incremental-session-artifact next) => (parse-hcl source))
                  (check (incremental-session-project-artifact next) => (incremental-session-artifact next))
                  (set! session next)))
              (list (make-edit 0 0 "λ = \"中😀\"\n") (make-edit 0 15 ""))))))
       '(#f #t)))
    (test-case "six-edit token convergence preserves full artifacts and original folds"
      (let* ((line "value = 001\n") (source (apply string-append (make-list 80 line)))
             (session (make-incremental-session hcl-parser source #t)))
        (for-each
         (lambda (edit)
           (let-values (((next receipt) (parameterize ((current-lr-lexical-plan-reuse-enabled? #t))
                                        (parse-incremental-session session edit))))
             (parameterize ((current-lr-lexical-plan-reuse-enabled? #f))
               (let-values (((control strict-receipt) (parse-incremental-session session edit)))
                 (when (and (zero? (edit-start-byte edit))
                            (equal? source (apply string-append (make-list 80 line))))
                   (check (< (cdr (assq 'fragmentCertificateProbeByteCount receipt))
                             (cdr (assq 'fragmentCertificateProbeByteCount strict-receipt))) => #t)
                   (check (> (cdr (assq 'certifiedLexicalProbeTokenCount receipt)) 0) => #t))
                 (check (incremental-session-artifact next) => (incremental-session-artifact control))))
             (set! source (apply-edit source edit))
             (check (incremental-session-artifact next) => (parse-hcl source))
             (check (incremental-session-project-artifact next) => (incremental-session-artifact next))
             (set! session next)))
         (list (make-edit 0 0 "other = 002\n") (make-edit 492 0 "third = 003\n")
               (make-edit 0 12 "") (make-edit 480 12 "")
               (make-edit 960 0 "last = 004\n") (make-edit 960 11 "")))))))
(export lexical-plan-test)

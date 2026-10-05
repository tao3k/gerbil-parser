;;; -*- Gerbil -*-
;;; Closed source generators and descriptor-bound native strategy qualification.
;;; This engine module has no language imports, callbacks or name dispatch.
(import (only-in :std/test check)
        (only-in :clan/poo/object .ref .slot?)
        (only-in ../language/descriptor language-grammar-machine)
        (only-in ../compiler/machine parser-machine-runtime parser-machine-trivia
                 parser-machine-grammar-digest parser-machine-direct-source)
        (only-in ../runtime/lr-parser lr-initial-checkpoint lr-checkpoint-drive lr-runtime-direct-step)
        (only-in ../runtime/lexer lex-source scan-source-token)
        (only-in ../runtime/token token-end)
        (only-in ../runtime/artifact make-success-parse-artifact parse-artifact-valid?
                 parse-artifact-success? parse-artifact-roundtrip parse-artifact-ref)
        (only-in ../../language-support/fixture syntax-fixture-source syntax-fixture-expected-status))
(export check-language-strategies check-language-installed check-language-fixture-strategies
        generate-language-test-sources parse-indexed-reference)

;;; Indexed reference execution selects no generated reduction or source route.
;;; It retains the same ranked scanner, trivia and canonical artifact builder.
(def (parse-indexed-reference machine source (executor #f))
  (let ((character-offset 0) (byte-offset 0) (pending-character #f) (tokens-reversed '()))
    (def (next-input mode)
      (if (= character-offset (string-length source)) #f
        (let-values (((input-token next-character)
                      (scan-source-token machine source character-offset byte-offset mode)))
          (let (start-character character-offset)
            (set! character-offset next-character)
            (set! byte-offset (token-end input-token))
            (if ((parser-machine-trivia machine) input-token)
              (begin (set! tokens-reversed (cons input-token tokens-reversed)) (next-input mode))
              (begin (set! pending-character start-character) input-token))))))
    (def (after-shift input-token _states _values _actions _shifts)
      (set! tokens-reversed (cons input-token tokens-reversed))
      (set! pending-character #f))
    (let-values (((status payload)
                  (lr-checkpoint-drive (lr-initial-checkpoint (parser-machine-runtime machine) '())
                                       next-input after-shift #f executor)))
      (unless (and (eq? status 'accepted) (= character-offset (string-length source))
                   (not pending-character) (null? (cadr payload)))
        (error "indexed language test reference did not complete" status))
      (make-success-parse-artifact (parser-machine-grammar-digest machine) source
                                  (reverse tokens-reversed) (car payload) (parser-machine-trivia machine)))))

;;; Every seeded row starts a fresh exact LCG stream. Nested repetition consumes
;;; draws depth-first, preserving source order and the original case index.
(def (generate-language-test-sources specification)
  (def (generated count seed template)
    (let (state seed)
      (def (draw modulus)
        (set! state (modulo (+ (* state 1103515245) 12345) 2147483648))
        (modulo state modulus))
      (def (render node index environment)
        (cond
         ((string? node) node)
         ((eq? node 'index) (number->string index))
         ((symbol? node) (let (entry (assq node environment))
                          (if entry (cdr entry) (error "unbound generator value" node))))
         (else
          (case (car node)
            ((concat) (apply string-append (map (lambda (part) (render part index environment)) (cdr node))))
            ((choice) (list-ref (cdr node) (draw (length (cdr node)))))
            ((character) (string (string-ref (cadr node) (draw (string-length (cadr node))))))
            ((integer) (number->string (+ (cadr node) (draw (caddr node)))))
            ((if-even) (render (if (even? index) (cadr node) (caddr node)) index environment))
            ((repeat)
             (let loop ((remaining (+ (cadr node) (draw (caddr node)))) (parts '()))
               (if (= remaining 0) (apply string-append (reverse parts))
                 (loop (- remaining 1) (cons (render (cadddr node) index environment) parts)))))
            ((bind)
             (let loop ((rows (cadr node)) (values environment))
               (if (null? rows) (render (caddr node) index values)
                 (let (row (car rows))
                   (loop (cdr rows) (cons (cons (car row) (render (cadr row) index values)) values))))))
            ((when-equal)
             (if (equal? (render (cadr node) index environment) (caddr node))
               (render (cadddr node) index environment) ""))
            (else (error "unknown source generator" node))))))
      (map (lambda (index) (render template index '())) (iota count))))
  (case (car specification)
    ((sources) (cdr specification))
    ((indexed) (list (apply string-append (generated (cadr specification) 0 (caddr specification)))))
    ((seeded) (generated (caddr specification) (cadr specification) (cadddr specification)))
    ((append) (apply append (map generate-language-test-sources (cdr specification))))
    (else (error "unknown language source collection" specification))))

(def (native-test-profile loader)
  (unless (.slot? loader 'native-test-profile)
    (error "language strategy requires a declared generated native test profile"))
  (.ref loader 'native-test-profile))

(def (check-language-installed loader route)
  (let (machine (language-grammar-machine (.ref loader 'descriptor)))
    (check (procedure? (case route
                       ((step) (lr-runtime-direct-step (parser-machine-runtime machine)))
                       ((source) (parser-machine-direct-source machine))
                       (else (error "unknown installed test route" route)))) => #t)))

(def (test-route-artifact loader source route)
  (let (machine (language-grammar-machine (.ref loader 'descriptor)))
    (case route
      ((entry) ((.ref loader '.parse) source))
      ((indexed) (parse-indexed-reference machine source))
      (else
       (let (recognize (.ref (native-test-profile loader) 'source))
         (case route
           ((source-default) (recognize machine source))
           ((source-ranked) (recognize machine source #t #f))
           ((source-raw) (recognize machine source #t #t #f))
           ((source-generic) (recognize machine source #f))
           (else (error "unknown language parser test route" route))))))))

(def (check-strategy-source loader source options)
  (let* ((routes (cdr (assq 'routes options)))
         (lexical (assq 'lexical options)) (accepted (assq 'accepted options))
         (artifacts (map (lambda (route) (test-route-artifact loader source route)) routes))
         (lexical-result #f))
    (when lexical
      (let* ((fast ((.ref (native-test-profile loader) 'lexer) source))
             (expectation (cadr lexical)))
        (set! lexical-result fast)
        (case expectation
          ((fast) (check (and fast #t) => #t))
          ((fallback) (check fast => #f))
          ((optional) (void))
          (else (error "unknown lexical test expectation" expectation)))
        (when fast (check fast => (lex-source (language-grammar-machine (.ref loader 'descriptor)) source)))
        (void)))
    (when (pair? artifacts)
      (for-each (lambda (artifact) (check artifact => (car artifacts))) (cdr artifacts)))
    (when accepted
      (for-each (lambda (artifact)
                  (if (cadr accepted)
                    (begin (check (and artifact #t) => #t)
                           (check (parse-artifact-success? artifact) => #t)
                           (check (parse-artifact-valid? artifact) => #t)
                           (check (parse-artifact-roundtrip artifact) => source))
                    (check artifact => #f))) artifacts))
    (and lexical-result #t)))

(def (check-language-strategies loader specification options)
  (let* ((sources (generate-language-test-sources specification))
         (coverage (assq 'coverage options)) (fast-count 0) (fallback-count 0))
    (unless (and (pair? sources) (andmap string? sources))
      (error "language strategy sources must be nonempty strings"))
    (for-each
     (lambda (source)
       (let (fast? (check-strategy-source loader source options))
         (when coverage
           (if fast? (set! fast-count (+ fast-count 1)) (set! fallback-count (+ fallback-count 1)))))) sources)
    (when coverage
      (check (> fast-count (cadr coverage)) => #t)
      (check (> fallback-count (caddr coverage)) => #t))))

(def (check-language-fixture-strategies loader status options)
  ;; The existing declared service checks all fixture identities and structure.
  ;; Selection below narrows only the native-route parity obligations.
  (let (fixtures (filter (lambda (fixture) (eq? (syntax-fixture-expected-status fixture) status))
                        (.ref loader 'fixtures)))
    (check (pair? fixtures) => #t)
    (for-each
     (lambda (fixture)
       (let ((source (syntax-fixture-source fixture)) (diagnostics (assq 'diagnostics options)))
         (check-strategy-source loader source options)
         (when diagnostics
           (check (length (parse-artifact-ref ((.ref loader '.parse) source) 'diagnostics)) => (cadr diagnostics)))))
     fixtures)))

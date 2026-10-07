;;; -*- Gerbil -*-
;;; Optional corpus, scanner and build services for language development.

(import ./loader-identity
        (only-in ../src/language/entry parse-language-source call-with-language-parser-policy)
        (only-in ./fixture
                 syntax-fixture? syntax-fixture-copy syntax-fixture-source-digest syntax-fixture-id syntax-fixture-source
                 syntax-fixture-language syntax-fixture-version syntax-fixture-contract
                 syntax-fixture-expected-status syntax-fixture-root-kind syntax-fixture-required-kinds)
        (only-in :clan/poo/object .o .ref .cc .slot? object?)
        (only-in :clan/poo/mop define-type validate)
        (only-in :core/types PooFlowContract. poo-flow-classification-evidence)
        (only-in ../src/runtime/identity sha256-text)
        (only-in ../src/runtime/source-scanner source-scanner-for-source?)
        (only-in ../src/compiler/machine parser-machine-grammar-digest parser-machine-direct-source)
        (only-in ../src/compiler/build-strategy BuildStrategyContract emit-build-strategy)
        (only-in ../src/runtime/parser parse-source)
        (only-in ../src/runtime/lr-parser current-lr-branch-budget)
        (only-in ../src/runtime/cst parse-artifact->cst)
        (only-in ../src/runtime/token make-token)
        (only-in ../src/runtime/artifact
                 parse-artifact-success? parse-artifact-valid? parse-artifact-ref
                 parse-artifact-roundtrip parse-artifact-status event-kind
                 parse-artifact-events token-event? token-event-token-kind
                 token-event-lexeme event-start event-end make-failure-parse-artifact)
        (only-in ../src/language/descriptor
                 language-grammar? language-grammar-contract language-grammar-language
                 language-grammar-machine language-grammar-observability
                 language-grammar-version language-grammar-parser-policy
                 language-parser-policy-branch-budget language-parser-policy-cst-validator)
        (only-in ../src/language/source
                 parse-source-language source-language?
                 source-language-contract source-language-language
                 source-language-version source-language-digest source-language-scanner-factory))
(export deflanguage-development-loader LanguageDevelopmentLoader. LanguageDevelopmentLoaderContract
        +language-parser-entry-schema+ language-parser-entry-ref language-metadata-ref parse-language-source
        language-loader-fixtures language-loader-fixture-count language-loader-fixture
        check-language-loader-fixtures! call-with-language-parser-policy
        declare-language-source-scan-worker make-language-scan-worker
        declare-language-fixture-test run-language-test
        declare-language-build-strategy emit-language-build-strategy)

;;; Fixture indexes are an engine-derived cached POO slot. Authors provide only
;;; the fixture value list. A new .cc/.o object recomputes its effective index.
(defstruct fixture-catalog (descriptor origin all accepted rejected by-id counts))
(def (prepare-loader-fixtures descriptor fixtures)
  (unless (list? fixtures) (error "language loader fixtures must be a list"))
  (let ((seen (make-hash-table)) (all '()) (accepted '()) (rejected '()))
    (for-each (lambda (fixture)
      (unless (syntax-fixture? fixture) (error "language loader requires syntax fixtures"))
      (let ((id (syntax-fixture-id fixture)) (source (syntax-fixture-source fixture))
            (status (syntax-fixture-expected-status fixture)) (kinds (syntax-fixture-required-kinds fixture)))
        (unless (and (string? id) (positive? (string-length id)) (not (hash-get seen id))
                     (string? source) (equal? (sha256-text source) (syntax-fixture-source-digest fixture))
                     (equal? (syntax-fixture-language fixture) (descriptor-ref descriptor 'language))
                     (equal? (syntax-fixture-version fixture) (descriptor-ref descriptor 'version))
                     (equal? (syntax-fixture-contract fixture) (descriptor-ref descriptor 'contract))
                     (list? kinds) (every symbol? kinds)
                     (case status
                       ((accepted) (symbol? (syntax-fixture-root-kind fixture)))
                       ((rejected) (and (not (syntax-fixture-root-kind fixture)) (null? kinds)))
                       (else #f)))
          (error "invalid, foreign or duplicate language fixture" id))
        (let (owned (syntax-fixture-copy fixture))
          (hash-put! seen (syntax-fixture-id owned) owned)
          (set! all (cons owned all))
          (if (eq? status 'accepted) (set! accepted (cons owned accepted)) (set! rejected (cons owned rejected)))))) fixtures)
    (make-fixture-catalog descriptor fixtures (reverse all) (reverse accepted) (reverse rejected) seen (vector (length all) (length accepted) (length rejected)))))
(def (loader-fixture-catalog loader)
  (unless (object? loader) (error "fixture query requires a language loader"))
  (let (catalog (.ref loader 'fixture-catalog))
    (unless (and (fixture-catalog? catalog)
                 (eq? (fixture-catalog-descriptor catalog) (.ref loader 'descriptor))
                 (eq? (fixture-catalog-origin catalog) (.ref loader 'fixtures)))
      (error "fixture catalog does not belong to the effective loader"))
    catalog))
(def (catalog-fixtures catalog status)
  (case status
    ((#f) (fixture-catalog-all catalog))
    ((accepted) (fixture-catalog-accepted catalog))
    ((rejected) (fixture-catalog-rejected catalog))
    (else (error "unknown fixture status" status))))
(def (language-loader-fixtures loader (status #f))
  (map syntax-fixture-copy (catalog-fixtures (loader-fixture-catalog loader) status)))
(def (language-loader-fixture-count loader (status #f))
  (let (catalog (loader-fixture-catalog loader))
    (vector-ref (fixture-catalog-counts catalog)
      (case status ((#f) 0) ((accepted) 1) ((rejected) 2) (else (error "unknown fixture status" status))))))
(def (language-loader-fixture loader id)
  (unless (string? id) (error "fixture lookup requires a string id" id))
  (let (fixture (hash-get (fixture-catalog-by-id (loader-fixture-catalog loader)) id))
    (and fixture (syntax-fixture-copy fixture))))

;;; Test declarations carry an engine opcode and exact descriptor identity.
;;; Their constructor is private; a language cannot substitute a success callback.
(defstruct language-fixture-test (descriptor))

(def (declare-language-fixture-test descriptor)
  (unless (or (language-grammar? descriptor) (source-language? descriptor))
    (error "fixture test requires a language descriptor"))
  (make-language-fixture-test descriptor))

(def (declared-test-services? rows descriptor)
  (and (list? rows)
       (let loop ((remaining rows) (seen '()))
         (or (null? remaining)
             (let (row (car remaining))
               (and (pair? row) (symbol? (car row))
                    (not (memq (car row) seen))
                    (language-fixture-test? (cdr row))
                    (eq? descriptor (language-fixture-test-descriptor (cdr row)))
                    (loop (cdr remaining) (cons (car row) seen))))))))

(def (run-language-test loader name)
  (unless (and (object? loader) (symbol? name))
    (error "invalid language test request" name))
  (let* ((descriptor (.ref loader 'descriptor)) (rows (.ref loader 'tests)))
    (unless (declared-test-services? rows descriptor)
      (error "language tests are not declared for this descriptor"))
    (unless (assq name rows) (error "unknown declared language test" name))
    (check-language-loader-fixtures! loader)))

;;; Source workers are derived from the admitted engine binding; no user factory.
(defstruct language-source-scan-worker (descriptor factory))

(def (declare-language-source-scan-worker descriptor)
  (unless (source-language? descriptor)
    (error "source scan worker requires a source language declaration"))
  (let (factory (source-language-scanner-factory descriptor))
    (unless (procedure? factory) (error "source engine has no scanner factory"))
    (make-language-source-scan-worker descriptor factory)))

(def (declared-scan-workers? rows descriptor)
  (and (list? rows)
       (let loop ((remaining rows) (seen '()))
         (or (null? remaining)
             (let (row (car remaining))
               (and (pair? row) (symbol? (car row))
                    (not (memq (car row) seen))
                    (language-source-scan-worker? (cdr row))
                    (eq? descriptor (language-source-scan-worker-descriptor (cdr row)))
                    (loop (cdr remaining) (cons (car row) seen))))))))

(def (make-language-scan-worker loader name source)
  (unless (and (object? loader) (symbol? name) (string? source))
    (error "invalid language scan worker request" name))
  (let* ((descriptor (.ref loader 'descriptor)) (rows (.ref loader 'scan-workers)))
    (unless (declared-scan-workers? rows descriptor)
      (error "language scan workers are not declared for this descriptor"))
    (let (row (assq name rows))
      (unless row (error "unknown declared language scan worker" name))
      (let (worker ((language-source-scan-worker-factory (cdr row)) source))
        (unless (source-scanner-for-source? worker source)
          (error "declared scan worker returned an invalid or foreign source executor" name))
        worker))))

;;; Optional generated-recognizer test profile is a POO value, bound to the
;;; effective Grammar IR machine. It describes existing generated routes;
;;; it cannot replace the Loader's parse method or fixture test service.
(def (loader-native-test-profile? candidate descriptor)
  (or (not (.slot? candidate 'native-test-profile))
      (let (profile (.ref candidate 'native-test-profile))
        (and (language-grammar? descriptor) (object? profile)
             (andmap (lambda (slot) (.slot? profile slot)) '(profile digest source lexer))
             (eq? (.ref profile 'profile) 'recursive-source)
             (let (machine (language-grammar-machine descriptor))
               (and (equal? (.ref profile 'digest) (parser-machine-grammar-digest machine))
                    (procedure? (.ref profile 'source))
                    (eq? (.ref profile 'source) (parser-machine-direct-source machine))
                    (procedure? (.ref profile 'lexer))))))))

(def (declare-language-build-strategy name strategy)
  (unless (symbol? name) (error "build strategy name must be a symbol" name))
  (validate BuildStrategyContract strategy)
  (cons name strategy))

;;; Build strategies are admitted once with the Loader. No build method is
;;; invoked during parser dispatch, and no language-specific route is selected.
(def (declared-build-strategies? rows descriptor)
  (with-catch (lambda (_) #f)
    (lambda ()
      (and (list? rows)
           (let loop ((remaining rows) (seen '()))
             (or (null? remaining)
                 (let (row (car remaining))
                   (and (pair? row) (symbol? (car row)) (not (memq (car row) seen))
                        (begin (validate BuildStrategyContract (cdr row)) #t)
                        (eq? (.ref (cdr row) 'descriptor) descriptor)
                        (loop (cdr remaining) (cons (car row) seen))))))))))

(def (emit-language-build-strategy loader name port)
  (validate LanguageDevelopmentLoaderContract loader)
  (let (row (assq name (.ref loader 'build-strategies)))
    (unless row (error "unknown declared language build strategy" name))
    (emit-build-strategy (cdr row) port)))

(def (loader-shape? candidate)
  (and (object? candidate)
       (andmap (lambda (slot) (.slot? candidate slot))
               '(schema descriptor language version contract capabilities .parse
                 grammars metadata fixtures fixture-catalog tests scan-workers build-strategies))
       (equal? (.ref candidate 'schema) +language-parser-entry-schema+)
       (let (descriptor (.ref candidate 'descriptor))
         (and (or (language-grammar? descriptor) (source-language? descriptor))
              (andmap (lambda (slot)
                        (let (value (descriptor-ref descriptor slot))
                          (and (string? value) (positive? (string-length value))
                               (equal? value (.ref candidate slot)))))
                      '(language version contract))
              (equal? (.ref candidate 'capabilities)
                      (descriptor-capabilities descriptor))
              (procedure? (.ref candidate '.parse))
              (let (grammars (.ref candidate 'grammars))
                (and (list? grammars) (memq descriptor grammars)
                     (every (lambda (grammar)
                              (or (language-grammar? grammar) (source-language? grammar))) grammars)))
              (loader-metadata? (.ref candidate 'metadata) descriptor)
              (loader-native-test-profile? candidate descriptor)
              (with-catch (lambda (_) #f) (lambda () (loader-fixture-catalog candidate) #t))
              (declared-test-services? (.ref candidate 'tests) descriptor)
              (declared-scan-workers? (.ref candidate 'scan-workers) descriptor)
              (declared-build-strategies? (.ref candidate 'build-strategies) descriptor)))))

(define-type (LanguageDevelopmentLoaderContract @ PooFlowContract.)
  identity: 'gerbil-parser/language-loader
  .classify: (lambda (candidate context)
               (let (accepted? (loader-shape? candidate))
                 (poo-flow-classification-evidence
                  'gerbil-parser/language-loader candidate accepted?
                  (if accepted? '() '((expected gerbil-parser/language-loader)))
                  context))))

;;; Slot inheritance is resolved once during loading; parsing uses the bound
;;; engine method without per-call POO contract admission or compiler work.
(def LanguageDevelopmentLoader.
  (.o (:: self)
      schema: +language-parser-entry-schema+
      descriptor: #f
      ;; Language-facing extension slots are inherited POO values. Admission
      ;; checks fixture data without parsing it or executing test/scan workers.
      (grammars (list (.ref self 'descriptor)))
      metadata: (.o)
      fixtures: '()
      (fixture-catalog (prepare-loader-fixtures (.ref self 'descriptor) (.ref self 'fixtures)))
      (tests (list (cons 'fixtures (declare-language-fixture-test (.ref self 'descriptor)))))
      scan-workers: '()
      build-strategies: '()
      (language (descriptor-ref (.ref self 'descriptor) 'language))
      (version (descriptor-ref (.ref self 'descriptor) 'version))
      (contract (descriptor-ref (.ref self 'descriptor) 'contract))
      (capabilities (descriptor-capabilities (.ref self 'descriptor)))
      (.parse (let (descriptor (.ref self 'descriptor))
                (lambda (source) (parse-language-source descriptor source))))))

;;; Public slot extensions are ordinary POO expressions and may inherit from
;;; a user prototype. Engine identity/dispatch slots are sealed by this macro.
;;; Additional contracts validate the effective object after inheritance.
(defsyntax (deflanguage-development-loader stx)
  (syntax-case stx (@ descriptor parse slots contracts grammar source)
    ((_ (binding marker loader-self prototype)
        (descriptor descriptor-value)
        (parse parse-binding)
        (slots slot ...)
        (contracts extension-contract ...))
     (and (identifier? #'binding) (identifier? #'loader-self)
          (eq? (syntax->datum #'marker) '::)
          (identifier? #'parse-binding))
     (let ()
       (def (check-slot! name location)
         (when (memq name '(schema descriptor language version contract
                           capabilities .parse fixture-catalog))
           (raise-syntax-error #f
            "language loader extension overrides an engine slot" location)))
       (let loop ((rows (stx-map (lambda (row) row) #'(slot ...))))
         (unless (null? rows)
           (let* ((row (car rows)) (datum (syntax->datum row)))
             (if (keyword? datum)
               (begin
                 (check-slot! (string->symbol (keyword->string datum)) row)
                 (when (null? (cdr rows))
                   (raise-syntax-error #f "language loader slot requires a value" row))
                 (loop (cddr rows)))
               (begin
                 (syntax-case row ()
                   ((name . _)
                    (begin
                      (unless (identifier? #'name)
                        (raise-syntax-error #f "language loader requires POO slot forms" row))
                      (check-slot! (syntax->datum #'name) row)))
                   (_ (raise-syntax-error #f "language loader requires POO slot forms" row)))
                 (loop (cdr rows)))))))
       #'(begin
           (def binding
             (let* ((author-extensions (.o (:: loader-self prototype) slot ...))
                    (parser-descriptor-value
                     (bind-parser-identity descriptor-value (.ref author-extensions 'metadata)))
                    (candidate
                     (.o (:: loader-self author-extensions)
                       descriptor: parser-descriptor-value
                       ;; Bind these methods at the leaf; a prototype cannot
                       ;; replace identity or dispatch through inherited slots.
                       schema: +language-parser-entry-schema+
                       (language (descriptor-ref (.ref loader-self 'descriptor) 'language))
                       (version (descriptor-ref (.ref loader-self 'descriptor) 'version))
                       (contract (descriptor-ref (.ref loader-self 'descriptor) 'contract))
                       (capabilities (descriptor-capabilities (.ref loader-self 'descriptor)))
                       (.parse (let (descriptor (.ref loader-self 'descriptor))
                                 (lambda (source)
                                   (parse-language-source descriptor source))))
                       (fixture-catalog (prepare-loader-fixtures (.ref loader-self 'descriptor) (.ref loader-self 'fixtures)))
                       )))
               (let* ((metadata (.ref candidate 'metadata))
                      (admitted (.cc candidate 'metadata
                                    (bind-loader-metadata (.ref candidate 'descriptor) metadata))))
                 (validate LanguageDevelopmentLoaderContract admitted)
                 (validate extension-contract admitted) ...
                 admitted)))
           (def parse-binding (.ref binding '.parse)))))
    ((_ binding (kind descriptor-value) (parse parse-binding) section ...)
     (let ()
       (unless (identifier? #'parse-binding)
         (raise-syntax-error #f "language loader parse binding must be an identifier" #'parse-binding))
       (def normalized-binding
         (if (identifier? #'binding)
           #'(binding :: self LanguageDevelopmentLoader.)
           (syntax-case #'binding (@)
             ((name @ prototype)
              (identifier? #'name)
              #'(name :: self prototype))
             ((name marker self prototype)
              (and (identifier? #'name) (identifier? #'self)
                   (eq? (syntax->datum #'marker) '::))
              #'(name :: self prototype))
             (_ (raise-syntax-error #f "invalid language loader binding" #'binding)))))
       (def normalized-descriptor
         (case (syntax->datum #'kind)
           ((descriptor) #'descriptor-value)
           ((grammar) #'(require-grammar-descriptor descriptor-value))
           ((source) #'(require-source-descriptor descriptor-value))
           (else (raise-syntax-error #f "invalid language loader descriptor kind" #'kind))))
       (def slot-section #'(slots))
       (def contract-section #'(contracts))
       (def seen-slots? #f)
       (def seen-contracts? #f)
       (for-each
        (lambda (row)
          (syntax-case row (slots contracts)
            ((slots slot ...)
             (begin
               (when (or seen-slots? seen-contracts?)
                 (raise-syntax-error #f "duplicate or out-of-order language loader slots" row))
               (set! seen-slots? #t) (set! slot-section row)))
            ((contracts contract ...)
             (begin
               (when seen-contracts?
                 (raise-syntax-error #f "duplicate language loader contracts" row))
               (set! seen-contracts? #t) (set! contract-section row)))
            (_ (raise-syntax-error #f "unknown language loader section" row))))
        (stx-map (lambda (row) row) #'(section ...)))
       (with-syntax ((normalized-binding normalized-binding)
                     (normalized-descriptor normalized-descriptor)
                     (slot-section slot-section) (contract-section contract-section))
         #'(deflanguage-development-loader normalized-binding
             (descriptor normalized-descriptor) (parse parse-binding)
             slot-section contract-section))))
    (_ (raise-syntax-error #f "invalid language loader declaration" stx))))

;;; Named test services accept a loader. Fixture values are ordinary POO slots;
;;; the engine verifies statuses, lossless source and required public node kinds.
(def (check-language-loader-fixtures! loader)
  (let (fixtures (fixture-catalog-all (loader-fixture-catalog loader)))
    (map
     (lambda (fixture)
       (let* ((source (syntax-fixture-source fixture))
              (artifact ((.ref loader '.parse) source))
              (kinds (filter-map
                      (lambda (event)
                        (and (eq? (event-kind event) 'start-node) (vector-ref event 2)))
                      (parse-artifact-events artifact))))
         (unless (and (eq? (parse-artifact-status artifact) (syntax-fixture-expected-status fixture))
                      (equal? (parse-artifact-roundtrip artifact) source)
                      (or (not (parse-artifact-success? artifact))
                          (and (pair? kinds) (eq? (car kinds) (syntax-fixture-root-kind fixture))))
                      (every (lambda (kind) (memq kind kinds)) (syntax-fixture-required-kinds fixture)))
           (error "language fixture conformance failed" (syntax-fixture-id fixture)))
         artifact)) fixtures)))

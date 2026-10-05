;;; -*- Gerbil -*-
;;; One POO admission and entry boundary for every versioned language pack.

(import (only-in ../../language-support/fixture
                 syntax-fixture? syntax-fixture-id syntax-fixture-source
                 syntax-fixture-language syntax-fixture-version syntax-fixture-contract
                 syntax-fixture-expected-status syntax-fixture-root-kind syntax-fixture-required-kinds)
        (only-in :clan/poo/object .o .ref .cc .slot? object?)
        (only-in :clan/poo/mop define-type validate)
        (only-in :core/types PooFlowContract. poo-flow-classification-evidence)
        (only-in ../runtime/source-scanner source-scanner-for-source?)
        (only-in ../compiler/machine parser-machine-grammar-digest parser-machine-direct-source)
        (only-in ../runtime/parser parse-source)
        (only-in ../runtime/lr-parser current-lr-branch-budget)
        (only-in ../runtime/cst parse-artifact->cst)
        (only-in ../runtime/token make-token)
        (only-in ../runtime/artifact
                 parse-artifact-success? parse-artifact-valid? parse-artifact-ref
                 parse-artifact-roundtrip parse-artifact-status event-kind
                 parse-artifact-events token-event? token-event-token-kind
                 token-event-lexeme event-start event-end make-failure-parse-artifact)
        (only-in ./descriptor
                 language-grammar? language-grammar-contract language-grammar-language
                 language-grammar-machine language-grammar-observability
                 language-grammar-version language-grammar-parser-policy
                 language-parser-policy-branch-budget language-parser-policy-cst-validator)
        (only-in ./source
                 parse-source-language source-language?
                 source-language-contract source-language-language
                 source-language-version source-language-digest))
(export deflanguage-parser-loader LanguageLoader. LanguageLoaderContract
        +language-parser-entry-schema+ language-parser-entry-ref parse-language-source
        check-language-loader-fixtures! call-with-language-parser-policy
        declare-language-source-scan-worker make-language-scan-worker
        declare-language-fixture-test run-language-test)

(def +language-parser-entry-schema+ "gerbil-parser.language-entry.v2")

(def (language-parser-entry-ref entry key)
  (and (object? entry) (.slot? entry key) (.ref entry key)))

(def (descriptor-capabilities descriptor)
  (cond ((source-language? descriptor) '(scheme-source))
        ((language-grammar-parser-policy descriptor)
         '(grammar-ir parser-ir scheme scheme-policy))
        (else '(grammar-ir parser-ir scheme))))

;;; The engine owns budgeting and lossless rejection assembly. Language policy
;;; callbacks inspect a successful CST and return #f or a diagnostic, never a
;;; replacement parser or artifact. Exceptions and invalid diagnostics escape.
(def (call-with-language-parser-policy descriptor source recognize)
  (let (policy (language-grammar-parser-policy descriptor))
    (if (not policy) (recognize)
      (parameterize ((current-lr-branch-budget (language-parser-policy-branch-budget policy)))
        (let* ((artifact (recognize))
               (diagnostic
                (and (parse-artifact-success? artifact)
                     ((language-parser-policy-cst-validator policy)
                      (parse-artifact->cst artifact)))))
          (if (not diagnostic) artifact
            (let (rejected
                  (make-failure-parse-artifact
                   (parse-artifact-ref artifact 'grammarDigest) source
                   (map (lambda (event)
                          (make-token (token-event-token-kind event)
                                      (token-event-lexeme event)
                                      (event-start event) (event-end event)))
                        (filter token-event? (parse-artifact-events artifact)))
                   diagnostic))
              (unless (parse-artifact-valid? rejected)
                (error "language parser policy returned an invalid diagnostic" diagnostic))
              rejected)))))))

(def (parse-language-source descriptor source)
  (cond
   ((source-language? descriptor) (parse-source-language descriptor source))
   ((language-grammar? descriptor)
    (call-with-language-parser-policy
     descriptor source
     (lambda ()
       (parse-source (language-grammar-machine descriptor) source
                     (language-grammar-observability descriptor)))))
   (else (error "language loader requires a language descriptor" descriptor))))

(def (require-grammar-descriptor descriptor)
  (unless (language-grammar? descriptor)
    (error "grammar loader requires a generated language descriptor" descriptor))
  descriptor)

(def (require-source-descriptor descriptor)
  (unless (source-language? descriptor)
    (error "source loader requires a source language descriptor" descriptor))
  descriptor)

(def (descriptor-ref descriptor field)
  (if (source-language? descriptor)
    (case field
      ((language) (source-language-language descriptor))
      ((version) (source-language-version descriptor))
      ((contract) (source-language-contract descriptor)))
    (case field
      ((language) (language-grammar-language descriptor))
      ((version) (language-grammar-version descriptor))
      ((contract) (language-grammar-contract descriptor)))))

(def (descriptor-digest descriptor)
  (if (source-language? descriptor) (source-language-digest descriptor)
    (parser-machine-grammar-digest (language-grammar-machine descriptor))))

(def (descriptor-digest-kind descriptor)
  (if (source-language? descriptor) 'source-identity 'parser-ir))

;;; Metadata extends ordinary POO values; identity is derived and sealed here.
(def (bind-loader-metadata descriptor inherited)
  (.o (:: self inherited)
      language: (descriptor-ref descriptor 'language)
      version: (descriptor-ref descriptor 'version)
      contract: (descriptor-ref descriptor 'contract)
      digest: (descriptor-digest descriptor)
      digest-kind: (descriptor-digest-kind descriptor)))

(def (loader-metadata? metadata descriptor)
  (and (object? metadata)
       (andmap (lambda (key) (.slot? metadata key))
               '(language version contract digest digest-kind))
       (andmap (lambda (key)
                 (equal? (.ref metadata key) (descriptor-ref descriptor key)))
               '(language version contract))
       (equal? (.ref metadata 'digest) (descriptor-digest descriptor))
       (eq? (.ref metadata 'digest-kind) (descriptor-digest-kind descriptor))))

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

;;; A source worker is an explicit, descriptor-bound transitional declaration.
;;; Ordinary closed-IR scanners are assembled by the compiler, not this bridge.
(defstruct language-source-scan-worker (descriptor factory))

(def (declare-language-source-scan-worker descriptor factory)
  (unless (and (source-language? descriptor) (procedure? factory))
    (error "source scan worker requires a source language declaration"))
  (make-language-source-scan-worker descriptor factory))

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

(def (loader-shape? candidate)
  (and (object? candidate)
       (andmap (lambda (slot) (.slot? candidate slot))
               '(schema descriptor language version contract capabilities .parse
                 grammars metadata fixtures tests scan-workers))
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
              (let (fixtures (.ref candidate 'fixtures))
                (and (list? fixtures) (every syntax-fixture? fixtures)))
              (declared-test-services? (.ref candidate 'tests) descriptor)
              (declared-scan-workers? (.ref candidate 'scan-workers) descriptor)))))

(define-type (LanguageLoaderContract @ PooFlowContract.)
  identity: 'gerbil-parser/language-loader
  .classify: (lambda (candidate context)
               (let (accepted? (loader-shape? candidate))
                 (poo-flow-classification-evidence
                  'gerbil-parser/language-loader candidate accepted?
                  (if accepted? '() '((expected gerbil-parser/language-loader)))
                  context))))

;;; Slot inheritance is resolved once during loading; parsing uses the bound
;;; engine method without per-call POO contract admission or compiler work.
(def LanguageLoader.
  (.o (:: self)
      schema: +language-parser-entry-schema+
      descriptor: #f
      ;; Language-facing extension slots are inherited POO values. Admission
      ;; checks fixture data without parsing it or executing test/scan workers.
      (grammars (list (.ref self 'descriptor)))
      metadata: (.o)
      fixtures: '()
      (tests (list (cons 'fixtures (declare-language-fixture-test (.ref self 'descriptor)))))
      scan-workers: '()
      (language (descriptor-ref (.ref self 'descriptor) 'language))
      (version (descriptor-ref (.ref self 'descriptor) 'version))
      (contract (descriptor-ref (.ref self 'descriptor) 'contract))
      (capabilities (descriptor-capabilities (.ref self 'descriptor)))
      (.parse (let (descriptor (.ref self 'descriptor))
                (lambda (source) (parse-language-source descriptor source))))))

;;; Public slot extensions are ordinary POO expressions and may inherit from
;;; a user prototype. Engine identity/dispatch slots are sealed by this macro.
;;; Additional contracts validate the effective object after inheritance.
(defsyntax (deflanguage-parser-loader stx)
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
                           capabilities .parse))
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
             (let (candidate
                   (.o (:: loader-self prototype)
                       descriptor: descriptor-value
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
                       slot ...))
               (let* ((metadata (.ref candidate 'metadata))
                      (admitted (if (object? metadata)
                                  (.cc candidate 'metadata
                                       (bind-loader-metadata (.ref candidate 'descriptor) metadata))
                                  candidate)))
                 (validate LanguageLoaderContract admitted)
                 (validate extension-contract admitted) ...
                 admitted)))
           (def parse-binding (.ref binding '.parse)))))
    ((_ binding (kind descriptor-value) (parse parse-binding) section ...)
     (let ()
       (unless (identifier? #'parse-binding)
         (raise-syntax-error #f "language loader parse binding must be an identifier" #'parse-binding))
       (def normalized-binding
         (if (identifier? #'binding)
           #'(binding :: self LanguageLoader.)
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
         #'(deflanguage-parser-loader normalized-binding
             (descriptor normalized-descriptor) (parse parse-binding)
             slot-section contract-section))))
    (_ (raise-syntax-error #f "invalid language loader declaration" stx))))

;;; Named test services accept a loader. Fixture values are ordinary POO slots;
;;; the engine verifies statuses, lossless source and required public node kinds.
(def (check-language-loader-fixtures! loader)
  (let (fixtures (.ref loader 'fixtures))
    (unless (and (list? fixtures) (every syntax-fixture? fixtures))
      (error "language loader fixtures must be a list of syntax fixtures"))
    (let ((seen (make-table test: equal?))
          (language (.ref loader 'language))
          (version (.ref loader 'version))
          (contract (.ref loader 'contract)))
      (for-each
       (lambda (fixture)
         (let (id (syntax-fixture-id fixture))
           (unless (and (string? id) (not (table-ref seen id #f))
                        (equal? (syntax-fixture-language fixture) language)
                        (equal? (syntax-fixture-version fixture) version)
                        (equal? (syntax-fixture-contract fixture) contract))
             (error "language fixture identity does not match loader or is duplicated" id))
           (table-set! seen id #t))) fixtures))
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

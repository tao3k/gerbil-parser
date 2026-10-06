;;; -*- Gerbil -*-
;;; Common parsing entry without corpus, test or build dependencies.

(import ./loader-identity
        (only-in ../src/language/entry parse-language-source call-with-language-parser-policy)
        (only-in :clan/poo/object .o .ref .cc .slot? object?)
        (only-in :clan/poo/mop define-type validate)
        (only-in :core/types PooFlowContract. poo-flow-classification-evidence)
        (only-in ../src/compiler/machine parser-machine-grammar-digest)
        (only-in ../src/language/descriptor
                 language-grammar? language-grammar-contract language-grammar-language
                 language-grammar-machine language-grammar-observability
                 language-grammar-version language-grammar-parser-policy
                 language-parser-policy-branch-budget language-parser-policy-cst-validator)
        (only-in ../src/language/source
                 parse-source-language source-language?
                 source-language-contract source-language-language
                 source-language-version source-language-digest source-language-scanner-factory))
(export deflanguage-parser-loader LanguageLoader. LanguageLoaderContract
        +language-parser-entry-schema+ language-parser-entry-ref parse-language-source)

(def (loader-shape? candidate)
  (and (object? candidate)
       (andmap (lambda (slot) (.slot? candidate slot))
               '(schema descriptor language version contract capabilities .parse
                 grammars metadata))
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
))))

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
      (grammars (list (.ref self 'descriptor)))
      metadata: (.o)
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

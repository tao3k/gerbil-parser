;;; -*- Gerbil -*-
;;; One POO admission and entry boundary for every versioned language pack.

(import (only-in :clan/poo/object .o .ref .slot? object?)
        (only-in :clan/poo/mop define-type validate)
        (only-in :core/types PooFlowContract. poo-flow-classification-evidence)
        (only-in ../runtime/parser parse-source)
        (only-in ./descriptor
                 language-grammar? language-grammar-contract language-grammar-language
                 language-grammar-machine language-grammar-observability
                 language-grammar-version)
        (only-in ./source
                 parse-source-language source-language?
                 source-language-contract source-language-language
                 source-language-version))
(export deflanguage-loader deflanguage-parser LanguageLoader. LanguageLoaderContract
        +language-parser-entry-schema+ language-parser-entry-ref parse-language-source)

(def +language-parser-entry-schema+ "gerbil-parser.language-entry.v1")

(def (language-parser-entry-ref entry key)
  (and (object? entry) (.slot? entry key) (.ref entry key)))

(def (parse-language-source descriptor source)
  (cond
   ((source-language? descriptor) (parse-source-language descriptor source))
   ((language-grammar? descriptor)
    (parse-source (language-grammar-machine descriptor) source
                  (language-grammar-observability descriptor)))
   (else (error "language loader requires a language descriptor" descriptor))))

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

(def (loader-shape? candidate)
  (and (object? candidate)
       (andmap (lambda (slot) (.slot? candidate slot))
               '(schema descriptor language version contract capabilities .parse))
       (equal? (.ref candidate 'schema) +language-parser-entry-schema+)
       (let (descriptor (.ref candidate 'descriptor))
         (and (or (language-grammar? descriptor) (source-language? descriptor))
              (andmap (lambda (slot)
                        (let (value (descriptor-ref descriptor slot))
                          (and (string? value) (positive? (string-length value))
                               (equal? value (.ref candidate slot)))))
                      '(language version contract))
              (equal? (.ref candidate 'capabilities)
                      (if (source-language? descriptor)
                        '(scheme-source)
                        '(grammar-ir parser-ir scheme)))
              (procedure? (.ref candidate '.parse))))))

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
      (language (descriptor-ref (.ref self 'descriptor) 'language))
      (version (descriptor-ref (.ref self 'descriptor) 'version))
      (contract (descriptor-ref (.ref self 'descriptor) 'contract))
      (capabilities (if (source-language? (.ref self 'descriptor))
                      '(scheme-source) '(grammar-ir parser-ir scheme)))
      (.parse (let (descriptor (.ref self 'descriptor))
                (lambda (source) (parse-language-source descriptor source))))))

;;; Public slot extensions are ordinary POO expressions and may inherit from
;;; a user prototype. Engine identity/dispatch slots are sealed by this macro.
;;; Additional contracts validate the effective object after inheritance.
(defsyntax (deflanguage-loader stx)
  (syntax-case stx (@ descriptor parse slots contracts grammar source)
    ((_ (binding :: loader-self prototype)
        (descriptor descriptor-value)
        (parse parse-binding)
        (slots slot ...)
        (contracts extension-contract ...))
     (and (identifier? #'binding) (identifier? #'loader-self)
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
                       (capabilities (if (source-language? (.ref loader-self 'descriptor))
                                       '(scheme-source) '(grammar-ir parser-ir scheme)))
                       (.parse (let (descriptor (.ref loader-self 'descriptor))
                                 (lambda (source)
                                   (parse-language-source descriptor source))))
                       slot ...))
               (validate LanguageLoaderContract candidate)
               (validate extension-contract candidate) ...
               candidate))
           (def parse-binding (.ref binding '.parse)))))
    ((_ (binding @ prototype) (descriptor descriptor-value)
        (parse parse-binding) (slots slot ...) (contracts extension-contract ...))
     #'(deflanguage-loader (binding :: self prototype)
         (descriptor descriptor-value) (parse parse-binding)
         (slots slot ...) (contracts extension-contract ...)))
    ((_ (binding @ prototype) (descriptor descriptor-value)
        (parse parse-binding) (slots slot ...))
     #'(deflanguage-loader (binding @ prototype)
         (descriptor descriptor-value) (parse parse-binding)
         (slots slot ...) (contracts)))
    ((_ (binding @ prototype) (descriptor descriptor-value) (parse parse-binding))
     #'(deflanguage-loader (binding @ prototype)
         (descriptor descriptor-value) (parse parse-binding) (slots) (contracts)))
    ((_ binding (descriptor descriptor-value) (parse parse-binding))
     #'(deflanguage-loader (binding @ LanguageLoader.)
         (descriptor descriptor-value) (parse parse-binding) (slots) (contracts)))
    ((_ binding (grammar descriptor-value) (parse parse-binding))
     #'(deflanguage-loader binding (descriptor descriptor-value) (parse parse-binding)))
    ((_ binding (source descriptor-value) (parse parse-binding))
     #'(deflanguage-loader binding (descriptor descriptor-value) (parse parse-binding)))
    (_ (raise-syntax-error #f "invalid language loader declaration" stx))))

;;; Existing entries delegate to the same loader, never a second assembly path.
(defrules deflanguage-parser ()
  ((_ argument ...) (deflanguage-loader argument ...)))

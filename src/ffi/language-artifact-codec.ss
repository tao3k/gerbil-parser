;;; -*- Gerbil -*-
;;; Parser-owned C ABI for language-selected ParseArtifact v1 surfaces.

(import (only-in ../runtime/parse-cost with-parser-cost-stage)
        (only-in ./schema +event-abi-version+ +artifact-abi-version+ +abi-descriptor-schema+ +abi-error-schema+)
        (only-in ./language-abi-context prepare-language-abi-context language-abi-context?
                 descriptor-section abi-terminal-rows language-abi-context-id
                 language-abi-context-descriptor language-abi-context-parser language-abi-context-field-symbols
                 language-abi-context-plan language-abi-context-plan-set!)
        (only-in ./event-binary +binary-header-size+ +binary-event-size+
                 copy-digest! write-event! abi-event-payload)
        (only-in :std/crypto/digest sha256)
        (only-in :std/vector/u8vector little u8vector-u32-set!)
        (only-in ./datum-codec abi-datum-write)
        (only-in ../language/descriptor language-grammar-ir language-grammar? language-grammar-grammar language-grammar-language language-grammar-machine
                 language-grammar-parser-policy language-parser-policy-identity
                 language-parser-policy-branch-budget)
        (only-in ../language/source source-language-digest source-language-root-kind source-language? source-language-language
                 parse-source-language/session source-language-session-artifact source-language-session-history)
        (only-in ../runtime/artifact parse-artifact-events parse-artifact-ref with-parse-event-walk)
        (only-in ../language/entry parse-language-source call-with-language-parser-policy)
        (only-in ../compiler/machine parser-machine-grammar-digest)
        (only-in ../runtime/parser contextual-parser-grammar-digest prepare-contextual-parser parse-source/contextual/prepared
                 parse-source/contextual/prepared/deferred))
(export prepare-language-abi-context
        bind-language-abi
        language-abi-context?
        artifact-abi-version
        abi-descriptor-payload
        abi-parse-binary-payload
        abi-parse-binary-payload/bytes
        abi-parse-binary-payload/bytes/session
        abi-error-payload)


(def (abi-error-payload exception)
  (abi-datum-write
   (hash (schema +abi-error-schema+)
         (message
          (call-with-output-string
           (lambda (port) (display-exception exception port)))))))

(def (syntax-kind->datum row)
  (vector (symbol->string (car row))
          (symbol->string (cadr row))
          (list->vector (map symbol->string (caddr row)))))

(def (terminal->datum row)
  (vector (symbol->string (car row))
          (symbol->string (cadr row))))

(def (artifact-abi-version)
  +artifact-abi-version+)

;; One bounded buffer crosses the C ABI. Token lexemes remain zero-copy source
;; slices, represented by their byte ranges instead of duplicated strings.
(def (abi-parse-binary-payload language source)
  (abi-artifact-binary-payload language ((language-abi-context-parser language) source)))
(def (abi-artifact-binary-payload language artifact)
  (let* ((events (parse-artifact-events artifact))
         (payload (make-u8vector
                   (+ +binary-header-size+
                      (* (length events) +binary-event-size+))
                   0)))
    (subu8vector-move! #u8(71 80 65 49) 0 4 payload 0) ; GPA1
    (u8vector-u32-set! payload 4 +event-abi-version+ little)
    (u8vector-u32-set! payload 8
                       (if (eq? (parse-artifact-ref artifact 'status)
                                'accepted)
                         0 1)
                       little)
    (u8vector-u32-set! payload 12 (length events) little)
    (copy-digest! payload 16 (parse-artifact-ref artifact 'grammarDigest))
    (copy-digest! payload 48 (parse-artifact-ref artifact 'sourceDigest))
    (for-each (lambda (event row) (write-event! language payload row event))
              events
              (iota (length events)))
    payload))

(def (abi-source-text bytes)
  (let (source (utf8->string bytes))
    ;; Preserve strict admission even on decoders that replace invalid sequences.
    (unless (equal? bytes (string->utf8 source))
      (error "native source is not canonical UTF-8"))
    source))

;;; Publish before returning history: any transport/admission exception leaves
;;; the handle's previous history untouched. Syntax rejection remains GPA1.
(def (abi-parse-binary-payload/bytes/session language bytes previous)
  (let (descriptor (language-abi-context-descriptor language))
    (if (source-language? descriptor)
      (let* ((session (parse-source-language/session descriptor (with-parser-cost-stage 'utf8-decode (abi-source-text bytes)) previous))
             (payload (with-parser-cost-stage 'payload-encoding
                        (abi-artifact-binary-payload language (source-language-session-artifact session)))))
        (values payload (source-language-session-history session)))
      (values (abi-parse-binary-payload/bytes language bytes) #f))))

(def (abi-parse-binary-payload/bytes language bytes)
  (let (source (abi-source-text bytes))
    (let (plan (language-abi-context-plan language))
      (if (or (not plan)
              (language-grammar-parser-policy (language-abi-context-descriptor language)))
        (abi-parse-binary-payload language source)
        (let ((source-byte-length (u8vector-length bytes)) (source-digest (sha256 bytes)))
          (parse-source/contextual/prepared/deferred
           plan source source-byte-length
           (lambda (digest _source tokens root trivia?)
             (abi-event-payload
              language digest source-digest
              (lambda (node-emitter field-emitter token-emitter)
                (with-parse-event-walk tokens root trivia? source-byte-length
                                      node-emitter field-emitter token-emitter))))
           (lambda (_machine digest _source tokens _condition)
             (let (publish
                   (abi-event-payload
                    language digest source-digest
                    (lambda (_node-emitter _field-emitter token-emitter)
                      (let loop ((remaining tokens) (id 0))
                        (when (pair? remaining)
                          (token-emitter id (car remaining))
                          (loop (cdr remaining) (+ id 1)))))))
               (lambda ()
                 (let (payload (publish))
                   (u8vector-u32-set! payload 8 1 little)
                   payload))))))))))

(def (abi-descriptor-payload language)
  (let (payload
        (hash (schema +abi-descriptor-schema+)
         (language (language-abi-context-id language))
         (rootKind (symbol->string
                     (if (source-language? (language-abi-context-descriptor language))
                       (source-language-root-kind (language-abi-context-descriptor language))
                       (cdr (assq 'root-kind (language-grammar-ir (language-abi-context-descriptor language)))))))
         (grammarDigest
          (let ((descriptor (language-abi-context-descriptor language))
                (plan (language-abi-context-plan language)))
            (cond (plan (contextual-parser-grammar-digest plan))
                  ((source-language? descriptor) (source-language-digest descriptor))
                  (else (parser-machine-grammar-digest (language-grammar-machine descriptor))))))
         (fields (list->vector
                  (map symbol->string
                       (language-abi-context-field-symbols language))))
         (syntaxKinds
          (list->vector (map syntax-kind->datum
                             (descriptor-section
                              (language-abi-context-descriptor language)
                              'syntax-kinds))))
         (terminals
          (list->vector (map terminal->datum
                             (abi-terminal-rows (language-abi-context-descriptor language)))))))
    (let* ((descriptor (language-abi-context-descriptor language))
           (policy (and (language-grammar? descriptor) (language-grammar-parser-policy descriptor))))
      (when policy
        (hash-put! payload 'parserPolicy
                   (hash (identity (language-parser-policy-identity policy))
                         (branchBudget (language-parser-policy-branch-budget policy))
                         (schemeCstValidation #t) (portable #f)))))
    (abi-datum-write payload)))

;;; A language pack supplies its descriptor once; no builtin language imports.
(def (bind-language-abi descriptor (contextual-product #f))
  (unless (or (language-grammar? descriptor) (source-language? descriptor))
    (error "native language requires an admitted descriptor"))
  (when (and contextual-product (source-language? descriptor))
    (error "source language does not admit a contextual parser product"))
  (let* ((plan (and contextual-product
                    (prepare-contextual-parser (language-grammar-machine descriptor) contextual-product)))
         (language
          (prepare-language-abi-context
           (if (source-language? descriptor) (source-language-language descriptor)
               (language-grammar-language descriptor)) descriptor
           (lambda (source)
             (if plan
               (call-with-language-parser-policy
                descriptor source (lambda () (parse-source/contextual/prepared plan source)))
               (parse-language-source descriptor source))))))
    (language-abi-context-plan-set! language plan)
    language))

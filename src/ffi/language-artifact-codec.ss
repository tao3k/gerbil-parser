;;; -*- Gerbil -*-
;;; Parser-owned C ABI for language-selected ParseArtifact v1 surfaces.

(import (only-in ./native-language-context make-native-language-context native-language?
                 descriptor-section native-terminal-rows native-language-id
                 native-language-descriptor native-language-parser native-language-field-symbols
                 native-language-plan native-language-plan-set!)
        (only-in ./native-event-binary +binary-header-size+ +binary-event-size+
                 copy-digest! write-event! native-event-payload)
        (only-in :std/crypto/digest sha256)
        (only-in :std/vector/u8vector little u8vector-u32-set!)
        (only-in ./native-datum native-datum-write)
        (only-in ../language/descriptor language-grammar-ir language-grammar? language-grammar-grammar language-grammar-language language-grammar-machine
                 language-grammar-parser-policy language-parser-policy-identity
                 language-parser-policy-branch-budget)
        (only-in ../language/source source-language-digest source-language-root-kind source-language? source-language-language)
        (only-in ../runtime/artifact parse-artifact-events parse-artifact-ref with-parse-event-walk)
        (only-in ../language/entry parse-language-source call-with-language-parser-policy)
        (only-in ../compiler/machine parser-machine-grammar-digest)
        (only-in ../runtime/parser contextual-parser-grammar-digest prepare-contextual-parser parse-source/contextual/prepared
                 parse-source/contextual/prepared/deferred))
(export make-native-language-context
        bind-native-language
        native-language?
        native-abi-version
        native-descriptor-payload
        native-parse-binary-payload
        native-parse-binary-payload/bytes
        native-error-payload)

(def +gerbil-parser-native-abi-version+ 1)
(def +gerbil-parser-native-descriptor-schema+
  "gerbil-parser.native-descriptor.v1")
(def +gerbil-parser-native-error-schema+
  "gerbil-parser.native-error.v1")

(def (native-error-payload exception)
  (native-datum-write
   (hash (schema +gerbil-parser-native-error-schema+)
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

(def (native-abi-version)
  +gerbil-parser-native-abi-version+)

;; One bounded buffer crosses the C ABI. Token lexemes remain zero-copy source
;; slices, represented by their byte ranges instead of duplicated strings.
(def (native-parse-binary-payload language-id source)
  (let* ((language language-id)
         (artifact ((native-language-parser language) source))
         (events (parse-artifact-events artifact))
         (payload (make-u8vector
                   (+ +binary-header-size+
                      (* (length events) +binary-event-size+))
                   0)))
    (subu8vector-move! #u8(71 80 65 49) 0 4 payload 0) ; GPA1
    (u8vector-u32-set! payload 4 1 little)
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

(def (native-parse-binary-payload/bytes language bytes)
  (let (source (utf8->string bytes))
    ;; Preserve strict admission even on decoders that replace invalid sequences.
    (unless (equal? bytes (string->utf8 source))
      (error "native source is not canonical UTF-8"))
    (let (plan (native-language-plan language))
      (if (or (not plan)
              (language-grammar-parser-policy (native-language-descriptor language)))
        (native-parse-binary-payload language source)
        (let ((source-byte-length (u8vector-length bytes)) (source-digest (sha256 bytes)))
          (parse-source/contextual/prepared/deferred
           plan source source-byte-length
           (lambda (digest _source tokens root trivia?)
             (native-event-payload
              language digest source-digest
              (lambda (node-emitter field-emitter token-emitter)
                (with-parse-event-walk tokens root trivia? source-byte-length
                                      node-emitter field-emitter token-emitter))))
           (lambda (_machine digest _source tokens _condition)
             (let (publish
                   (native-event-payload
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

(def (native-descriptor-payload language)
  (let (payload
        (hash (schema +gerbil-parser-native-descriptor-schema+)
         (language (native-language-id language))
         (rootKind (symbol->string
                     (if (source-language? (native-language-descriptor language))
                       (source-language-root-kind (native-language-descriptor language))
                       (cdr (assq 'root-kind (language-grammar-ir (native-language-descriptor language)))))))
         (grammarDigest
          (let ((descriptor (native-language-descriptor language))
                (plan (native-language-plan language)))
            (cond (plan (contextual-parser-grammar-digest plan))
                  ((source-language? descriptor) (source-language-digest descriptor))
                  (else (parser-machine-grammar-digest (language-grammar-machine descriptor))))))
         (fields (list->vector
                  (map symbol->string
                       (native-language-field-symbols language))))
         (syntaxKinds
          (list->vector (map syntax-kind->datum
                             (descriptor-section
                              (native-language-descriptor language)
                              'syntax-kinds))))
         (terminals
          (list->vector (map terminal->datum
                             (native-terminal-rows (native-language-descriptor language)))))))
    (let* ((descriptor (native-language-descriptor language))
           (policy (and (language-grammar? descriptor) (language-grammar-parser-policy descriptor))))
      (when policy
        (hash-put! payload 'parserPolicy
                   (hash (identity (language-parser-policy-identity policy))
                         (branchBudget (language-parser-policy-branch-budget policy))
                         (schemeCstValidation #t) (portable #f)))))
    (native-datum-write payload)))

;;; A language pack supplies its descriptor once; no builtin language imports.
(def (bind-native-language descriptor (contextual-product #f))
  (unless (or (language-grammar? descriptor) (source-language? descriptor))
    (error "native language requires an admitted descriptor"))
  (when (and contextual-product (source-language? descriptor))
    (error "source language does not admit a contextual parser product"))
  (let* ((plan (and contextual-product
                    (prepare-contextual-parser (language-grammar-machine descriptor) contextual-product)))
         (language
          (make-native-language-context
           (if (source-language? descriptor) (source-language-language descriptor)
               (language-grammar-language descriptor)) descriptor
           (lambda (source)
             (if plan
               (call-with-language-parser-policy
                descriptor source (lambda () (parse-source/contextual/prepared plan source)))
               (parse-language-source descriptor source))))))
    (native-language-plan-set! language plan)
    language))

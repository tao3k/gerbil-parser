;;; -*- Gerbil -*-
;;; Parser-owned C ABI for language-selected ParseArtifact v1 surfaces.

(import (only-in :std/crypto/digest sha256)
        (only-in ../runtime/token token-kind token-start token-end)
        (only-in :std/vector/u8vector little u8vector-u32-set! u8vector-u64-set!)
        (only-in :std/list/list delete-duplicates/hash)
        (only-in ./native-datum native-datum-write)
        (only-in :std/encoding/hex hex-decode)
        (only-in ../grammar/algebra grammar-expression-fields)
        (only-in ../language/descriptor language-grammar-ir language-grammar? language-grammar-grammar language-grammar-language language-grammar-machine
                 language-grammar-parser-policy language-parser-policy-identity
                 language-parser-policy-branch-budget)
        (only-in ../language/source source-language-root-kind source-language? source-language-language source-language-result-catalog)
        (only-in ../runtime/artifact parse-artifact-events parse-artifact-ref with-parse-event-walk)
        (only-in ../language/entry parse-language-source call-with-language-parser-policy)
        (only-in ../runtime/parser prepare-contextual-parser parse-source/contextual/prepared
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

(defstruct native-language
  (id descriptor parser syntax-kind-index terminal-index field-symbols field-index plan)
  transparent: #t)

(def (descriptor-section descriptor name)
  (if (and (source-language? descriptor) (eq? name 'rules)) '()
    (let (row (assq name (if (source-language? descriptor)
                          (source-language-result-catalog descriptor)
                          (language-grammar-grammar descriptor))))
      (unless row (error "native descriptor lacks a required result section" name))
      (cdr row))))

(def (syntax-kind->datum row)
  (vector (symbol->string (car row))
          (symbol->string (cadr row))
          (list->vector (map symbol->string (caddr row)))))

(def (terminal->datum row)
  (vector (symbol->string (car row))
          (symbol->string (cadr row))))

(def (native-abi-version)
  +gerbil-parser-native-abi-version+)

(def +binary-header-size+ 80)
(def +binary-event-size+ 24)

(def +event-tags+
  (hash (start-node 1)
        (finish-node 2)
        (start-field 3)
        (finish-field 4)
        (token 5)))

(def (indexed-symbols symbols)
  (let (table (make-hash-table-eq size: (length symbols)))
    (for-each (lambda (symbol index) (hash-put! table symbol index))
              symbols
              (iota (length symbols)))
    table))

;; A shared field has one stable id. The grammar expression algebra is the
;; authority for fields emitted by the parser; declared syntax fields are
;; retained first for the descriptor's complete public surface.
(def (descriptor-field-symbols grammar)
  (delete-duplicates/hash
   (append
    (apply append (map caddr (descriptor-section grammar 'syntax-kinds)))
    (apply append
           (map (lambda (row) (grammar-expression-fields (cadr row)))
                (descriptor-section grammar 'rules))))
   from-end?: #t))

(def (make-native-language-context id grammar parser)
  (let (field-symbols (descriptor-field-symbols grammar))
    (make-native-language
     id grammar parser
     (indexed-symbols (map car (descriptor-section grammar 'syntax-kinds)))
     (indexed-symbols (map car (descriptor-section grammar 'terminals)))
     field-symbols
     (indexed-symbols field-symbols) #f)))

(def (required-index table symbol domain)
  (or (hash-get table symbol)
      (error "native ParseArtifact symbol is outside descriptor" domain symbol)))

(def (copy-digest! payload offset digest)
  (let (bytes (hex-decode digest 7))
    (unless (= (u8vector-length bytes) 32)
      (error "invalid ParseArtifact digest" digest))
    (subu8vector-move! bytes 0 32 payload offset)))

(def (write-event! language payload row event)
  (let* ((offset (+ +binary-header-size+ (* row +binary-event-size+)))
         (tag (vector-ref event 0)))
    (u8vector-u32-set! payload offset
                       (required-index +event-tags+ tag 'event-tag) little)
    (case tag
      ((start-node finish-node)
       (u8vector-u32-set!
        payload (+ offset 4)
        (required-index (native-language-syntax-kind-index language)
                        (vector-ref event 2) 'syntax-kind)
        little)
       (u8vector-u64-set! payload (+ offset 8) (vector-ref event 1) little)
       (let (position (vector-ref event 3))
         (u8vector-u32-set! payload (+ offset 16)
                            (if (eq? tag 'start-node) position 0) little)
         (u8vector-u32-set! payload (+ offset 20)
                            (if (eq? tag 'finish-node) position 0) little)))
      ((start-field finish-field)
       (u8vector-u32-set!
        payload (+ offset 4)
        (required-index (native-language-field-index language)
                        (vector-ref event 1) 'field)
        little)
       (let (position (vector-ref event 2))
         (u8vector-u32-set! payload (+ offset 16)
                            (if (eq? tag 'start-field) position 0) little)
         (u8vector-u32-set! payload (+ offset 20)
                            (if (eq? tag 'finish-field) position 0) little)))
      ((token)
       (u8vector-u32-set!
        payload (+ offset 4)
        (required-index (native-language-terminal-index language)
                        (vector-ref event 2) 'token-kind)
        little)
       (u8vector-u64-set! payload (+ offset 8) (vector-ref event 1) little)
       (u8vector-u32-set! payload (+ offset 16) (vector-ref event 4) little)
       (u8vector-u32-set! payload (+ offset 20) (vector-ref event 5) little)))))

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

;;; Validate/count without event objects, then publish one exact-sized buffer.
;;; Both passes consume the same immutable recognition/program representation.
;;; The thunk keeps binary allocation/catalog failures outside the parse catch.
(def (native-event-payload language grammar-digest source-digest walk)
  (let (count 0)
    (def (count-node _tag _id _kind _position) (set! count (+ count 1)))
    (def (count-field _tag _field _position) (set! count (+ count 1)))
    (def (count-token _id _token) (set! count (+ count 1)))
    (walk count-node count-field count-token)
    (lambda ()
      (let ((payload (make-u8vector (+ +binary-header-size+ (* count +binary-event-size+)) 0))
            (row 0))
        (def (emit-row! tag index id start end)
          (let (offset (+ +binary-header-size+ (* row +binary-event-size+)))
            (u8vector-u32-set! payload offset tag little)
            (u8vector-u32-set! payload (+ offset 4) index little)
            (u8vector-u64-set! payload (+ offset 8) id little)
            (u8vector-u32-set! payload (+ offset 16) start little)
            (u8vector-u32-set! payload (+ offset 20) end little)
            (set! row (+ row 1))))
        (def (emit-node! tag id kind position)
          (emit-row! (if (eq? tag 'start-node) 1 2)
                     (required-index (native-language-syntax-kind-index language) kind 'syntax-kind)
                     id (if (eq? tag 'start-node) position 0)
                     (if (eq? tag 'finish-node) position 0)))
        (def (emit-field! tag field position)
          (emit-row! (if (eq? tag 'start-field) 3 4)
                     (required-index (native-language-field-index language) field 'field)
                     0 (if (eq? tag 'start-field) position 0)
                     (if (eq? tag 'finish-field) position 0)))
        (def (emit-token! id token)
          (emit-row! 5 (required-index (native-language-terminal-index language)
                                      (token-kind token) 'token-kind)
                     id (token-start token) (token-end token)))
        (walk emit-node! emit-field! emit-token!)
        (unless (= row count) (error "native event count changed during publication"))
        (subu8vector-move! #u8(71 80 65 49) 0 4 payload 0)
        (u8vector-u32-set! payload 4 1 little)
        (u8vector-u32-set! payload 12 count little)
        (copy-digest! payload 16 grammar-digest)
        (subu8vector-move! source-digest 0 32 payload 48)
        payload))))

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
          (parse-artifact-ref ((native-language-parser language) "")
                              'grammarDigest))
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
                             (descriptor-section
                              (native-language-descriptor language)
                              'terminals))))))
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

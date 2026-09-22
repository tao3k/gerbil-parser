;;; -*- Gerbil -*-
;;; Parser-owned C ABI for language-selected ParseArtifact v1 surfaces.

(import (only-in :std/vector/u8vector little u8vector-u32-set! u8vector-u64-set!)
        (only-in :std/list/list delete-duplicates/hash)
        :std/encoding/json
        (only-in :std/encoding/hex hex-decode)
        (only-in ../grammar/algebra grammar-expression-fields)
        (only-in ../language/descriptor language-grammar-grammar)
        (only-in ../runtime/artifact parse-artifact-events parse-artifact-ref)
        (only-in ../../languages/gql/iso-39075-2024/grammar
                 gql-iso-language-grammar)
        (only-in ../../languages/gql/iso-39075-2024/parser
                 parse-gql-iso-39075-2024)
        (only-in ../../languages/cypher/opencypher-2024-1/grammar
                 opencypher-2024-1-language-grammar)
        (only-in ../../languages/cypher/opencypher-2024-1/parser
                 parse-opencypher-2024-1))
(export native-abi-version
        native-descriptor-payload
        native-parse-binary-payload
        native-error-payload)

(def +gerbil-parser-native-abi-version+ 1)
(def +gerbil-parser-native-descriptor-schema+
  "gerbil-parser.native-descriptor.v1")
(def +gerbil-parser-native-error-schema+
  "gerbil-parser.native-error.v1")

(def (native-error-payload exception)
  (json->string
   (hash (schema +gerbil-parser-native-error-schema+)
         (message
          (call-with-output-string
           (lambda (port) (display-exception exception port)))))))

(defstruct native-language
  (id grammar parser syntax-kind-index terminal-index field-symbols field-index)
  transparent: #t)

(def (grammar-section grammar name)
  (cdr (assq name (language-grammar-grammar grammar))))

(def (syntax-kind->json row)
  (vector (symbol->string (car row))
          (symbol->string (cadr row))
          (list->vector (map symbol->string (caddr row)))))

(def (terminal->json row)
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
(def (grammar-field-symbols grammar)
  (delete-duplicates/hash
   (append
    (apply append (map caddr (grammar-section grammar 'syntax-kinds)))
    (apply append
           (map (lambda (row) (grammar-expression-fields (cadr row)))
                (grammar-section grammar 'rules))))
   from-end?: #t))

(def (make-native-language-context id grammar parser)
  (let (field-symbols (grammar-field-symbols grammar))
    (make-native-language
     id grammar parser
     (indexed-symbols (map car (grammar-section grammar 'syntax-kinds)))
     (indexed-symbols (map car (grammar-section grammar 'terminals)))
     field-symbols
     (indexed-symbols field-symbols))))

(def +gql-native-language+
  (delay
    (make-native-language-context "gql" gql-iso-language-grammar
                                  parse-gql-iso-39075-2024)))

(def +cypher-native-language+
  (delay
    (make-native-language-context "cypher" opencypher-2024-1-language-grammar
                                  parse-opencypher-2024-1)))

(def (resolve-native-language language)
  (cond
   ((string=? language "gql") (force +gql-native-language+))
   ((string=? language "cypher") (force +cypher-native-language+))
   (else (error "unsupported native parser language" language))))

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
  (let* ((language (resolve-native-language language-id))
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

(def (native-descriptor-payload language-id)
  (let (language (resolve-native-language language-id))
    (json->string
     (hash (schema +gerbil-parser-native-descriptor-schema+)
         (language (native-language-id language))
         (grammarDigest
          (parse-artifact-ref ((native-language-parser language) "")
                              'grammarDigest))
         (fields (list->vector
                  (map symbol->string
                       (native-language-field-symbols language))))
         (syntaxKinds
          (list->vector (map syntax-kind->json
                             (grammar-section
                              (native-language-grammar language)
                              'syntax-kinds))))
         (terminals
          (list->vector (map terminal->json
                             (grammar-section
                              (native-language-grammar language)
                              'terminals))))))))

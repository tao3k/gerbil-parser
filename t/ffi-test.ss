#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-

;; Admit the negative-case parser fixture through the normal native loader
;; before the timed test dynamically inspects its exported descriptor kinds.
(import (only-in :gerbil-parser/t/abi-datum-support abi-datum-read)
        :std/test
        (only-in :gerbil-parser/t/fixtures/runtime-record-assignments/languages/records/parser
                 parse-records)
        (only-in :gerbil-parser/t/fixtures/runtime-record-assignments/languages/records/grammar records-language-grammar)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-machine)
        (only-in :gerbil-parser/src/compiler/machine parser-machine-grammar-digest)
        (only-in :std/vector/u8vector little u8vector-u32-ref)

        (only-in :gerbil-parser/src/ffi/language-artifact-codec
                 bind-language-abi artifact-abi-version
                 abi-descriptor-payload
                 abi-parse-binary-payload)
        (only-in ../src/ffi/rust-runtime-aot
                 rust-aot-abi-version
                 rust-runtime-source))

(import (only-in :gerbil-parser/languages/gql/parser gql-language-grammar)
        (only-in :gerbil-parser/languages/cypher/parser opencypher-language-grammar))
(def gql-native (bind-language-abi gql-language-grammar))
(def cypher-native (bind-language-abi opencypher-language-grammar))

(deflanguage-parser-tests ffi-tests "parser-owned native ParseArtifact v1 ABI"
  (engine)
    (scenario "descriptor publishes the parser-owned grammar surface"
      (equal (artifact-abi-version)  1)
      (scope ((descriptor (abi-datum-read (abi-descriptor-payload gql-native)
                                    )))
        (equal (hash-get descriptor "schema")
                "gerbil-parser.native-descriptor.v1")
        (equal (hash-get descriptor "language")  "gql")
        (equal (positive? (length (hash-get descriptor "syntaxKinds")))
                #t)
        (scope ((fields (hash-get descriptor "fields")))
          (equal (not (not (member "operator" fields)))  #t)
          (equal (not (not (member "sign" fields)))  #t))))
    (property "accepted source produces typed binary ParseArtifact v1"
      (bindings (artifact
            (abi-parse-binary-payload gql-native "MATCH (n) RETURN n")))
      (equal (subu8vector artifact 0 4)  #u8(71 80 65 49))
      (equal (u8vector-u32-ref artifact 4 little)  1)
      (equal (u8vector-u32-ref artifact 8 little)  0)
      (equal (positive? (u8vector-u32-ref artifact 12 little))  #t)
      (equal (u8vector-length artifact)
                (+ 80 (* 24 (u8vector-u32-ref artifact 12 little)))))
    (property "openCypher uses its own descriptor and parser"
      (bindings (descriptor
              (abi-datum-read (abi-descriptor-payload cypher-native)
                            )) (artifact
              (abi-parse-binary-payload
               cypher-native "MATCH (n:Person) RETURN n\n")))
      (equal (hash-get descriptor "language")  "opencypher")
      (equal (positive? (length (hash-get descriptor "syntaxKinds")))
                #t)
      (equal (u8vector-u32-ref artifact 8 little)  0)
      (equal (positive? (u8vector-u32-ref artifact 12 little))  #t))
    (scenario "invalid descriptor fails closed"
      (raises (bind-language-abi "unknown") true))
    (scenario "grammar path compiles through the Scheme Rust backend"
      (equal (rust-aot-abi-version)  1)
      (scope ((source
            (rust-runtime-source
             "t/fixtures/runtime-record-assignments/languages/records/grammar.ss")))
        (equal (not (not (string-contains
                           source "language: \"record-assignments\"")))  #t)
        (equal (not (not (string-contains
                           source (string-append "grammar_digest: \""
                             (parser-machine-grammar-digest
                               (language-grammar-machine records-language-grammar)) "\"" ))))  #t)
        (equal (not (not (string-contains
                           source "pub static LANGUAGE: LanguageSpec")))  #t)))
    (scenario "module without a language descriptor fails closed"
      (raises
       (rust-runtime-source
        "t/fixtures/runtime-record-assignments/languages/records/parser.ss")
       true)))


;; gxtest discovers only exported names ending in -test.
(def ffi-test ffi-tests)
(export ffi-test)

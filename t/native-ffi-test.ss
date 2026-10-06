#!/usr/bin/env gxi
;;; -*- Gerbil -*-

;; Admit the negative-case parser fixture through the normal native loader
;; before the timed test dynamically inspects its exported descriptor kinds.
(import (only-in :gerbil-parser/t/native-datum-support native-datum-read)
        :std/test
        (only-in :gerbil-parser/t/fixtures/rowan-record-assignments/languages/records/parser
                 parse-records)
        (only-in :std/vector/u8vector little u8vector-u32-ref)

        (only-in :gerbil-parser/src/ffi/parse-artifact-v1
                 native-abi-version
                 native-descriptor-payload
                 native-parse-binary-payload)
        (only-in ../src/ffi/rust-rowan-aot-v1
                 native-rowan-aot-abi-version
                 native-rust-rowan-source))

(def native-ffi-tests
  (test-suite "parser-owned native ParseArtifact v1 ABI"
    (test-case "descriptor publishes the parser-owned grammar surface"
      (check (native-abi-version) => 1)
      (let (descriptor (native-datum-read (native-descriptor-payload "gql")
                                    ))
        (check (hash-get descriptor "schema")
               => "gerbil-parser.native-descriptor.v1")
        (check (hash-get descriptor "language") => "gql")
        (check (positive? (length (hash-get descriptor "syntaxKinds")))
               => #t)
        (let (fields (hash-get descriptor "fields"))
          (check (not (not (member "operator" fields))) => #t)
          (check (not (not (member "sign" fields))) => #t))))
    (test-case "accepted source produces typed binary ParseArtifact v1"
      (let (artifact
            (native-parse-binary-payload "gql" "MATCH (n) RETURN n"))
        (check (subu8vector artifact 0 4) => #u8(71 80 65 49))
        (check (u8vector-u32-ref artifact 4 little) => 1)
        (check (u8vector-u32-ref artifact 8 little) => 0)
        (check (positive? (u8vector-u32-ref artifact 12 little)) => #t)
        (check (u8vector-length artifact)
               => (+ 80 (* 24 (u8vector-u32-ref artifact 12 little))))))
    (test-case "openCypher uses its own descriptor and parser"
      (let* ((descriptor
              (native-datum-read (native-descriptor-payload "cypher")
                            ))
             (artifact
              (native-parse-binary-payload
               "cypher" "MATCH (n:Person) RETURN n\n")))
        (check (hash-get descriptor "language") => "cypher")
        (check (positive? (length (hash-get descriptor "syntaxKinds")))
               => #t)
        (check (u8vector-u32-ref artifact 8 little) => 0)
        (check (positive? (u8vector-u32-ref artifact 12 little)) => #t)))
    (test-case "unknown language fails closed"
      (check-exception (native-descriptor-payload "unknown") true))
    (test-case "grammar path compiles through the Scheme Rowan backend"
      (check (native-rowan-aot-abi-version) => 1)
      (let (source
            (native-rust-rowan-source
             "t/fixtures/rowan-record-assignments/languages/records/grammar.ss"))
        (check (not (not (string-contains
                           source "language: \"record-assignments\""))) => #t)
        (check (not (not (string-contains
                           source "grammar_digest: \"sha256:9c7e8b0f"))) => #t)
        (check (not (not (string-contains
                           source "pub static LANGUAGE: LanguageSpec"))) => #t)))
    (test-case "module without a language descriptor fails closed"
      (check-exception
       (native-rust-rowan-source
        "t/fixtures/rowan-record-assignments/languages/records/parser.ss")
       true))))

(export native-ffi-tests)

;; gxtest discovers only exported names ending in -test.
(def native-ffi-test native-ffi-tests)
(export native-ffi-test)

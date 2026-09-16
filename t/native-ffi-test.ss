#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        (only-in :std/misc/bytes little u8vector-u32-ref)
        :std/text/json
        (only-in ../src/ffi/parse-artifact-v1
                 native-abi-version
                 native-descriptor-payload
                 native-parse-binary-payload))

(def native-ffi-tests
  (test-suite "parser-owned native ParseArtifact v1 ABI"
    (test-case "descriptor publishes the parser-owned grammar surface"
      (check (native-abi-version) => 1)
      (let (descriptor (string->json-object (native-descriptor-payload)))
        (check (hash-get descriptor "schema")
               => "gerbil-parser.native-descriptor.v1")
        (check (positive? (length (hash-get descriptor "syntaxKinds")))
               => #t)
        (let (fields (hash-get descriptor "fields"))
          (check (not (not (member "operator" fields))) => #t)
          (check (not (not (member "sign" fields))) => #t))))
    (test-case "accepted source produces typed binary ParseArtifact v1"
      (let (artifact (native-parse-binary-payload "MATCH (n) RETURN n"))
        (check (subu8vector artifact 0 4) => #u8(71 80 65 49))
        (check (u8vector-u32-ref artifact 4 little) => 1)
        (check (u8vector-u32-ref artifact 8 little) => 0)
        (check (positive? (u8vector-u32-ref artifact 12 little)) => #t)
        (check (u8vector-length artifact)
               => (+ 80 (* 24 (u8vector-u32-ref artifact 12 little))))))))

(run-tests! native-ffi-tests)

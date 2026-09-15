#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :std/text/json
        (only-in ../src/ffi/parse-artifact-v1
                 native-abi-version
                 native-descriptor-payload
                 native-parse-payload))

(def native-ffi-tests
  (test-suite "parser-owned native ParseArtifact v1 ABI"
    (test-case "descriptor publishes the parser-owned grammar surface"
      (check (native-abi-version) => 1)
      (let (descriptor (string->json-object (native-descriptor-payload)))
        (check (hash-get descriptor "schema")
               => "gerbil-parser.native-descriptor.v1")
        (check (positive? (length (hash-get descriptor "syntaxKinds")))
               => #t)))
    (test-case "accepted source produces ParseArtifact v1"
      (let (artifact
            (string->json-object
             (native-parse-payload "MATCH (n) RETURN n")))
        (check (hash-get artifact "schema")
               => "gerbil-parser.parse-artifact.v1")
        (check (hash-get artifact "status") => "accepted")))))

(run-tests! native-ffi-tests)

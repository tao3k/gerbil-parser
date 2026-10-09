(import :std/test :std/encoding/json :std/io/tempfile :std/misc/ports
        (only-in "../tools/ci/bootstrap.ss" admit-release! append-lines!))
(export bootstrap-test)
(def bootstrap-test
  (test-suite "SDK release bootstrap"
    (test-case "immutable release platform admission"
      (let (metadata (string->json "{\"schema\":\"gerbil-bazel.toolchain-release.v1\",\"upstreamRevision\":\"2591dcd9b7c6d2c4e9dd8611a17c5b1a5d82bbdb\",\"platform\":{\"os\":\"darwin\",\"arch\":\"aarch64\"}}"
                                  (JSONReadOptions object-as-hash: #t)))
        (admit-release! metadata "darwin" "aarch64")
        (check-exception (admit-release! metadata "linux" "x86_64") true)))
    (test-case "environment publication preserves append bytes"
      (let (file (make-temporary-file-name "parser-bootstrap-env-"))
        (try
          (append-lines! file '("FIRST=one" "SECOND=two"))
          (append-lines! file '("THIRD=three"))
          (check (call-with-input-file file read-all-as-string)
            => "FIRST=one\nSECOND=two\nTHIRD=three\n")
          (finally (when (file-exists? file) (delete-file file))))))))

(import :gerbil-parser/language-test-support)
(import :std/test :std/encoding/json :std/io/tempfile :std/misc/ports
        (only-in "../tools/ci/bootstrap.ss" admit-release! append-lines! selected-build-cores))

(deflanguage-parser-tests bootstrap-test "SDK release bootstrap"
  (engine)
    (scenario "explicit worker capacity survives host detection"
      (equal (selected-build-cores "2" "3") "2")
      (equal (selected-build-cores #f "3") "3")
      (each (invalid) ('("0" "-1" "2.5" "" "invalid"))
        (raises (selected-build-cores invalid "3") true)))
    (scenario "immutable release platform admission"
      (scope ((metadata (string->json "{\"schema\":\"gerbil-bazel.toolchain-release.v1\",\"upstreamRevision\":\"2591dcd9b7c6d2c4e9dd8611a17c5b1a5d82bbdb\",\"platform\":{\"os\":\"darwin\",\"arch\":\"aarch64\"}}"
                                  (JSONReadOptions object-as-hash: #t))))
        (act (admit-release! metadata "darwin" "aarch64"))
        (raises (admit-release! metadata "linux" "x86_64") true)))
    (scenario "environment publication preserves append bytes"
      (scope ((file (make-temporary-file-name "parser-bootstrap-env-")))
        (protect (body
          (act (append-lines! file '("FIRST=one" "SECOND=two")))
          (act (append-lines! file '("THIRD=three")))
          (equal (call-with-input-file file read-all-as-string)
             "FIRST=one\nSECOND=two\nTHIRD=three\n")
          ) (cleanup (when (file-exists? file) (act (delete-file file))))))))

;;; Native language admission uses independent descriptors, never builtin names.
(import :std/test
        :std/encoding/json
        (only-in :std/vector/u8vector little u8vector-u32-ref)
        (only-in :gerbil-parser/t/fixtures/shared-scanner/records records-language-grammar records-contextual-product)
        (only-in :gerbil-parser/src/ffi/language-handles register-native-language! release-native-language!
                 native-language-handle-descriptor native-language-handle-parse)
        (only-in :gerbil-parser/src/runtime/parser prepare-contextual-parser parse-source/contextual/prepared)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-machine)
        (only-in :gerbil-parser/src/language/entry parse-language-source)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-events parse-artifact-ref)
        (only-in :gerbil-parser/src/compiler/native-language generate-native-language-pack))
(def native-language-tests
  (test-suite "independent language native admission"
    (test-case "contextual and canonical Scheme trees preserve exact events"
      (let (plan (prepare-contextual-parser (language-grammar-machine records-language-grammar) records-contextual-product))
        (for-each
         (lambda (source)
           (let ((canonical (parse-language-source records-language-grammar source))
                 (contextual (parse-source/contextual/prepared plan source)))
             (check (parse-artifact-ref contextual 'status) => (parse-artifact-ref canonical 'status))
             (check (parse-artifact-events contextual) => (parse-artifact-events canonical))))
         '("" "α=1\n" "a = 1\r\nb=\"β\"\n" "name=value"))))
    (test-case "two handles retain distinct lifetimes and descriptor identity"
      (let ((a (register-native-language! records-language-grammar records-contextual-product))
            (b (register-native-language! records-language-grammar)))
        (check (not (= a b)) => #t)
        (let (descriptor (string->json (native-language-handle-descriptor a) (JSONReadOptions object-as-hash: #t)))
          (check (hash-get descriptor "language") => "record-assignments"))
        (check (u8vector-u32-ref (native-language-handle-parse a (string->utf8 "α=1\n")) 8 little) => 0)
        (release-native-language! a) (release-native-language! a)
        (check-exception (native-language-handle-parse a #u8()) (lambda (e) (equal? (error-message e) "unknown or released native language handle")))
        (check (u8vector-u32-ref (native-language-handle-parse b #u8()) 8 little) => 0)
        (release-native-language! b)))
    (test-case "invalid UTF-8 and embedded NUL have distinct admission outcomes"
      (let (handle (register-native-language! records-language-grammar records-contextual-product))
        (check-exception (native-language-handle-parse handle #u8(255)) true)
        (check (u8vector-u32-ref (native-language-handle-parse handle #u8(97 61 49 0 33)) 8 little) => 1)
        (release-native-language! handle)))
    (test-case "C symbol injection fails before producing any output"
      (check-exception
       (generate-native-language-pack "/private/tmp/should-not-exist.ss" "/private/tmp/should-not-exist.h"
                                      "pack/entry" "pack/grammar" 'grammar "bad;symbol") true))))
(export native-language-tests)

(def native-language-test native-language-tests)
(export native-language-test)

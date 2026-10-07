;;; Native language admission uses independent descriptors, never builtin names.
(import :std/test
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/src/language/source declare-source-language source-language-result-catalog source-language-digest)
        (only-in :gerbil-parser/src/runtime/source-engines LineSourceStrategy.)
        (only-in :gerbil-parser/languages/bash/parser bash-source-language)
        (only-in :gerbil-parser/t/fixtures/native-ffi/language-v2-probe probe-source-language)
        (only-in :gerbil-parser/t/native-datum-support native-datum-read)
        (only-in :std/vector/u8vector little u8vector-u32-ref)
        (only-in :gerbil-parser/t/fixtures/shared-scanner/records records-language-grammar records-contextual-product)
        (only-in :gerbil-parser/src/ffi/language-handles register-native-language! release-native-language!
                 native-language-handle-descriptor native-language-handle-parse)
        (only-in :gerbil-parser/src/runtime/parser prepare-contextual-parser parse-source/contextual/prepared)
        (only-in :gerbil-parser/src/runtime/lr-parser current-lr-event-program-enabled?)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-machine
                 +language-grammar-schema+ language-grammar-language language-grammar-version
                 language-grammar-contract language-grammar-grammar language-grammar-ir
                 language-grammar-observability make-language-grammar)
        (only-in :gerbil-parser/src/ffi/language-artifact-codec
                 make-native-language-context native-descriptor-payload
                 bind-native-language native-parse-binary-payload native-parse-binary-payload/bytes)
        (only-in :gerbil-parser/t/fixtures/progress report-test-progress!)
        (only-in :gerbil-parser/src/language/entry parse-language-source)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-events parse-artifact-ref)
        (only-in :gerbil-parser/src/compiler/native-language generate-native-language-pack))
(def native-language-tests
  (test-suite "independent language native admission"
    (test-case "descriptor queries never execute a parser and retain bound identity"
      (for-each
       (lambda (descriptor)
         (let* ((calls 0)
                (language (make-native-language-context
                           "metadata-only" descriptor
                           (lambda (_) (set! calls (+ calls 1))
                             (error "metadata must not execute recognition"))))
                (metadata (native-datum-read (native-descriptor-payload language))))
           (check calls => 0)
           (check (string? (hash-get metadata "grammarDigest")) => #t)))
       (list records-language-grammar bash-source-language))
      (let* ((language (bind-native-language records-language-grammar records-contextual-product))
             (metadata (native-datum-read (native-descriptor-payload language))))
        (check (hash-get metadata "grammarDigest") => (cdr (assq 'digest records-contextual-product)))
        (for-each
         (lambda (source)
           (let* ((plan (prepare-contextual-parser (language-grammar-machine records-language-grammar)
                                                   records-contextual-product))
                  (artifact (parse-source/contextual/prepared plan source)))
             (check (hash-get metadata "grammarDigest") => (parse-artifact-ref artifact 'grammarDigest))))
         '("a=1\n" "!"))))
    (test-case "contextual and canonical Scheme trees preserve exact events"
      (let (plan (prepare-contextual-parser (language-grammar-machine records-language-grammar) records-contextual-product))
        (for-each
         (lambda (source)
           (let ((canonical (parse-language-source records-language-grammar source))
                 (contextual (parse-source/contextual/prepared plan source)))
             (check (parse-artifact-ref contextual 'status) => (parse-artifact-ref canonical 'status))
             (check (parse-artifact-events contextual) => (parse-artifact-events canonical))))
         '("" "α=1\n" "a = 1\r\nb=\"β\"\n" "name=value"))))
    (test-case "direct publication matches every canonical payload byte at scale"
      (for-each
       (lambda (events?)
        (parameterize ((current-lr-event-program-enabled? events?))
         (let (language (bind-native-language records-language-grammar records-contextual-product))
        (for-each
         (lambda (source)
           (check (native-parse-binary-payload/bytes language (string->utf8 source))
                  => (native-parse-binary-payload language source))
           (report-test-progress! "NATIVE-PAYLOAD-PARITY-OK bytes="
                                  (u8vector-length (string->utf8 source))))
         (append '("" "α=1\n" "a = 1\r\nb=\"β\"\n" "name=value" "a=1\x0;!" "a=1\n!")
                 (map (lambda (row) (apply string-append (make-list 4096 row)))
                      '("a=1\n" "α=1\r\n")))))))
       '(#f #t)))
    (test-case "strict byte admission rejects noncanonical UTF-8 before publication"
      (let (language (bind-native-language records-language-grammar records-contextual-product))
        (for-each
         (lambda (bytes) (check-exception (native-parse-binary-payload/bytes language bytes) true))
         '(#u8(255) #u8(192 175) #u8(237 160 128) #u8(244 144 128 128) #u8(226 130)))))
    (test-case "binary catalog errors escape rather than become rejected source"
      (let* ((descriptor records-language-grammar)
             (grammar (map (lambda (row) (if (eq? (car row) 'syntax-kinds)
                                             (cons 'syntax-kinds '()) row))
                           (language-grammar-grammar descriptor)))
             (invalid (make-language-grammar
                       +language-grammar-schema+ (language-grammar-language descriptor)
                       (language-grammar-version descriptor) (language-grammar-contract descriptor)
                       grammar (language-grammar-ir descriptor) (language-grammar-machine descriptor)
                       (language-grammar-observability descriptor)))
             (language (bind-native-language invalid records-contextual-product)))
        (check-exception (native-parse-binary-payload/bytes language (string->utf8 "a=1\n"))
                         (lambda (e) (equal? (error-message e)
                                            "native ParseArtifact symbol is outside descriptor")))))
    (test-case "two handles retain distinct lifetimes and descriptor identity"
      (let ((a (register-native-language! records-language-grammar records-contextual-product))
            (b (register-native-language! records-language-grammar)))
        (check (not (= a b)) => #t)
        (let (descriptor (native-datum-read (native-language-handle-descriptor a)))
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
    (test-case "source descriptors bind to the same length-delimited public C ABI"
      (for-each (lambda (descriptor)
                  (check (probe-source-language (register-native-language! descriptor)) => 0))
                (list bash-source-language
                      (declare-source-language "independent-lines" "1" "lines.v1"
                        (.o (:: self LineSourceStrategy.) root-kind: 'IndependentFile token-kind: 'IndependentLine)))))
    (test-case "source result catalog drives binary symbol indexes and rejected publications"
      (let* ((native (bind-native-language bash-source-language))
             (source "echo \"α${x:-中}\"\n"))
        (check (native-parse-binary-payload native source)
               => (native-parse-binary-payload/bytes native (string->utf8 source)))
        (check (u8vector-u32-ref (native-parse-binary-payload native "if true; then\n") 8 little) => 1)
        (check-exception (bind-native-language bash-source-language records-contextual-product) true)))
    (test-case "C symbol injection fails before producing any output"
      (check-exception
       (generate-native-language-pack "/private/tmp/should-not-exist.ss" "/private/tmp/should-not-exist.h"
                                      "pack/entry" "pack/grammar" 'grammar "bad;symbol") true))))
(export native-language-tests)

(def native-language-test native-language-tests)
(export native-language-test)

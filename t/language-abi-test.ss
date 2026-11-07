(import :gerbil-parser/language-test-support)
;;; Native language admission uses independent descriptors, never builtin names.
(import :std/test
        (only-in :gerbil-parser/src/ffi/language-abi-context language-abi-context-source-admission write-abi-row! ascii-source-bytes? copy-ascii-source-range! ascii-source-range?)
        (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/src/language/source declare-source-language source-language-result-catalog source-language-digest)
        (only-in :gerbil-parser/src/runtime/source-engines LineSourceStrategy.)
        (only-in :gerbil-parser/languages/bash/parser bash-source-language)
        (only-in :gerbil-parser/t/fixtures/ffi/language-probe probe-source-language)
        (only-in :gerbil-parser/t/abi-datum-support abi-datum-read)
        (only-in :std/vector/u8vector little u8vector-u32-ref u8vector-u64-ref)
        (only-in :gerbil-parser/t/fixtures/shared-scanner/records records-language-grammar records-contextual-product)
        (only-in :gerbil-parser/src/ffi/language-handles register-language! release-language!
                 language-handle-descriptor language-handle-parse
                 language-handle-parse/publish language-handle-scan-counts
                 language-handle-count)
        (only-in :gerbil-parser/src/runtime/parser prepare-contextual-parser parse-source/contextual/prepared)
        (only-in :gerbil-parser/src/runtime/lr-parser current-lr-event-program-enabled?)
        (only-in :gerbil-parser/src/language/descriptor language-grammar-machine
                 +language-grammar-schema+ language-grammar-language language-grammar-version
                 language-grammar-contract language-grammar-grammar language-grammar-ir
                 language-grammar-observability make-language-grammar)
        (only-in :gerbil-parser/src/ffi/language-artifact-codec
                 prepare-language-abi-context abi-descriptor-payload
                 bind-language-abi abi-parse-binary-payload abi-parse-binary-payload/bytes)
        (only-in :gerbil-parser/t/fixtures/progress report-test-progress!)
        (only-in :gerbil-parser/src/language/entry parse-language-source)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-events parse-artifact-ref)
        (only-in :gerbil-parser/src/compiler/language-pack generate-language-pack))
(deflanguage-parser-tests language-abi-tests "independent language native admission"
  (engine)
    (scenario "bounded GPA1 rows preserve little endian widths and adjacent bytes"
      (scope ((bytes (make-u8vector 26 170)))
        (act (write-abi-row! bytes 1 4294967295 2147483648 18446744073709551615 0 4294967295))
        (equal (u8vector-ref bytes 0)  170)
        (equal (u8vector-ref bytes 25)  170)
        (equal (u8vector-u32-ref bytes 1 little)  4294967295)
        (equal (u8vector-u32-ref bytes 5 little)  2147483648)
        (equal (u8vector-u64-ref bytes 9 little)  18446744073709551615)
        (equal (u8vector-u32-ref bytes 17 little)  0)
        (equal (u8vector-u32-ref bytes 21 little)  4294967295)
        (scope ((before (u8vector-copy bytes)))
          (each (offset) ('(-1 3 26 18446744073709551615))
            (raises (write-abi-row! bytes offset 1 2 3 4 5) true)
            (equal bytes  before))
          (raises (write-abi-row! bytes 1 4294967296 0 0 0 0) true)
          (equal bytes  before))))
    (scenario "inline fixnum row admission preserves full widths and rejects before writes"
      (scope ((bytes (make-u8vector 26 170)))
        (act (write-abi-row! bytes 1 4294967295 2147483648 4294967296 4294967295 0))
        (equal (u8vector-ref bytes 0)  170)
        (equal (u8vector-ref bytes 25)  170)
        (equal (u8vector-u32-ref bytes 1 little)  4294967295)
        (equal (u8vector-u32-ref bytes 5 little)  2147483648)
        (equal (u8vector-u64-ref bytes 9 little)  4294967296)
        (equal (u8vector-u32-ref bytes 17 little)  4294967295)
        (equal (u8vector-u32-ref bytes 21 little)  0)
        (scope ((before (u8vector-copy bytes)))
          (raises (write-abi-row! bytes 1 1 2 3 4 4294967296) true)
          (equal bytes  before)
          (raises (write-abi-row! bytes 1 1 2 -1 4 5) true)
          (equal bytes  before)
          (raises (write-abi-row! bytes 1 1 2 3 4 5.5) true)
          (equal bytes  before))))
    (scenario "native Source handles own edit histories and publish fresh payload parity"
      (sequential ((baseline (language-handle-count))
             (a (register-language! bash-source-language))
             (b (register-language! bash-source-language))
             (language (bind-language-abi bash-source-language))
             (first "cat <<A\nα\nA\necho end\n")
             (edit "echo猫 <<A\nα\nA\necho end\n"))
        (equal (language-handle-parse a (string->utf8 first))  (abi-parse-binary-payload language first))
        (equal (language-handle-parse a (string->utf8 edit))  (abi-parse-binary-payload language edit))
        (scope-values (((scanned reused) (language-handle-scan-counts a)))
          (equal scanned  1) (equal (> reused 0)  #t))
        ;; A new handle cannot consume another handle's token/context history.
        (equal (language-handle-parse b (string->utf8 edit))  (abi-parse-binary-payload language edit))
        (scope-values (((scanned reused) (language-handle-scan-counts b)))
          (equal (> scanned 1)  #t) (equal reused  0))
        (each (source) ('("echo猫 <<'A'\nα\nA\necho end\n" "cat <<A <<B\nα\nA\nβ\nB\necho end\n"
           "cat <<A <<C\nα\nA\nβ\nC\necho end\n" "if true; then\n" "cat <<A\nα\n" "echo done\n" ""))
           (equal (language-handle-parse a (string->utf8 source))
                   (abi-parse-binary-payload language source)))
        (act (release-language! a)) (act (release-language! b))
        (equal (language-handle-count)  baseline)
        (raises (language-handle-scan-counts a) true)))
    (scenario "failed UTF-8 or result publication preserves the previous Source history"
      (sequential ((handle (register-language! bash-source-language))
             (first "cat <<A\nα\nA\necho end\n")
             (edit "echo猫 <<A\nα\nA\necho end\n"))
        (act (language-handle-parse handle (string->utf8 first)))
        (scope-values (((before-scanned before-reused) (language-handle-scan-counts handle)))
          (raises (language-handle-parse handle #u8(255)) true)
          ;; Simulate the C allocation/copy owner rejecting a complete payload.
          (act (language-handle-parse/publish handle (string->utf8 "unrelated\n") (lambda (_) #f)))
          (raises
           (language-handle-parse/publish handle (string->utf8 "unrelated\n")
             (lambda (_) (error "publication failed"))) true)
          (scope-values (((scanned reused) (language-handle-scan-counts handle)))
            (equal scanned  before-scanned) (equal reused  before-reused)))
        (act (language-handle-descriptor handle))
        (act (language-handle-parse handle (string->utf8 edit)))
        (scope-values (((scanned reused) (language-handle-scan-counts handle)))
          (equal scanned  1) (equal (> reused 0)  #t))
        ;; Lexical rejection publishes a diagnostic but cannot authorize reuse.
        (act (language-handle-parse handle (string->utf8 "cat <<A\nα\n")))
        (scope-values (((scanned reused) (language-handle-scan-counts handle)))
          (equal scanned  #f) (equal reused  0))
        (act (language-handle-parse handle (string->utf8 edit)))
        (scope-values (((scanned reused) (language-handle-scan-counts handle)))
          (equal (> scanned 1)  #t) (equal reused  0))
        (act (release-language! handle))))
    (scenario "descriptor queries never execute a parser and retain bound identity"
      (each (descriptor) ((list records-language-grammar bash-source-language))
         (sequential ((calls 0)
                (language (prepare-language-abi-context
                           "metadata-only" descriptor
                           (lambda (_) (set! calls (+ calls 1))
                             (error "metadata must not execute recognition"))))
                (metadata (abi-datum-read (abi-descriptor-payload language))))
           (equal calls  0)
           (equal (string? (hash-get metadata "grammarDigest"))  #t)))
      (sequential ((language (bind-language-abi records-language-grammar records-contextual-product))
             (metadata (abi-datum-read (abi-descriptor-payload language))))
        (equal (hash-get metadata "grammarDigest")  (cdr (assq 'digest records-contextual-product)))
        (each (source) ('("a=1\n" "!"))
           (sequential ((plan (prepare-contextual-parser (language-grammar-machine records-language-grammar)
                                                   records-contextual-product))
                  (artifact (parse-source/contextual/prepared plan source)))
             (equal (hash-get metadata "grammarDigest")  (parse-artifact-ref artifact 'grammarDigest))))))
    (scenario "contextual and canonical Scheme trees preserve exact events"
      (scope ((plan (prepare-contextual-parser (language-grammar-machine records-language-grammar) records-contextual-product)))
        (each (source) ('("" "α=1\n" "a = 1\r\nb=\"β\"\n" "name=value"))
           (scope ((canonical (parse-language-source records-language-grammar source))
                 (contextual (parse-source/contextual/prepared plan source)))
             (equal (parse-artifact-ref contextual 'status)  (parse-artifact-ref canonical 'status))
             (equal (parse-artifact-events contextual)  (parse-artifact-events canonical))))))
    (scenario "direct publication matches every canonical payload byte at scale"
      (each (events?) ('(#f #t))
        (parameters ((current-lr-event-program-enabled? events?))
         (scope ((language (bind-language-abi records-language-grammar records-contextual-product)))
        (each (source) ((append '("" "α=1\n" "a = 1\r\nb=\"β\"\n" "name=value" "a=1\x0;!" "a=1\n!")
                 (map (lambda (row) (apply string-append (make-list 4096 row)))
                      '("a=1\n" "α=1\r\n"))))
           (equal (abi-parse-binary-payload/bytes language (string->utf8 source))
                   (abi-parse-binary-payload language source))
           (act (report-test-progress! "NATIVE-PAYLOAD-PARITY-OK bytes="
                                  (u8vector-length (string->utf8 source)))))))))
    (scenario "ASCII canonical proof covers every byte and excludes all high bytes"
      (sequential ((language (bind-language-abi records-language-grammar records-contextual-product))
             (admit (language-abi-context-source-admission language))
             (bytes (list->u8vector (iota 128)))
             (source (list->string (map integer->char (iota 128)))))
        (equal (ascii-source-bytes? bytes)  #t)
        (equal (admit bytes)  source)
        (equal (admit #u8())  "")
        (each (width) ('(4095 4096 4097 8191 8192 8193))
          (scope ((text (make-string width #\a)))
            (equal (admit (string->utf8 text))  text)
            (equal (admit (string->utf8 (string-append text "猫")))  (string-append text "猫"))))
        (each (byte) ((iota 256))
          (equal (ascii-source-bytes? (u8vector byte))  (< byte 128)))))
    (scenario "ASCII leaves reject oversized ranges and short destinations before writing"
      (scope ((bytes (make-u8vector 4097 97)) (source (make-string 4097 #\z)))
        (equal (ascii-source-range? bytes 0 4097)  #f)
        (equal (copy-ascii-source-range! bytes source 0 4097)  #f)
        (equal source  (make-string 4097 #\z))
        (equal (copy-ascii-source-range! bytes (make-string 4096 #\z) 0 4096)  #f)
        (equal (copy-ascii-source-range! bytes source 0 4096)  #t)
        (equal (string-ref source 4096)  #\z)
        (equal (copy-ascii-source-range! bytes source 4096 4097)  #t)
        (equal source  (make-string 4097 #\a))))
    (scenario "upstream bulk admission preserves Unicode scalars and chunk boundaries"
      (sequential ((language (bind-language-abi records-language-grammar records-contextual-product))
             (admit (language-abi-context-source-admission language))
             (scalars (map integer->char '(0 127 128 2047 2048 55295 57344 65535 65536 1114111))))
        (each (width) ('(0 245 246 255 256 257 511 512))
           (sequential ((source (list->string (append (make-list width #\a) scalars)))
                  (bytes (string->utf8 source)))
             (equal (admit bytes)  source)))))
    (scenario "strict byte admission rejects noncanonical UTF-8 before publication"
      (scope ((language (bind-language-abi records-language-grammar records-contextual-product)))
        (each (bytes) ('(#u8(255) #u8(192 175) #u8(237 160 128) #u8(244 144 128 128) #u8(226 130))) (raises (abi-parse-binary-payload/bytes language bytes) true))))
    (scenario "binary catalog errors escape rather than become rejected source"
      (sequential ((descriptor records-language-grammar)
             (grammar (map (lambda (row) (if (eq? (car row) 'syntax-kinds)
                                             (cons 'syntax-kinds '()) row))
                           (language-grammar-grammar descriptor)))
             (invalid (make-language-grammar
                       +language-grammar-schema+ (language-grammar-language descriptor)
                       (language-grammar-version descriptor) (language-grammar-contract descriptor)
                       grammar (language-grammar-ir descriptor) (language-grammar-machine descriptor)
                       (language-grammar-observability descriptor)))
             (language (bind-language-abi invalid records-contextual-product)))
        (raises (abi-parse-binary-payload/bytes language (string->utf8 "a=1\n"))
                         (lambda (e) (equal? (error-message e)
                                            "ParseArtifact ABI symbol is outside descriptor")))))
    (scenario "two handles retain distinct lifetimes and descriptor identity"
      (scope ((a (register-language! records-language-grammar records-contextual-product))
            (b (register-language! records-language-grammar)))
        (equal (not (= a b))  #t)
        (scope ((descriptor (abi-datum-read (language-handle-descriptor a))))
          (equal (hash-get descriptor "language")  "record-assignments"))
        (equal (u8vector-u32-ref (language-handle-parse a (string->utf8 "α=1\n")) 8 little)  0)
        (act (release-language! a)) (act (release-language! a))
        (raises (language-handle-parse a #u8()) (lambda (e) (equal? (error-message e) "unknown or released language ABI handle")))
        (equal (u8vector-u32-ref (language-handle-parse b #u8()) 8 little)  0)
        (act (release-language! b))))
    (scenario "invalid UTF-8 and embedded NUL have distinct admission outcomes"
      (scope ((handle (register-language! records-language-grammar records-contextual-product)))
        (raises (language-handle-parse handle #u8(255)) true)
        (equal (u8vector-u32-ref (language-handle-parse handle #u8(97 61 49 0 33)) 8 little)  1)
        (act (release-language! handle))))
    (scenario "source descriptors bind to the same length-delimited public C ABI"
      (each (descriptor) ((list bash-source-language
                      (declare-source-language "independent-lines" "1" "lines.v1"
                        (.o (:: self LineSourceStrategy.) root-kind: 'IndependentFile token-kind: 'IndependentLine))))
                  (equal (probe-source-language (register-language! descriptor))  0)))
    (scenario "source result catalog drives binary symbol indexes and rejected publications"
      (sequential ((native (bind-language-abi bash-source-language))
             (source "echo \"α${x:-中}\"\n"))
        (equal (abi-parse-binary-payload native source)
                (abi-parse-binary-payload/bytes native (string->utf8 source)))
        (equal (u8vector-u32-ref (abi-parse-binary-payload native "if true; then\n") 8 little)  1)
        (raises (bind-language-abi bash-source-language records-contextual-product) true)))
    (scenario "C symbol injection fails before producing any output"
      (raises
       (generate-language-pack "/private/tmp/should-not-exist.ss" "/private/tmp/should-not-exist.h"
                                      "pack/entry" "pack/grammar" 'grammar "bad;symbol") true)))


(def language-abi-test language-abi-tests)
(export language-abi-test)

;;; Both ABI Suites share the persistent runtime owner thread.


;; Affinity preserves the persistent ABI owner thread but does not isolate its
;; C-to-Scheme negative-path return from another Worker's Scheme execution.
;; Keep this normal-return control inside an exclusive native-entry interval;
;; concurrent host reentry is an external, separately unqualified premise.

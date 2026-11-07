;;; Complete immutable publication and authenticated artifact admission.
(import (only-in :std/io/tempfile make-temporary-file-name)
        (only-in :std/list/list-builder with-list-builder)
        (only-in :std/misc/ports read-all-as-u8vector)
        (only-in :gerbil-parser/src/compiler/language-artifact
                 materialize-compiled-language-artifact/output-dirs)
        (only-in :gerbil-parser/src/runtime/language-artifact
                 compiled-language-artifact-relative-path
                 load-compiled-language-artifact/roots
                 load-compiled-language-artifact/embedded)
        (only-in :gerbil-parser/src/runtime/embedded-image pack-language-artifact-image)
        (only-in :gerbil-parser/src/runtime/identity sha256-text)
        (only-in :std/encoding/zlib compress)
        (only-in ../parser-stage-cost/benchmark
                 measure-parser-component measure-parser-cpu-pairs))
(export benchmark-language-artifact compare-language-artifact)

(def +schema+ "gerbil-parser.language-artifact-benchmark.v1")

;; An independent ordered wire datum; no language parser or compiler cache
;; participates in fixture construction or semantic result admission.
(def (artifact-value width)
  (let (payload
        (with-list-builder (push!)
          (let loop ((index 0))
            (unless (= index width)
              (push! (list index '中文 'λ (make-string 64 #\λ) "line\r\nvalue"))
              (loop (fx+ index 1))))))
    `((schema . ,+schema+) (payload . ,payload))))

;; Setup creates the content using the original text specification. Timed warm
;; publication verifies that file; both loaders admit the complete expected datum.
;; File and image equality are checked again after all timed work.
(def (call-with-artifact-control width proc)
  (let* ((value (artifact-value width))
         (text (call-with-output-string (lambda (port) (write value port))))
         (digest (sha256-text text))
         (locator (list (compiled-language-artifact-relative-path digest) digest))
         (compressed (compress (string->utf8 text) compression: 9))
         (image (pack-language-artifact-image compressed))
         (root (make-temporary-file-name "gerbil-parser-artifact-benchmark"))
         (path (path-expand (car locator) root)))
    (try
     (create-directory* (path-directory path))
     (call-with-output-file path
       (lambda (port) (write-subu8vector compressed 0 (u8vector-length compressed) port)))
     (let (result (proc value locator image root))
       (unless (equal? (call-with-input-file path read-all-as-u8vector) compressed)
         (error "artifact benchmark modified immutable input"))
       result)
     (finally (when (file-exists? root) (delete-file-or-directory root #t))))))

(def (warm-roundtrip value root publish load)
  (let (locator (publish value (list root)))
    (list locator (load +schema+ locator (list root)))))

(def (benchmark-language-artifact (samples 20) (calls 5) (width 512)
                                 (publish materialize-compiled-language-artifact/output-dirs)
                                 (load load-compiled-language-artifact/roots)
                                 (embedded load-compiled-language-artifact/embedded))
  (call-with-artifact-control width
    (lambda (value locator image root)
      (let ((roundtrip (lambda () (warm-roundtrip value root publish load)))
            (admit (lambda () (embedded +schema+ locator image))))
        (measure-parser-component (list 'artifact-warm-roundtrip width) samples calls
                                  roundtrip (list locator value))
        (measure-parser-component (list 'artifact-embedded-admission width) samples calls
                                  admit value)
        (displayln "LANGUAGE-ARTIFACT-BENCHMARK-OK")))))

;;; Original owners are experiment inputs, never production fallback routes.
(def (compare-language-artifact original-publish original-load original-embedded
                               (groups 20) (calls 20) (width 512))
  (call-with-artifact-control width
    (lambda (value locator image root)
      (let ((roundtrip-ratios
             (measure-parser-cpu-pairs 'language-artifact (list 'warm-roundtrip width)
               groups calls (list locator value)
               (lambda () (warm-roundtrip value root original-publish original-load))
               (lambda () (warm-roundtrip value root materialize-compiled-language-artifact/output-dirs
                                         load-compiled-language-artifact/roots))))
            (admission-ratios
             (measure-parser-cpu-pairs 'language-artifact (list 'embedded-admission width)
               groups calls value
               (lambda () (original-embedded +schema+ locator image))
               (lambda () (load-compiled-language-artifact/embedded +schema+ locator image)))))
        (write (list 'LANGUAGE-ARTIFACT-CPU-RATIOS width roundtrip-ratios admission-ratios))
        (newline) (force-output)
        (displayln "LANGUAGE-ARTIFACT-COMPARISON-OK")))))

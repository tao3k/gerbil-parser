;;; Complete C ABI calls: input ownership, strict admission, parsing, GPA1
;;; publication, C result ownership and readback all remain inside timing.
(import (only-in :gerbil-parser/src/runtime/lr-parser/prepare make-lr-action-selector))
(import (only-in :std/vector/u8vector little u8vector-u32-ref u8vector-u32-set! u8vector-u64-set!)
        (only-in :gerbil-parser/t/fixtures/shared-scanner/records records-language-grammar records-contextual-product)
        (only-in :gerbil-parser/src/ffi/language-artifact-codec bind-language-abi abi-parse-binary-payload)
        (only-in :gerbil-parser/src/ffi/language-abi-context language-abi-context-source-admission-set! language-abi-context-event-row-writer-set!)
        (only-in :gerbil-parser/src/ffi/language-handles register-language-context! release-language!)
        (only-in :gerbil-parser/t/benchmarks/source-edits/abi-call parse-source/abi!)
        (only-in :gerbil-parser/t/benchmarks/ranked-literal/benchmark ranked-cpu-admission))
(import (only-in :gerbil-parser/t/fixtures/contextual-result-selection install-reference-contextual-results!)
        (only-in :gerbil-parser/t/fixtures/lr-action-selection reference-lr-action-selector)
        (only-in :gerbil-parser/src/compiler/machine parser-machine-with-lr-action-selector parser-machine-runtime
                 parser-machine-grammar-digest install-parser-machine-direct-step!)
        (only-in :gerbil-parser/src/runtime/lr-parser lr-runtime-direct-step)
        (only-in :gerbil-parser/src/language/descriptor make-language-grammar
                 +language-grammar-schema+ language-grammar-language language-grammar-version
                 language-grammar-contract language-grammar-grammar language-grammar-ir
                 language-grammar-machine language-grammar-observability))
(export benchmark-abi-utf8-cpu qualify-abi-utf8-cpu qualify-abi-utf8-regression qualify-abi-event-cpu)
(def (records-with-selector factory)
  (let* ((base (language-grammar-machine records-language-grammar))
         (machine (parser-machine-with-lr-action-selector base factory))
         (step (lr-runtime-direct-step (parser-machine-runtime base))))
    ;; Both controls retain the identical generated reduction step, when present.
    (when step
      (install-parser-machine-direct-step! machine (parser-machine-grammar-digest base) step))
    (make-language-grammar +language-grammar-schema+
      (language-grammar-language records-language-grammar)
      (language-grammar-version records-language-grammar)
      (language-grammar-contract records-language-grammar)
      (language-grammar-grammar records-language-grammar)
      (language-grammar-ir records-language-grammar) machine
      (language-grammar-observability records-language-grammar))))

;;; The former admission primitive is an independent control, not a runtime mode.
(def (reference-source-admission bytes)
  (let (source (utf8->string bytes))
    (unless (equal? bytes (string->utf8 source))
      (error "native source is not canonical UTF-8"))
    source))

;;; Independent former complete-row encoding, retained only by the benchmark.
(def (reference-event-row! payload offset tag index id start end)
  (u8vector-u32-set! payload offset tag little)
  (u8vector-u32-set! payload (+ offset 4) index little)
  (u8vector-u64-set! payload (+ offset 8) id little)
  (u8vector-u32-set! payload (+ offset 16) start little)
  (u8vector-u32-set! payload (+ offset 20) end little))
(def (benchmark-abi-utf8-cpu (groups 20) (calls 128) (lines 32) (unicode? #f) (shape 'records) (events? #f) (engine 'none))
  (unless (and (exact-integer? lines) (positive? lines))
    (error "ABI workload requires positive exact record count"))
  (let* ((source (case shape
                   ((records) (apply string-append (make-list lines (if unicode? "α=1\n" "a=1\n"))))
                   ((identifier) (string-append (make-string (* 32 lines) (if unicode? #\α #\a)) "=1\n"))
                   ((padding) (string-append "a=" (make-string (* 32 lines) #\space) "1\n"))
                   (else (error "unknown complete ABI source shape" shape))))
         (bytes (string->utf8 source))
         (control-grammar (if (memq engine '(lr combined))
                            (records-with-selector reference-lr-action-selector)
                            records-language-grammar))
         (candidate-grammar (if (memq engine '(lr combined))
                              (records-with-selector
                               make-lr-action-selector)
                              records-language-grammar))
         (original (bind-language-abi control-grammar records-contextual-product))
         (candidate (bind-language-abi candidate-grammar records-contextual-product))
         (expected (abi-parse-binary-payload candidate source))
         (left-output (make-u8vector (u8vector-length expected)))
         (right-output (make-u8vector (u8vector-length expected))))
    (unless (zero? (u8vector-u32-ref expected 8 little))
      (error "complete ABI CPU workload must be accepted" shape))
    (when (memq engine '(scanner combined)) (install-reference-contextual-results! original))
    (if events?
      (language-abi-context-event-row-writer-set! original reference-event-row!)
      (when (eq? engine 'none)
        (language-abi-context-source-admission-set! original reference-source-admission)))
    (let ((left (register-language-context! original))
          (right (register-language-context! candidate)))
      (dynamic-wind void
        (lambda ()
          ;; Validate accepted, rejected, empty and multibyte complete products
          ;; outside timing, through both independent public C ABI handles.
          (when (or events? (not (eq? engine 'none)))
            (for-each
             (lambda (text)
               (let* ((input (string->utf8 text))
                      (wanted (abi-parse-binary-payload candidate text))
                      (a (make-u8vector (u8vector-length wanted)))
                      (b (make-u8vector (u8vector-length wanted))))
                 (unless (and (zero? (parse-source/abi! left input a))
                              (zero? (parse-source/abi! right input b))
                              (equal? a wanted) (equal? b wanted))
                   (error "complete event-row reference parity failed" text))))
             '("" "a=1\n" "α=1\r\n" "name=\"猫\"\n" "a=1 b=1\n" "a=\n" "?")))
          (def (call handle output)
            (unless (zero? (parse-source/abi! handle bytes output))
              (error "complete ABI request failed"))
            output)
          (write (list 'ABI-UTF8-WORKLOAD 'source-bytes (u8vector-length bytes)
                       'payload-bytes (u8vector-length expected) 'records (if (eq? shape 'records) lines 1) 'units lines 'unicode unicode? 'shape shape))
          (newline) (force-output)
          (ranked-cpu-admission (list (cond ((not (eq? engine 'none)) (list 'complete-c-abi-engine engine)) (events? 'complete-c-abi-event-rows) (else 'complete-c-abi-utf8)) shape lines unicode?) groups calls expected
            (lambda () (call left left-output)) (lambda () (call right right-output))))
        (lambda () (release-language! left) (release-language! right))))))
(def (qualify-abi-utf8-cpu (groups 20) (calls 128) (lines 32) (unicode? #f) (shape 'records))
  (let (summary (benchmark-abi-utf8-cpu groups calls lines unicode? shape))
    (unless (cdr (assq 'cpu-benefit (cdr summary)))
      (error "complete ABI UTF-8 CPU controls or benefit gate failed" summary))
    (displayln "ABI-UTF8-CPU-PROOF-OK") (force-output)
    summary))

;;; A many-record request is a separate no-regression obligation. It must not
;;; be relabeled a speedup when strict CPU benefit admission rejects it.
(def (qualify-abi-utf8-regression (groups 20) (calls 256) (lines 64) (unicode? #f) (shape 'records))
  (let* ((summary (benchmark-abi-utf8-cpu groups calls lines unicode? shape))
         (rows (cdr summary))
         (stable? (lambda (values) (and (>= (car values) 0.90) (<= (caddr values) 1.10))))
         (before (cdr (assq 'before rows))) (after (cdr (assq 'after rows)))
         (production (cdr (assq 'production rows))))
    (unless (and (stable? before) (stable? after) (<= (caddr production) 1.05))
      (error "complete ABI request regression or control gate failed" summary))
    (displayln "ABI-UTF8-CPU-REGRESSION-OK") (force-output)
    summary))

;;; Both contexts use the same upstream decoder and full parser; only the
;;; publication primitive differs. Every timed call produces the entire GPA1.
(def (qualify-abi-event-cpu (groups 20) (calls 256) (lines 64) (unicode? #f) (shape 'records))
  (let (summary (benchmark-abi-utf8-cpu groups calls lines unicode? shape #t))
    (unless (cdr (assq 'cpu-benefit (cdr summary)))
      (error "complete ABI event CPU controls or benefit gate failed" summary))
    (displayln "ABI-EVENT-CPU-PROOF-OK") (force-output)
    summary))

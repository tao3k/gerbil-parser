;;; Native engine frame costs, not an Org grammar or end-to-end latency gate.
(import (only-in :gerbil-parser/src/runtime/source-lines
                 fold-source-lines fold-source-byte-lines fold-source-byte-spans)
        (only-in :gerbil-parser/src/runtime/event-fold-lines
                 current-fold-line-view fold-line-prefix-boundary?)
        (only-in :gerbil-parser/src/runtime/parse-cost admit-parser-allocation))
(import (only-in :std/string/utf8 utf8->string))
(export main)

(def prefixes
  (map (lambda (index) (string-append "#+key" (number->string index) ":")) (iota 32)))
(def encoded-prefixes (map string->utf8 prefixes))

;; Frozen pre-fixnum algorithm, benchmark-only; never a production fallback.
(def (reference-byte-lines bytes initial visit)
  (let (size (u8vector-length bytes))
    (def (emit from until state)
      (let (view (subu8vector bytes from until))
        (visit (utf8->string view) view from until state)))
    (let loop ((start 0) (cursor 0) (state initial))
      (cond
       ((= cursor size) (if (= start size) state (emit start size state)))
       ((memv (u8vector-ref bytes cursor) '(10 13))
        (let (after (+ cursor
                       (if (and (= (u8vector-ref bytes cursor) 13)
                                (< (+ cursor 1) size)
                                (= (u8vector-ref bytes (+ cursor 1)) 10))
                         2 1)))
          (loop after after (emit start after state))))
       (else (loop start (+ cursor 1) state))))))

(def (mask line bytes explicit? (from 0) (until (and bytes (u8vector-length bytes))))
  (let loop ((names prefixes) (encoded encoded-prefixes) (bit 1) (flags 0))
    (if (null? names) flags
      (loop (cdr names) (cdr encoded) (* bit 2)
            (if (if explicit?
                  (fold-line-prefix-boundary? line (car names) #f bytes (car encoded) from until)
                  (fold-line-prefix-boundary? line (car names) #f))
              (+ flags bit) flags)))))

(def (parse text mode)
  ;; Both generated routes encode the whole source for source-bounded helpers.
  (let (source (string->utf8 text))
    (unless (>= (u8vector-length source) (string-length text))
      (error "invalid native frame source extent"))
    (def (visit line bytes start end events)
      (cons (list 'line start end (mask line bytes #t)) events))
    (reverse!
     (case mode
       ((borrowed-byte-spans)
        (fold-source-byte-spans source '()
          (lambda (start end events)
            (cons (list 'line start end (mask #f source #t start end)) events))))
       ((byte-frames) (fold-source-byte-lines source '() visit))
       ((generic-byte-frames) (reference-byte-lines source '() visit))
       (else (fold-source-lines text '()
         (lambda (line start end events)
           (let (bytes (string->utf8 line))
             (if (eq? mode 'explicit-string-frames)
               (visit line bytes start end events)
               (parameterize ((current-fold-line-view (cons line bytes)))
                 (cons (list 'line start end (mask line #f #f)) events)))))))))))

(def (document index)
  (string-append "#+KEY1: value " (number->string index) "\r\n"
                 "#+key31: λ中🦀\n#+key10:not-a-boundary\r"
                 "ordinary UTF-8 é\r\n\nunterminated"))

(def (positive-count text)
  (let (number (string->number text))
    (unless (and (exact-integer? number) (positive? number))
      (error "benchmark count must be a positive exact integer" text))
    number))

(def (measure documents mode expected)
  (let* ((before (##process-statistics)) (start (##current-time-point))
         (count (foldl (lambda (text sum) (+ sum (length (parse text mode)))) 0 documents))
         (wall (* 1000 (- (##current-time-point) start)))
         (after (##process-statistics))
         (delta (lambda (slot) (- (f64vector-ref after slot) (f64vector-ref before slot)))))
    (unless (= count expected) (error "native frame benchmark lost events" mode count expected))
    (list 'wall-ms wall 'cpu-ms (* 1000 (+ (delta 0) (delta 1)))
          'gc-count (delta 6) 'allocation-counter-delta (delta 7)
          'allocated-bytes (admit-parser-allocation (delta 7) (delta 6)))))

(def (main (samples-text "7") . sizes-text)
  (let ((samples (positive-count samples-text))
        (sizes (map positive-count (if (null? sizes-text) '("1000" "2000" "10000") sizes-text))))
    (for-each
     (lambda (size)
       (let* ((documents (map document (iota size)))
              (expected (* size 6)))
         ;; Check every document outside timing. No shrinking or hit-only view.
         (for-each
          (lambda (text)
            (let (reference (parse text 'dynamic-string-frames))
              (for-each
               (lambda (mode)
                 (unless (equal? reference (parse text mode))
                   (error "native frame benchmark semantic mismatch" mode text)))
               '(explicit-string-frames generic-byte-frames byte-frames borrowed-byte-spans))))
          documents)
         (displayln "FRAME-PARITY-OK documents=" size) (force-output)
         (let sample ((index 0))
           (when (< index samples)
             ;; Rotate order so one route is not always the first cold sample.
             (let* ((modes '(dynamic-string-frames explicit-string-frames generic-byte-frames
                            byte-frames borrowed-byte-spans))
                    (offset (modulo index (length modes))))
               (for-each
                (lambda (mode)
                  (write (cons 'event-fold-frames
                               (append (list 'documents size 'sample index 'mode mode)
                                       (measure documents mode expected))))
                  (newline) (force-output))
                (append (drop modes offset) (take modes offset))))
             (sample (+ index 1))))))
     sizes))
  (displayln "FRAME-BENCHMARK-OK") (force-output))

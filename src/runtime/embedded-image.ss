;;; -*- Gerbil -*-
;;; Native constant storage for compressed language images; byte identity is unchanged.
(import (only-in :std/io open-buffered-reader BufferedReader-read-u64 BufferedReader-read-u8
                        open-buffered-writer BufferedWriter-write-u64 BufferedWriter-flush))
(export pack-language-artifact-image unpack-language-artifact-image)

;;; The first unsigned word is byte length; remaining words use std/io's network order.
;;; Final zero padding is storage framing and never enters the compressed stream.
(def (pack-language-artifact-image bytes)
  (unless (u8vector? bytes) (error "language artifact image requires bytes"))
  (let* ((size (u8vector-length bytes))
         (full (quotient size 8))
         (tail (modulo size 8))
         (count (+ full (if (zero? tail) 0 1)))
         (words (make-u64vector (+ count 1) 0)))
    (u64vector-set! words 0 size)
    ;; Read complete words from the original input. Only the final partial word
    ;; needs padding; never allocate/copy a padded image of the entire payload.
    (call-with-input-u8vector bytes
      (lambda (port)
        (let (reader (open-buffered-reader port))
          (let loop ((index 1))
            (when (<= index full)
              (u64vector-set! words index (BufferedReader-read-u64 reader))
              (loop (fx+ index 1))))
          (unless (zero? tail)
            (let loop ((remaining tail) (word 0))
              (if (zero? remaining)
                (u64vector-set! words count (arithmetic-shift word (* 8 (- 8 tail))))
                (loop (fx- remaining 1)
                      (bitwise-ior (arithmetic-shift word 8) (BufferedReader-read-u8 reader)))))))))
    words))

(def (unpack-language-artifact-image words)
  (unless (and (u64vector? words) (> (u64vector-length words) 0))
    (error "invalid native language artifact image"))
  (let ((size (u64vector-ref words 0)) (count (u64vector-length words)))
    ;; Validate size before allocation; a hostile length cannot request a larger buffer.
    (unless (= count (+ 1 (quotient (+ size 7) 8)))
      (error "invalid native language artifact image length" size count))
    (let (bytes
           (call-with-output-u8vector
            (lambda (port)
              (let (writer (open-buffered-writer port))
                (let loop ((index 1))
                  (when (< index count)
                    (BufferedWriter-write-u64 writer (u64vector-ref words index))
                    (loop (fx+ index 1))))
                (BufferedWriter-flush writer)))))
      (let loop ((index size))
        (when (< index (u8vector-length bytes))
          (unless (zero? (u8vector-ref bytes index))
            (error "invalid native language artifact image padding"))
          (loop (fx+ index 1))))
      (subu8vector bytes 0 size))))

;;; -*- Gerbil -*-
;;; Stable SHA-256 text identities shared by generated machines and artifacts.

(import :std/crypto/digest)
(export sha256-text)

(def +hex-digits+ "0123456789abcdef")

(def (sha256-text text)
  (let ((output (make-string 71 #\0))
        (bytes (sha256 text)))
    (let prefix-loop ((index 0))
      (when (< index 7)
        (string-set! output index
                     (string-ref "sha256:" index))
        (prefix-loop (+ index 1))))
    (let byte-loop ((index 0))
      (when (< index (u8vector-length bytes))
        (let* ((byte (u8vector-ref bytes index))
               (offset (+ 7 (* index 2))))
          (string-set! output offset
                       (string-ref +hex-digits+ (quotient byte 16)))
          (string-set! output (+ offset 1)
                       (string-ref +hex-digits+ (modulo byte 16)))
          (byte-loop (+ index 1)))))
    output))

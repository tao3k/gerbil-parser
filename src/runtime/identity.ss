;;; -*- Gerbil -*-
;;; Stable SHA-256 text identities shared by generated machines and artifacts.

(import (only-in :std/crypto/digest sha256))
(export sha256-text)

(def +hex-digits+ "0123456789abcdef")

;; sha256-text
;;   : (-> String String)
;;   | doc m%
;;       `sha256-text` returns the canonical prefixed digest for source identity.
;;
;;       # Examples
;;
;;       ```scheme
;;       (sha256-text "")
;;       ;; => "sha256:e3b0c442..."
;;       ```
;;     %
(def (sha256-text text)
  (let ((output (make-string 71 #\0))
        (bytes (sha256 (string->utf8 text))))
    (for-each (lambda (index)
                (string-set! output index (string-ref "sha256:" index)))
              (iota 7))
    (for-each
     (lambda (index)
       (let* ((byte (u8vector-ref bytes index))
              (offset (fx+ 7 (fx* index 2))))
         (string-set! output offset
                      (string-ref +hex-digits+ (fxquotient byte 16)))
         (string-set! output (fx+ offset 1)
                      (string-ref +hex-digits+ (fxmodulo byte 16)))))
     (iota (u8vector-length bytes)))
    output))

;;; -*- Gerbil -*-
;;; Stable SHA-256 text identities shared by generated machines and artifacts.

(import (only-in :std/crypto/digest sha256))
(export sha256-bytes sha256-text)

(def +hex-digits+ "0123456789abcdef")

;; : (-> U8Vector String)
;; Encode one canonical prefixed digest from bytes shared by artifact fields.
(def (sha256-bytes input)
  (let ((output (make-string 71 #\0))
        (bytes (sha256 input)))
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
  (sha256-bytes (string->utf8 text)))

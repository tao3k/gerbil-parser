;;; -*- Gerbil -*-
;;; Request-local preparation of admitted byte-predicate lookup indices.
(export current-fold-byte-set-cache fold-byte-set-for fold-marker-byte)

;; Shared scalar admission must not import the interpreter/runtime closure.
(def (fold-marker-byte value)
  (unless (and (string? value) (= (string-length value) 1)
               (< (char->integer (string-ref value 0)) 128))
    (error "event fold marker must be one ASCII byte" value))
  (char->integer (string-ref value 0)))

(def current-fold-byte-set-cache (make-parameter #f))

(def (fold-byte-set-for values expression)
  (let* ((cache (current-fold-byte-set-cache))
         (prepared (and cache (table-ref cache values #f))))
    ;; The same request owner also holds ordinary vectors for ASCII name
    ;; indices. A uniform byte vector is the scalar-byte admission witness;
    ;; a name-index hit must never admit an invalid byte operand.
    (or (and (u8vector? prepared) prepared)
        (begin
          (unless (list? values)
            (error "invalid event fold byte-set predicate" expression))
          (let (table (make-u8vector 256 0))
            (for-each
             (lambda (byte)
               (unless (and (exact-integer? byte) (<= 0 byte 255))
                 (error "invalid event fold byte-set predicate" expression))
               (u8vector-set! table byte 1))
             values)
            (when cache (table-set! cache values table))
            table)))))

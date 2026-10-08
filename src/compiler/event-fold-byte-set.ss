;;; -*- Gerbil -*-
;;; Request-local preparation of immutable grammar byte sets.
(export current-fold-byte-set-cache fold-byte-set-for)

(def current-fold-byte-set-cache (make-parameter #f))

(def (fold-byte-set-for values expression)
  (let* ((cache (current-fold-byte-set-cache))
         (prepared (and cache (table-ref cache values #f))))
    (or prepared
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

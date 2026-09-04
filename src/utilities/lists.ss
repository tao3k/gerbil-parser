;;; Small general-purpose list primitives shared across compiler modules.

(export append-unique
        merge-keyed-row)

(def (append-unique values value)
  (if (member value values) values (append values (list value))))

(def (merge-keyed-row rows row row-key conflict)
  (let* ((key (row-key row))
         (existing
          (let loop ((rest rows))
            (and (pair? rest)
                 (if (equal? (row-key (car rest)) key)
                   (car rest)
                   (loop (cdr rest)))))))
    (cond
     ((not existing) (append rows (list row)))
     ((equal? existing row) rows)
     (else (conflict key)))))

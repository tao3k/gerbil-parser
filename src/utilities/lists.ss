;;; Small general-purpose list primitives shared across compiler modules.

(export append-unique
        merge-keyed-row)

;; append-unique
;; : (forall (a) (-> [a] a [a]))
;; : (-> List Datum List)
(def (append-unique values value)
  (if (member value values)
    values
    (foldr cons (list value) values)))

;; merge-keyed-row
;;   : (forall (a k) (-> [a] a Procedure Procedure [a]))
;;   : (-> List Datum Procedure Procedure List)
;;   | doc m%
;;       `merge-keyed-row` inserts one keyed row or delegates duplicate handling.
;;
;;       # Examples
;;
;;       ```scheme
;;       (merge-keyed-row '() '(name value) car error)
;;       ;; => ((name value))
;;       ```
;;     %
(def (merge-keyed-row rows row row-key conflict)
  (let* ((key (row-key row))
         (existing (find (lambda (value) (equal? (row-key value) key)) rows)))
    (cond
     ((not existing) (foldr cons (list row) rows))
     ((equal? existing row) rows)
     (else (conflict key)))))

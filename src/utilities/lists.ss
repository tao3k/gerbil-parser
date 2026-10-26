;;; Small general-purpose list primitives shared across compiler modules.

(import (only-in :clan/poo/object .o)
        (only-in :clan/poo/mop validate)
        (only-in ./types ListOperationsScopeOf))

(export make-list-operations append-unique
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

;; Explicit caller Types own row/key admission; the native pair owns the result.
(def (make-list-operations Row Key)
  (let ((type (ListOperationsScopeOf Row Key))
        (append-method append-unique) (merge-method merge-keyed-row))
    (cons type (validate type
      (.o append-unique: append-method merge-keyed-row: merge-method)))))

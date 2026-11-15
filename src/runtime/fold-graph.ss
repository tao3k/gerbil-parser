;;; An Element projection consumes the same effects as the navigation index.
(import (only-in ./fold-index fold-index-owner make-fold-index fold-index-start! fold-index-token!
                 fold-index-finish! fold-index-publish! fold-index-position fold-index-open))
(export make-fold-graph fold-graph-start! fold-graph-token!
        fold-graph-finish! fold-graph-publish!)

;; These private owners retain their types throughout one Fold. Let Gerbil
;; prove field access once at entry instead of rechecking every state read.
(defstruct fold-element
  ((id :- :fixnum) (rule :- :vector) parent (row :- :vector) first-fields)
  final: #t)
(defstruct fold-graph-state
  ((index :- fold-index-owner) (rules :- :vector) (count :- :fixnum)
   active (records :- :list) select?)
  final: #t)

(def (make-fold-graph bytes digest (rules : :vector) (navigation? #t) (select? #f))
  => fold-graph-state
  (make-fold-graph-state (make-fold-index bytes digest navigation?) rules 0 #f '() select?))

(def (active-id frame)
  (if (pair? frame) (car frame) (and frame (fold-element-id frame))))

(def (fold-graph-start! (sink : fold-graph-state) kind)
  (let* ((index (fold-graph-state-index sink))
         (rule (vector-ref (fold-graph-state-rules sink) kind))
         (parent (fold-graph-state-active sink))
         (select? (fold-graph-state-select? sink)))
    (when rule
      (if (or (not select?)
              (select? (vector-ref rule 0)
                       (and (fold-element? parent) (vector-ref (fold-element-rule parent) 0))
                       (fold-index-position index)))
        (let* ((id (fold-graph-state-count sink))
               (row (vector (vector-ref rule 0) (active-id parent)
                            (fold-index-position index) 0 0 '())))
          (fold-graph-state-count-set! sink (+ id 1))
          (fold-graph-state-records-set! sink (cons row (fold-graph-state-records sink)))
          (fold-graph-state-active-set! sink (make-fold-element id rule parent row #f)))
        ;; Keep the skipped rule on the stack so it cannot leak fields to a parent.
        (fold-graph-state-active-set! sink (cons (active-id parent) parent))))
    (fold-index-start! index kind))
  sink)

(def (capture! (owner : fold-element) field start end)
  (let* ((group (vector-ref field 1))
         (separate? (vector-ref field 2))
         (firsts (or (fold-element-first-fields owner)
                     (let (storage (make-vector (vector-ref (fold-element-rule owner) 3) #f))
                       (fold-element-first-fields-set! owner storage)
                       storage)))
         (first (vector-ref firsts group)))
    (if (and first (not separate?))
      ;; Append observes concatenated text, not token boundaries. Extend our
      ;; private last span when contiguous; each fields retain their own rows.
      (let (last (car (cdr first)))
        (if (= (cdr last) start)
          (set-cdr! last end)
          (set-cdr! first (cons (cons start end) (cdr first)))))
      (let ((value (cons (vector-ref field 0) (list (cons start end))))
            (row (fold-element-row owner)))
        (unless first (vector-set! firsts group value))
        (vector-set! row 5 (cons value (vector-ref row 5)))))))

(def (capture-kind! (owner : fold-element) kind start end)
  (let loop ((fields (vector-ref (vector-ref (fold-element-rule owner) 1) kind)))
    (unless (null? fields)
      (capture! owner (car fields) start end)
      (loop (cdr fields)))))

(def (fold-graph-token! (sink : fold-graph-state) kind from until)
  (fold-index-token! (fold-graph-state-index sink) kind from until)
  (let (active (fold-graph-state-active sink))
    (when (fold-element? active) (capture-kind! active kind from until)))
  sink)

(def (fold-graph-finish! (sink : fold-graph-state))
  (let ((index (fold-graph-state-index sink)) (active (fold-graph-state-active sink)))
    (when active
      (fold-index-open index
        (lambda (kind start)
          (let* ((end (fold-index-position index))
                 (mapped? (vector-ref (fold-graph-state-rules sink) kind)))
            (if (fold-element? active)
              (begin
                (capture-kind! active kind start end)
                (when mapped?
                  (let loop ((fields (vector-ref (fold-element-rule active) 2)))
                    (unless (null? fields)
                      (let ((firsts (fold-element-first-fields active)) (field (car fields)))
                        (unless (and firsts (vector-ref firsts (vector-ref field 1)))
                          (capture! active field end end)))
                      (loop (cdr fields))))
                  (let* ((row (fold-element-row active))
                         (fields (reverse! (vector-ref row 5))))
                    (for-each (lambda (field) (set-cdr! field (reverse! (cdr field)))) fields)
                    (vector-set! row 3 end)
                    (vector-set! row 4 (fold-graph-state-count sink))
                    (vector-set! row 5 fields))
                  (fold-graph-state-active-set! sink (fold-element-parent active))))
              (when mapped? (fold-graph-state-active-set! sink (cdr active))))))))
    (fold-index-finish! index))
  sink)

(def (fold-graph-publish! (sink : fold-graph-state))
  (let* ((index (fold-index-publish! (fold-graph-state-index sink)))
         (records (reverse! (fold-graph-state-records sink))))
    (fold-graph-state-records-set! sink '())
    (vector (vector-ref index 0) (vector-ref index 1) (list->vector records))))

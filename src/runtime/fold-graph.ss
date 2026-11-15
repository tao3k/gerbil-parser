;;; An Element projection consumes the same effects as the navigation index.
(import (only-in ./fold-index fold-index-owner make-fold-index fold-index-start! fold-index-token!
                 fold-index-finish! fold-index-publish! fold-index-position fold-index-open))
(export make-fold-graph fold-graph-start! fold-graph-token!
        fold-graph-finish! fold-graph-publish!)

;; An unpublished row is its own open frame: #(rule parent start id firsts fields).
;; Closing replaces the private slots with the public Element layout. Selectors
;; receive only kind, parent kind and position; no open row escapes.
(defstruct fold-graph-state
  ((index :- fold-index-owner) (rules :- :vector) (count :- :fixnum)
   active (records :- :list) select?)
  final: #t)

(def (make-fold-graph bytes digest (rules : :vector) (navigation? #t) (select? #f))
  => fold-graph-state
  (make-fold-graph-state (make-fold-index bytes digest navigation?) rules 0 #f '() select?))

(def (active-id frame)
  (if (pair? frame) (car frame) (and frame (vector-ref frame 3))))

(def (fold-graph-start! (sink : fold-graph-state) kind)
  (let* ((index (fold-graph-state-index sink))
         (rule (vector-ref (fold-graph-state-rules sink) kind))
         (parent (fold-graph-state-active sink))
         (select? (fold-graph-state-select? sink)))
    (when rule
      (if (or (not select?)
              (select? (vector-ref rule 0)
                       (and (vector? parent) (vector-ref (vector-ref parent 0) 0))
                       (fold-index-position index)))
        (let* ((id (fold-graph-state-count sink))
               (row (vector rule parent (fold-index-position index) id #f '())))
          (fold-graph-state-count-set! sink (+ id 1))
          (fold-graph-state-records-set! sink (cons row (fold-graph-state-records sink)))
          (fold-graph-state-active-set! sink row))
        ;; Keep the skipped rule on the stack so it cannot leak fields to a parent.
        (fold-graph-state-active-set! sink (cons (active-id parent) parent))))
    (fold-index-start! index kind))
  sink)

(def (capture! (owner : :vector) field start end)
  (let* ((group (vector-ref field 1))
         (separate? (vector-ref field 2))
         (firsts (or (vector-ref owner 4)
                     (let (storage (make-vector (vector-ref (vector-ref owner 0) 3) #f))
                       (vector-set! owner 4 storage)
                       storage)))
         (first (vector-ref firsts group)))
    (if (and first (not separate?))
      ;; Append observes concatenated text, not token boundaries. Extend our
      ;; private last span when contiguous; each fields retain their own rows.
      (let (last (car (cdr first)))
        (if (= (cdr last) start)
          (set-cdr! last end)
          (set-cdr! first (cons (cons start end) (cdr first)))))
      (let (value (cons (vector-ref field 0) (list (cons start end))))
        (unless first (vector-set! firsts group value))
        (vector-set! owner 5 (cons value (vector-ref owner 5)))))))

(def (capture-kind! (owner : :vector) kind start end)
  (let loop ((fields (vector-ref (vector-ref (vector-ref owner 0) 1) kind)))
    (unless (null? fields)
      (capture! owner (car fields) start end)
      (loop (cdr fields)))))

(def (fold-graph-token! (sink : fold-graph-state) kind from until)
  (fold-index-token! (fold-graph-state-index sink) kind from until)
  (let (active (fold-graph-state-active sink))
    (when (vector? active) (capture-kind! active kind from until)))
  sink)

(def (fold-graph-finish! (sink : fold-graph-state))
  (let ((index (fold-graph-state-index sink)) (active (fold-graph-state-active sink)))
    (when active
      (fold-index-open index
        (lambda (kind start)
          (let* ((end (fold-index-position index))
                 (mapped? (vector-ref (fold-graph-state-rules sink) kind)))
            (if (vector? active)
              (begin
                (capture-kind! active kind start end)
                (when mapped?
                  (let loop ((fields (vector-ref (vector-ref active 0) 2)))
                    (unless (null? fields)
                      (let ((firsts (vector-ref active 4)) (field (car fields)))
                        (unless (and firsts (vector-ref firsts (vector-ref field 1)))
                          (capture! active field end end)))
                      (loop (cdr fields))))
                  (let* ((rule (vector-ref active 0))
                         (parent (vector-ref active 1))
                         (fields (reverse! (vector-ref active 5))))
                    (for-each (lambda (field) (set-cdr! field (reverse! (cdr field)))) fields)
                    (vector-set! active 0 (vector-ref rule 0))
                    (vector-set! active 1 (active-id parent))
                    (vector-set! active 3 end)
                    (vector-set! active 4 (fold-graph-state-count sink))
                    (vector-set! active 5 fields)
                    (fold-graph-state-active-set! sink parent))))
              (when mapped? (fold-graph-state-active-set! sink (cdr active))))))))
    (fold-index-finish! index))
  sink)

(def (fold-graph-publish! (sink : fold-graph-state))
  (let* ((index (fold-index-publish! (fold-graph-state-index sink)))
         (records (reverse! (fold-graph-state-records sink))))
    (fold-graph-state-records-set! sink '())
    (vector (vector-ref index 0) (vector-ref index 1) (list->vector records))))

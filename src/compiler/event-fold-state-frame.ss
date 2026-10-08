;;; -*- Gerbil -*-
;;; Private request-owned execution slots; never escape as parser results.
(export prepare-fold-state-frame fold-state-frame?
        fold-frame-ref fold-frame-update! fold-frame-bound?
        fold-frame-bind! fold-frame-unbind!)

(defstruct fold-state-frame (slots) final: #t)

(def (prepare-fold-state-frame bindings)
  (let (slots (make-table test: eq?))
    ;; First binding owns reads. Updating its cell represents updating every
    ;; duplicate name, since binding enumeration never leaves this frame.
    (for-each (lambda (entry)
                (unless (table-ref slots (car entry) #f)
                  (table-set! slots (car entry) (vector (cdr entry)))))
              bindings)
    (make-fold-state-frame slots)))

(def (fold-frame-ref frame name)
  (let (cell (table-ref (fold-state-frame-slots frame) name #f))
    (unless cell (error "undeclared event fold state" name))
    (vector-ref cell 0)))

(def (fold-frame-bound? frame name)
  (and (table-ref (fold-state-frame-slots frame) name #f) #t))

(def (fold-frame-update! frame name value)
  (let (cell (table-ref (fold-state-frame-slots frame) name #f))
    (when cell (vector-set! cell 0 value)))
  frame)

(def (fold-frame-bind! frame name value)
  (when (fold-frame-bound? frame name)
    (error "event fold temporary state already bound" name))
  (table-set! (fold-state-frame-slots frame) name (vector value))
  frame)

(def (fold-frame-unbind! frame name)
  (table-set! (fold-state-frame-slots frame) name)
  frame)

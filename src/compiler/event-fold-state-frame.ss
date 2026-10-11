;;; -*- Gerbil -*-
;;; Private request-owned execution slots; never escape as parser results.
(export prepare-fold-state-layout instantiate-fold-state-frame
        prepare-fold-state-frame fold-state-frame?
        fold-frame-ref fold-frame-update! fold-frame-bound?
        fold-frame-bind! fold-frame-unbind!)

(defstruct fold-state-layout (index defaults) final: #t)
(defstruct fold-state-frame (layout values temporaries) final: #t)
(def missing-fold-slot (gensym 'missing-fold-slot))

(def (prepare-fold-state-layout bindings)
  (let ((index (make-table test: eq?)) (defaults '()) (width 0))
    ;; First binding owns reads. Updating its cell represents updating every
    ;; duplicate name, since binding enumeration never leaves this frame.
    (for-each (lambda (entry)
                (unless (or (boolean? (cdr entry)) (null? (cdr entry))
                            (and (exact-integer? (cdr entry)) (>= (cdr entry) 0)))
                  (error "mutable or untyped event fold initial value" entry))
                (unless (table-ref index (car entry) #f)
                  (table-set! index (car entry) width)
                  (set! defaults (cons (cdr entry) defaults))
                  (set! width (+ width 1))))
              bindings)
    (make-fold-state-layout index (list->vector (reverse defaults)))))

(def (instantiate-fold-state-frame layout)
  (make-fold-state-frame layout (vector-copy (fold-state-layout-defaults layout)) #f))

(def (prepare-fold-state-frame bindings)
  (instantiate-fold-state-frame (prepare-fold-state-layout bindings)))

(def (fold-frame-slot frame name)
  (table-ref (fold-state-layout-index (fold-state-frame-layout frame)) name #f))

(def (fold-frame-value frame name)
  (let (slot (fold-frame-slot frame name))
    (if slot (vector-ref (fold-state-frame-values frame) slot)
      (let (temporaries (fold-state-frame-temporaries frame))
        (if temporaries (table-ref temporaries name missing-fold-slot)
          missing-fold-slot)))))

(def (fold-frame-ref frame name)
  (let (value (fold-frame-value frame name))
    (when (eq? value missing-fold-slot) (error "undeclared event fold state" name))
    value))

(def (fold-frame-bound? frame name)
  (not (eq? (fold-frame-value frame name) missing-fold-slot)))

(def (fold-frame-update! frame name value)
  (let (slot (fold-frame-slot frame name))
    (if slot
      (unless (eq? (vector-ref (fold-state-frame-values frame) slot) missing-fold-slot)
        (vector-set! (fold-state-frame-values frame) slot value))
      (let (temporaries (fold-state-frame-temporaries frame))
        (when (and temporaries
                   (not (eq? (table-ref temporaries name missing-fold-slot) missing-fold-slot)))
          (table-set! temporaries name value)))))
  frame)

(def (fold-frame-bind! frame name value)
  (when (fold-frame-bound? frame name)
    (error "event fold temporary state already bound" name))
  (let (slot (fold-frame-slot frame name))
    (if slot (vector-set! (fold-state-frame-values frame) slot value)
      (let (temporaries (or (fold-state-frame-temporaries frame)
                           (let (table (make-table test: eq?))
                             (fold-state-frame-temporaries-set! frame table)
                             table)))
        (table-set! temporaries name value))))
  frame)

(def (fold-frame-unbind! frame name)
  (let (slot (fold-frame-slot frame name))
    (if slot (vector-set! (fold-state-frame-values frame) slot missing-fold-slot)
      (let (temporaries (fold-state-frame-temporaries frame))
        (when temporaries (table-set! temporaries name)))))
  frame)

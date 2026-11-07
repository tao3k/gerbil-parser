;;; -*- Gerbil -*-
;;; Original LR field/alias actions lower directly to persistent event programs.
(import (only-in ./event-program
                 event-program-value? event-program-value-code
                 event-program-token event-program-append event-program-relocate
                 event-program-field event-program-node-value event-program-fragment-value
                 event-program-sequence? event-program-sequence-arity
                 event-program-sequence-start event-program-sequence-end
                 event-program-sequence-field?)
        (only-in ./recognition
                 recognition-child-field recognition-child-value make-recognition-child
                 recognition-value-start recognition-value-end
                 recognition-relocation? recognition-relocation-value recognition-relocation-delta
                 recognition-node? recognition-node-kind recognition-node-children
                 recognition-node-start recognition-node-end
                 recognition-fragment? recognition-fragment-children recognition-fragment-start recognition-fragment-end)
        (only-in ./funcs recognition-sequence-arity recognition-sequence-start
                 recognition-sequence-end recognition-sequence-for-each recognition-sequence->list)
        (only-in ./token token?))
(export event-children-field event-children-alias)

;;; Conversion freezes the current semantic action's field and value. It never
;;; caches bytecode by a mutable recognition child/node identity. Already lowered
;;; program values retain their private code by identity, including reused LR
;;; fragments; token identity is revalidated against the request at publication.
(def (value-code value)
  (cond
   ((event-program-value? value) (event-program-value-code value))
   ((token? value) (event-program-token value))
   ((recognition-relocation? value)
    (event-program-relocate (value-code (recognition-relocation-value value))
                            (recognition-relocation-delta value)))
   ((recognition-node? value)
    (event-program-value-code
     (event-program-node-value (recognition-node-kind value)
       (recognition-node-start value) (recognition-node-end value)
       (children-code (recognition-node-children value)))))
   ((recognition-fragment? value)
    (event-program-value-code
     (event-program-fragment-value (recognition-fragment-start value)
       (recognition-fragment-end value)
       (children-code (recognition-fragment-children value)))))
   (else (error "invalid semantic value for event lowering"))))
(def (children-code children)
  (if (event-program-sequence? children) children
  (let (code #f)
    (recognition-sequence-for-each
     (lambda (child delta moved?)
       (let* ((value (recognition-child-value child))
              (field (recognition-child-field child))
              (body (value-code value)))
         (set! code
           (event-program-append code
             (event-program-relocate
              (if field
                (event-program-field field (recognition-value-start value)
                                           (recognition-value-end value) body)
                body) delta moved?))))) children)
    code)))

(def (event-children-field name children offset (unused-fragment-constructor #f))
  (let (code (children-code children))
    (case (event-program-sequence-arity code)
      ((0) #f)
      ((1)
       (event-program-field name
         (event-program-sequence-start code offset)
         (event-program-sequence-end code offset)
         (if (event-program-sequence-field? code)
           (event-program-fragment-value
             (event-program-sequence-start code offset)
             (event-program-sequence-end code offset) code)
           code)))
      (else
       (event-program-field name
         (event-program-sequence-start code offset)
         (event-program-sequence-end code offset)
         (event-program-fragment-value
           (event-program-sequence-start code offset)
           (event-program-sequence-end code offset) code))))))
(def (event-children-alias kind children offset)
  (let (code (children-code children))
    (event-program-node-value kind
      (event-program-sequence-start code offset)
      (event-program-sequence-end code offset) code)))

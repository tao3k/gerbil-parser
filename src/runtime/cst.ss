;;; -*- Gerbil -*-
;;; Gerbil CST projection replayed from canonical ParseArtifact v1 events.

(import ./artifact
        ./token)
(export parse-artifact->cst
        syntax-node?
        syntax-node-kind
        syntax-node-start
        syntax-node-end
        syntax-node-children
        syntax-field?
        syntax-field-name
        syntax-field-start
        syntax-field-end
        syntax-field-children)

(defstruct syntax-node (kind start end children) transparent: #t)
(defstruct syntax-field (name start end children) transparent: #t)

(def (make-frame type identity kind start)
  (vector type identity kind start '()))

(def (frame-add! frame value)
  (vector-set! frame 4 (cons value (vector-ref frame 4))))

(def (frame-children frame)
  (reverse (vector-ref frame 4)))

(def (parse-artifact->cst artifact)
  (unless (and (parse-artifact-valid? artifact)
               (parse-artifact-success? artifact))
    (error "CST projection requires an accepted ParseArtifact"))
  (let ((stack '()) (root #f))
    (letrec
        ((append-value!
          (lambda (value)
            (if (pair? stack)
              (frame-add! (car stack) value)
              (begin
                (when root (error "multiple CST projection roots"))
                (set! root value)))))
         (push!
          (lambda (frame)
            (set! stack (cons frame stack))))
         (pop!
          (lambda ()
            (let (frame (car stack))
              (set! stack (cdr stack))
              frame))))
      (for-each
       (lambda (event)
         (case (event-kind event)
           ((start-node)
            (push! (make-frame 'node
                               (vector-ref event 1)
                               (vector-ref event 2)
                               (event-start event))))
           ((finish-node)
            (let (frame (pop!))
              (append-value!
               (make-syntax-node (vector-ref frame 2)
                                 (vector-ref frame 3)
                                 (event-end event)
                                 (frame-children frame)))))
           ((start-field)
            (push! (make-frame 'field
                               (vector-ref event 1)
                               #f
                               (event-start event))))
           ((finish-field)
            (let (frame (pop!))
              (append-value!
               (make-syntax-field (vector-ref frame 1)
                                  (vector-ref frame 3)
                                  (event-end event)
                                  (frame-children frame)))))
           ((token)
            (append-value!
             (make-token (token-event-token-kind event)
                         (token-event-lexeme event)
                         (event-start event)
                         (event-end event))))))
       (parse-artifact-events artifact))
      (unless (and (null? stack) (syntax-node? root))
        (error "CST event replay did not produce one root"))
      root)))

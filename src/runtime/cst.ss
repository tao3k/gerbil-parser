;;; -*- Gerbil -*-
;;; Gerbil CST projection replayed from canonical ParseArtifact v1 events.

(import (only-in ./artifact
                 event-end event-kind event-start parse-artifact-events
                 parse-artifact-success? parse-artifact-valid?
                 token-event-lexeme token-event-token-kind)
        (only-in ./token make-token))
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
;;; Mutable only while replaying the canonical event stream; frames never
;;; escape parse-artifact->cst, and completed children are immutable values.
(defstruct cst-frame (type identity kind start children) transparent: #t)

;;; Replays accepted artifact events with a private stack and publishes exactly
;;; one immutable root only after balanced node and field frames are observed.
;; parse-artifact->cst
;; : (-> ParseArtifact SyntaxNode)
(def (parse-artifact->cst artifact)
  (unless (and (parse-artifact-valid? artifact)
               (parse-artifact-success? artifact))
    (error "CST projection requires an accepted ParseArtifact"))
  (let ((stack '()) (root #f))
    (letrec
        ((append-value!
          (lambda (value)
            (if (pair? stack)
              (let (frame (car stack))
                (set! (cst-frame-children frame)
                      (cons value (cst-frame-children frame))))
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
            (push! (make-cst-frame 'node
                                   (vector-ref event 1)
                                   (vector-ref event 2)
                                   (event-start event)
                                   '())))
           ((finish-node)
            (let (frame (pop!))
              (append-value!
               (make-syntax-node (cst-frame-kind frame)
                                 (cst-frame-start frame)
                                 (event-end event)
                                 (reverse (cst-frame-children frame))))))
           ((start-field)
            (push! (make-cst-frame 'field
                                   (vector-ref event 1)
                                   #f
                                   (event-start event)
                                   '())))
           ((finish-field)
            (let (frame (pop!))
              (append-value!
               (make-syntax-field (cst-frame-identity frame)
                                  (cst-frame-start frame)
                                  (event-end event)
                                  (reverse (cst-frame-children frame))))))
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

;;; -*- Gerbil -*-
;;; Engine-owned static strategy data, never request buffers or mutable slots.
(import (only-in ./event-fold-state-frame.ss prepare-fold-state-layout))
(export prepare-event-fold-program fold-initial-states snapshot-fold-data event-fold-program?
        event-fold-program-root event-fold-program-layout
        event-fold-program-line-forms event-fold-program-finish-forms
        event-fold-program-helpers
        event-fold-helper-layout event-fold-helper-forms event-fold-helper-parameters
        fold-helper-ref)

(defstruct event-fold-program (root layout line-forms finish-forms helpers) final: #t)
(defstruct event-fold-helper (layout forms parameters) final: #t)

;; Preserve DAG sharing while taking ownership of grammar data. Reject cycles
;; and executable/mutable foreign values before constructing a reusable plan.
(def (snapshot-fold-data value)
  (let ((copies (make-table test: eq?)) (active (make-table test: eq?)))
    (def (copy value)
      (cond
       ((pair? value)
        (when (table-ref active value #f) (error "cyclic event fold program"))
        (or (table-ref copies value #f)
            (begin
              (table-set! active value #t)
              (let (result (cons (copy (car value)) (copy (cdr value))))
                (table-set! active value)
                (table-set! copies value result)
                result))))
       ((string? value)
        (or (table-ref copies value #f)
            (let (result (string-copy value))
              (table-set! copies value result) result)))
       ((or (null? value) (symbol? value) (number? value)
            (boolean? value) (char? value)) value)
       (else (error "event fold program requires closed data"))))
    (copy value)))

(def (fold-initial-states initial)
  (map (lambda (entry)
          (unless (and (list? entry) (= (length entry) 2) (symbol? (car entry))
                       (or (boolean? (cadr entry))
                           (and (exact-integer? (cadr entry)) (>= (cadr entry) 0))
                           (equal? (cadr entry) '(uint-stack))))
            (error "event fold requires typed state" entry))
          (cons (car entry) (if (equal? (cadr entry) '(uint-stack)) '() (cadr entry))))
       initial))

(def (initial-layout initial)
  (prepare-fold-state-layout (fold-initial-states initial)))

(def (prepare-event-fold-program root initial line-forms finish-forms (helpers '()))
  (let* ((owned (snapshot-fold-data (list root initial line-forms finish-forms helpers)))
         (directory (make-table test: eq?)))
    (for-each
     (lambda (helper)
       (unless (and (list? helper) (memv (length helper) '(3 4))
                    (symbol? (car helper)))
         (error "invalid event fold helper" helper))
       ;; Match the existing first-declaration lookup policy.
       (unless (table-ref directory (car helper) #f)
         (table-set! directory (car helper)
           (make-event-fold-helper (initial-layout (cadr helper)) (caddr helper)
                                   (if (= (length helper) 4) (cadddr helper) '())))))
     (list-ref owned 4))
    (make-event-fold-program (car owned) (initial-layout (cadr owned))
                            (caddr owned) (cadddr owned) directory)))

(def (fold-helper-ref helpers name)
  (table-ref helpers name #f))

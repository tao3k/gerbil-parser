;;; Fuse adjacent admitted source probes, not their ordered state transitions.
(import (only-in ./event-fold-analysis fold-expression-analysis fold-statement-analysis))
(export fold-fuse-byte-probes)

(def (probe form)
  (and (pair? form) (eq? (car form) 'if)
       (pair? (cadr form)) (eq? (caadr form) 'line-bytes-any-in?)
       (list? (list-ref (cadr form) 3)) (cadr form)))

(def (compatible? first second)
  (let* ((left (probe first)) (right (probe second))
         (bounds (list (cadr left) (caddr left)))
         (dependencies (fold-expression-analysis (cons 'offset-less? bounds)))
         (effects (fold-statement-analysis (append (caddr first) (cadddr first)))))
    (and (equal? bounds (list (cadr right) (caddr right)))
         (null? (vector-ref dependencies 2))
         (not (any (lambda (read) (memq read (vector-ref effects 1)))
                   (vector-ref dependencies 0)))
         ;; No opaque mutation, helper handoff or source-frame change may
         ;; invalidate a probe. Errors/events keep their original order below.
         (not (any (cut memq <> '(opaque helper bounds)) (vector-ref effects 2))))))

(def (fold-fuse-byte-probes forms)
  (let loop ((rest forms))
    (cond ((null? rest) '())
          ((and (pair? (cdr rest)) (probe (car rest)) (probe (cadr rest))
                (compatible? (car rest) (cadr rest)))
           (let ((classes (make-u8vector 256 0))
                 (first (car rest)) (second (cadr rest)))
             (for-each (lambda (form bit)
                         (for-each (lambda (byte)
                                     (u8vector-set! classes byte
                                       (fxior (u8vector-ref classes byte) bit)))
                                   (list-ref (cadr form) 3)))
                       (list first second) '(1 2))
             ;; Private compiler plan only; public IR admission is unchanged.
             (cons (list '%shared-byte-probes first second classes
                         (+ (if (null? (list-ref (cadr first) 3)) 0 1)
                            (if (null? (list-ref (cadr second) 3)) 0 2)))
                   (loop (cddr rest)))))
          (else (cons (car rest) (loop (cdr rest)))))))

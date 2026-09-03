;;; -*- Gerbil -*-
;;; Small compiler-domain functions shared by parser construction algorithms.

(export compiler-work-queue-empty?
        compiler-work-queue-pop
        compiler-work-queue-push-back
        compiler-terminal-set-add
        compiler-terminal-set-member?
        compiler-terminal-set-for-each)

;; A two-list FIFO gives state discovery and fixed-point propagation O(1)
;; amortized queue operations while preserving deterministic insertion order.
(def (compiler-work-queue-empty? queue)
  (and (null? (car queue)) (null? (cdr queue))))

(def (compiler-work-queue-pop queue)
  (let ((front (car queue))
        (rear (cdr queue)))
    (if (null? front)
      (compiler-work-queue-pop (cons (reverse rear) '()))
      (values (car front) (cons (cdr front) rear)))))

(def (compiler-work-queue-push-back queue value)
  (cons (car queue) (cons value (cdr queue))))

;; Terminal identities are dense non-negative integers during one compiler
;; admission, so an integer bit set avoids allocation in the propagation loop.
(def (compiler-terminal-set-add set terminal)
  (bitwise-ior set (arithmetic-shift 1 terminal)))

(def (compiler-terminal-set-member? set terminal)
  (not (zero? (bitwise-and set (arithmetic-shift 1 terminal)))))

(def (compiler-terminal-set-for-each set procedure)
  (let loop ((rest set) (terminal 0))
    (unless (zero? rest)
      (when (odd? rest) (procedure terminal))
      (loop (quotient rest 2) (+ terminal 1)))))

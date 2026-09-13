;;; -*- Gerbil -*-
;;; Allocation-free indexed-set algorithms shared by parser compilation.

(import (only-in :std/misc/list-builder with-list-builder))
(export compiler-terminal-set-add
        compiler-terminal-set-difference
        compiler-terminal-set-empty?
        compiler-terminal-set-member?
        compiler-terminal-set-for-each
        compiler-terminal-set-singleton
        compiler-terminal-set-union
        compiler-terminal-set->ordered-list)

;; Terminal identities are dense non-negative integers during one compiler
;; admission, so one arbitrary-precision integer is a compact immutable set.
;; : (-> Integer Fixnum Integer)
(def (compiler-terminal-set-add set terminal)
  (bitwise-ior set (arithmetic-shift 1 terminal)))

;; : (-> Fixnum Integer)
(def (compiler-terminal-set-singleton terminal)
  (arithmetic-shift 1 terminal))

;; : (-> Integer Integer Integer)
(def (compiler-terminal-set-union left right)
  (bitwise-ior left right))

;; : (-> Integer Integer Integer)
(def (compiler-terminal-set-difference set excluded)
  (bitwise-and set (bitwise-not excluded)))

;; : (-> Integer Boolean)
(def (compiler-terminal-set-empty? set)
  (zero? set))

;; : (-> Integer Fixnum Boolean)
(def (compiler-terminal-set-member? set terminal)
  (not (zero? (bitwise-and set (arithmetic-shift 1 terminal)))))

;; Gambit's optimized first-set-bit visits only present terminals. Clearing the
;; lowest bit makes sparse traversal O(cardinality), not O(max terminal index),
;; and allocates no intermediate list on the propagation hot path.
;; : (-> Integer Procedure Void)
(def (compiler-terminal-set-for-each set procedure)
  (let loop ((rest set))
    (unless (zero? rest)
      (let (terminal (first-set-bit rest))
        (procedure terminal)
        (loop (bitwise-and rest (- rest 1)))))))

;; Materialize in canonical terminal-catalog order through the standard
;; library's linear list builder.
;; : (-> Integer Vector List)
(def (compiler-terminal-set->ordered-list set terminal-values)
  (with-list-builder (collect!)
    (compiler-terminal-set-for-each
     set
     (lambda (terminal)
       (collect! (vector-ref terminal-values terminal))))))

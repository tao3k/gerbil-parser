;;; -*- Gerbil -*-
;;; Allocation-free indexed-set algorithms shared by parser compilation.

(import (only-in :std/list/list-builder with-list-builder))
(export compiler-index-set-add
        compiler-index-set-difference
        compiler-index-set-empty?
        compiler-index-set-member?
        compiler-index-set-for-each
        compiler-index-set-singleton
        compiler-index-set-union
        compiler-index-set->ordered-values)

;; Indexed identities are dense non-negative integers within one compiler
;; domain, so one arbitrary-precision integer is a compact immutable set.
;; : (-> Integer Fixnum Integer)
(def (compiler-index-set-add set index)
  (bitwise-ior set (arithmetic-shift 1 index)))

;; : (-> Fixnum Integer)
(def (compiler-index-set-singleton index)
  (arithmetic-shift 1 index))

;; : (-> Integer Integer Integer)
(def (compiler-index-set-union left right)
  (bitwise-ior left right))

;; : (-> Integer Integer Integer)
(def (compiler-index-set-difference set excluded)
  (bitwise-and set (bitwise-not excluded)))

;; : (-> Integer Boolean)
(def (compiler-index-set-empty? set)
  (zero? set))

;; : (-> Integer Fixnum Boolean)
(def (compiler-index-set-member? set index)
  (not (zero? (bitwise-and set (arithmetic-shift 1 index)))))

;; Gambit's optimized first-set-bit visits only present indexes. Clearing the
;; lowest bit makes sparse traversal O(cardinality), not O(maximum index),
;; and allocates no intermediate list on the propagation hot path.
;; : (-> Integer Procedure Void)
(def (compiler-index-set-for-each set procedure)
  (let loop ((rest set))
    (unless (zero? rest)
      (let (index (first-set-bit rest))
        (procedure index)
        (loop (bitwise-and rest (- rest 1)))))))

;; Materialize in canonical catalog order through the standard library's
;; linear list builder.
;; : (-> Integer Vector List)
(def (compiler-index-set->ordered-values set values)
  (with-list-builder (collect!)
    (compiler-index-set-for-each
     set
     (lambda (index)
       (collect! (vector-ref values index))))))

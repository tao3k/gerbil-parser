;;; -*- Gerbil -*-
;;; Shared indexed-set and source-position algorithms for parser compilation.

(import (only-in :std/list/list-builder with-list-builder))
(export ascii-lower-byte
        compiler-index-set-add
        compiler-index-set-difference
        compiler-index-set-empty?
        compiler-index-set-member?
        compiler-index-set-for-each
        compiler-index-set-singleton
        compiler-index-set-union
        compiler-index-set->ordered-values
        compiler-position-index-add!
        compiler-position-index-freeze!
        compiler-position-vector-next
        compiler-u8vector-prefix?)

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

;; A source-order scan conses offsets into descending lists. Freeze once so
;; repeated forward queries can use binary search without copying any suffix.
(def (compiler-position-index-add! table keys key position)
  (let (positions (hash-get table key))
    (unless positions (set! keys (cons key keys)))
    (hash-put! table key (cons position (or positions '())))
    keys))

(def (compiler-position-index-freeze! table keys)
  (for-each (lambda (key)
              (hash-put! table key (list->vector (hash-get table key))))
            keys))

;; Vectors are descending; return the first source position at or after from.
(def (compiler-position-vector-next positions from)
  (and positions
       (let search ((low 0) (high (vector-length positions)))
         (if (< low high)
           (let (middle (quotient (+ low high) 2))
             (if (>= (vector-ref positions middle) from)
               (search (+ middle 1) high)
               (search low middle)))
           (and (> low 0) (vector-ref positions (- low 1)))))))

(def (ascii-lower-byte value)
  (if (and (<= 65 value) (<= value 90)) (+ value 32) value))

;; Compare a prefix in place so noncandidate source lines allocate no key.
(def (compiler-u8vector-prefix? bytes from until prefix ascii-ci?)
  (let (size (u8vector-length prefix))
    (and (<= (+ from size) until)
         (let compare ((index 0))
           (or (= index size)
               (and (let ((actual (u8vector-ref bytes (+ from index)))
                          (expected (u8vector-ref prefix index)))
                      (= (if ascii-ci? (ascii-lower-byte actual) actual)
                         (if ascii-ci? (ascii-lower-byte expected) expected)))
                    (compare (+ index 1))))))))

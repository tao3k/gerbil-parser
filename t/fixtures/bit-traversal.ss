;;; Execute the complete unchanged traversal declaration. Branch controls
;;; replace only classification/count inputs; arithmetic primitives forward.
(import (only-in "event-walk-source.ss" source-procedure))
(export traversal-observation traversal-reference traversal-masks)

(def traversal-producer
  (source-procedure "src/compiler/funcs.ss" 'compiler-index-set-for-each
    '(compiler-index-field-width fixnum? bit-count extract-bit-field bitwise-and)))

(def (traversal-reference mask)
  (let loop ((index 0) (found '()))
    (if (= index (integer-length mask)) (reverse found)
      (loop (+ index 1)
        (if (not (zero? (bitwise-and mask (arithmetic-shift 1 index)))) (cons index found) found)))))

(def (traversal-masks width)
  (let* ((boundary (* 3 width)) (high (+ boundary 2))
         (dense (- (arithmetic-shift 1 (+ boundary 1)) 1)))
    (list 0 1 5 dense (arithmetic-shift 1 high)
      (bitwise-ior (arithmetic-shift 1 (- width 1)) (arithmetic-shift 1 width)
        (arithmetic-shift 1 (+ width 1)) (arithmetic-shift 1 high))
      (bitwise-ior (arithmetic-shift 1 (+ boundary 1)) (arithmetic-shift 1 high))
      (bitwise-ior 1 (arithmetic-shift 1 4096)))))

(def (traversal-observation mask width route (callback void))
  (let ((callbacks '()) (fields '()) (events '()))
    (traversal-producer mask
      (lambda (index)
        (set! callbacks (cons index callbacks))
        (set! events (cons 'callback events))
        (callback index))
      width
      (lambda (value) (or (zero? value) (eq? route 'fixnum)
                         (and (eq? route 'native) (fixnum? value))))
      (lambda (value)
        (case route
          ((direct) 0)
          ((field) (+ (integer-length value) 1))
          (else (bit-count value))))
      (lambda (size offset value)
        (set! fields (cons offset fields))
        (extract-bit-field size offset value))
      (lambda (left right)
        (set! events (cons 'clear events))
        (bitwise-and left right)))
    (list (reverse callbacks) (reverse fields) (reverse events))))

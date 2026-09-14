;;; -*- Gerbil -*-
;;; Allocation-bounded string algorithms shared by grammar-source adapters.

(import (only-in :std/srfi/13 string-contains))
(export ascii-whitespace? string-prefix-at? string-index-from text-lines)

(def (ascii-whitespace? ch)
  (or (char=? ch #\space) (char=? ch #\tab)
      (char=? ch #\newline) (char=? ch #\return)))

;;; Tests a prefix in place instead of allocating a suffix substring.
(def (string-prefix-at? text prefix offset)
  (let ((length (string-length text))
        (prefix-length (string-length prefix)))
    (and (exact-integer? offset) (>= offset 0)
         (<= (+ offset prefix-length) length)
         (let loop ((index 0))
           (or (= index prefix-length)
               (and (char=? (string-ref text (+ offset index))
                            (string-ref prefix index))
                    (loop (+ index 1))))))))

;;; SRFI-13 owns the optimized substring search; the wrapper fixes the shared
;;; adapter contract and its optional starting offset.
(def (string-index-from text wanted (start 0))
  (string-contains text wanted start))

;;; Preserves empty and final lines because source coordinates and digests are
;;; grammar evidence, not presentation text.
(def (text-lines source)
  (let (length (string-length source))
    (let loop ((start 0) (offset 0) (lines '()))
      (cond
       ((= offset length)
        (reverse (cons (substring source start offset) lines)))
       ((char=? (string-ref source offset) #\newline)
        (loop (+ offset 1) (+ offset 1)
              (cons (substring source start offset) lines)))
       (else (loop start (+ offset 1) lines))))))

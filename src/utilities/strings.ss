;;; -*- Gerbil -*-
;;; Allocation-bounded string algorithms shared by grammar-source adapters.

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

;;; V19's core owns the optimized substring search.  Its compiler signature
;;; currently describes the index-or-#f result as Boolean, so keep the call
;;; indirect at this source-adapter boundary until that upstream signature is
;;; corrected.
(def (string-index-from text wanted (start 0))
  (let (index (apply string-contains (list text wanted start)))
    (and index (:- index :fixnum))))

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

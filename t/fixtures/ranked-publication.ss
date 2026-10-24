;;; Matched compact-trie primitive from before terminal staging/publication.
;;; Keep the parser, lexical plans and result protocol outside this comparator.
(import (only-in :gerbil-parser/src/runtime/scan make-literal-trie literal-trie-child make-ranked-regular-scanner horizontal-whitespace? newline? identifier-start?)
        (only-in :gerbil-parser/src/compiler/lexical-expression prefer-generated-match))
(export reference-ranked-publication-scanner reference-ranked-lexical-scanner-factory)
(def (reference-ranked-publication-scanner entries)
  (def (best-admitted candidates admitted)
    (let loop ((remaining candidates) (best #f))
      (if (null? remaining) best
        (let (candidate (car remaining))
          (loop (cdr remaining)
            (if (and (or (not admitted) (vector-ref admitted (caddr candidate)))
                     (or (not best) (> (cadr candidate) (cadr best))
                         (and (= (cadr candidate) (cadr best))
                              (< (caddr candidate) (caddr best)))))
              candidate best))))))
  (let (root (make-literal-trie
                (map (lambda (entry) (cons (car entry) (cdr entry))) entries)
                '() (lambda (previous payload) (cons payload previous))))
    (lambda (source start (admitted #f))
      (let (limit (string-length source))
        (let scan ((node root) (offset start) (selected #f))
          (if (= offset limit) selected
            (let (child (literal-trie-child node (string-ref source offset)))
              (if child
                (let* ((next (+ offset 1)) (terminal (best-admitted (vector-ref child 0) admitted)))
                  (scan child next
                    (if terminal (list (car terminal) next (cadr terminal) (caddr terminal)) selected)))
                selected))))))))


;;; Original separate traversals and complete ranked result competition.
(def (reference-ranked-lexical-scanner-factory entries)
  (let ((literal (reference-ranked-publication-scanner entries))
        (starts (make-vector 128 #f)))
    (for-each (lambda (entry)
                (let (code (char->integer (string-ref (car entry) 0)))
                  (when (< code 128) (vector-set! starts code #t)))) entries)
    (lambda (regular-entries admissions)
      (let* ((regular (and (pair? regular-entries) (make-ranked-regular-scanner regular-entries)))
            (admitted (and admissions (vector-copy admissions)))
             (has-literals? (any (lambda (entry) (or (not admitted) (vector-ref admitted (cadddr entry)))) entries))
             (regular-starts
              (list->vector (map (lambda (code)
                (let (ch (integer->char code))
                  (any (lambda (entry)
                    (case (car entry)
                      ((whitespace+) (char-whitespace? ch))
                      ((horizontal-whitespace+) (horizontal-whitespace? ch))
                      ((newline+) (newline? ch))
                      ((decimal-digit+ number) (char-numeric? ch))
                      ((identifier) (identifier-start? ch)))) regular-entries))) (iota 128)))))
        (lambda (source start)
          (and (not (= start (string-length source)))
               (let (code (char->integer (string-ref source start)))
                 (prefer-generated-match
                  (and regular (or (>= code 128) (vector-ref regular-starts code))
                       (regular source start))
                  (and has-literals? (or (>= code 128) (vector-ref starts code))
                       (literal source start admitted))))))))))

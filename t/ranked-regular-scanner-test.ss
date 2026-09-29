;;; -*- Gerbil -*-
;;; Differential witness for the mode-local closed-run lexical DFA.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/compiler/machine lexical-dispatch/ranked)
        (only-in :gerbil-parser/src/runtime/scan
                 make-ranked-regular-scanner))

(def regular-scanner
  (make-ranked-regular-scanner
   '((whitespace+ white 0 0)
     (horizontal-whitespace+ horizontal 3 1)
     (newline+ newline 2 2)
     (decimal-digit+ digits 1 3)
     (identifier name 0 4))))

(def (legacy-regular-match source offset)
  (lexical-dispatch/ranked
   source offset
   ((white (whitespace+))
    (horizontal (precedence 3 (horizontal-whitespace+)))
    (newline (precedence 2 (newline+)))
    (digits (precedence 1 (decimal-digit+)))
    (name (identifier)))))

(def ranked-regular-scanner-tests
  (test-suite "ranked regular scanner"
    (test-case "DFA agrees with individual scanners at every source offset"
      (for-each
       (lambda (source)
         (let loop ((offset 0))
           (when (< offset (string-length source))
             (let (match (regular-scanner source offset))
               (check (and match (list (car match) (cadr match)
                                       (caddr match)))
                      => (legacy-regular-match source offset)))
             (loop (+ offset 1)))))
       '("alpha-27 42\r\n" "αβ-٣\t\n?" "7.2 foo" " \t\n"
         "_name-1+other")))
    (test-case "longest match precedes rank and declaration order"
      (check (regular-scanner " \t\n" 0) => '(white 3 0 0))
      (check (regular-scanner "\t\tx" 0) => '(horizontal 2 3 1))
      (check (regular-scanner "α-2+" 0) => '(name 3 0 4))
      (check (regular-scanner "?" 0) => #f))))

(export ranked-regular-scanner-tests)

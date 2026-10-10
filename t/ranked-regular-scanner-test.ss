;;; -*- Gerbil -*-
;;; Differential witness for the mode-local closed-run lexical DFA.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/compiler/machine lexical-dispatch/ranked)
        (only-in :gerbil-parser/src/runtime/scan
                 make-ranked-regular-scanner scan-whitespace scan-horizontal-whitespace
                 scan-newline scan-decimal-digits scan-identifier scan-number-literal))

(def regular-scanner
  (make-ranked-regular-scanner
   '((whitespace+ white 0 0)
     (horizontal-whitespace+ horizontal 3 1)
     (newline+ newline 2 2)
     (decimal-digit+ digits 1 3)
     (identifier name 0 4)
     (number number 0 5))))

(def (legacy-regular-match source offset)
  (lexical-dispatch/ranked
   source offset
   ((white (whitespace+))
    (horizontal (precedence 3 (horizontal-whitespace+)))
    (newline (precedence 2 (newline+)))
    (digits (precedence 1 (decimal-digit+)))
    (name (identifier))
    (number (number)))))

(def regular-entries
  '((whitespace+ white 0 0) (horizontal-whitespace+ horizontal 3 1)
    (newline+ newline 2 2) (decimal-digit+ digits 1 3)
    (identifier name 0 4) (number number 0 5)))
(def independent-runs
  (list scan-whitespace scan-horizontal-whitespace scan-newline
        scan-decimal-digits scan-identifier scan-number-literal))
(def (masked-run-reference source at mask)
  (let loop ((entries regular-entries) (runs independent-runs) (index 0) (best #f))
    (if (null? entries) best
      (let* ((entry (car entries))
             (end (and (odd? (quotient mask (expt 2 index))) ((car runs) source at)))
             (candidate (and end (list (cadr entry) end (caddr entry) index))))
        (loop (cdr entries) (cdr runs) (+ index 1)
              (if (and candidate
                       (or (not best) (> end (cadr best))
                           (and (= end (cadr best))
                                (or (> (caddr candidate) (caddr best))
                                    (and (= (caddr candidate) (caddr best))
                                         (< index (cadddr best)))))))
                candidate best))))))

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
         "_name-1+other" "1." "1e" "1e+" "1.2e+3"
         "٧.٢E-٣" "8e+4z")))
    (test-case "start-state specialization agrees with independent runs in every rule subset"
      (for-each (lambda (mask)
        (let (scan (make-ranked-regular-scanner
                     (filter (lambda (entry) (odd? (quotient mask (expt 2 (cadddr entry))))) regular-entries)))
          (for-each (lambda (source)
            (let offsets ((at 0))
              (when (< at (string-length source))
                (check (scan source at) => (masked-run-reference source at mask))
                (offsets (+ at 1)))))
            '("alpha-27 42\r\n" "αβ-٣\t\n?" "1." "1e" "1e+" "1.2e+3"
              "٧.٢E-٣" "Ⅳ¼²名字" "8e+4z" "e.E+-1" "_name-1+other" "" "?"))))
        (iota 64)))
    (test-case "longest match precedes rank and declaration order"
      (check (regular-scanner " \t\n" 0) => '(white 3 0 0))
      (check (regular-scanner "\t\tx" 0) => '(horizontal 2 3 1))
      (check (regular-scanner "α-2+" 0) => '(name 3 0 4))
      (check (regular-scanner "12.3e+4!" 0) => '(number 7 0 5))
      (check (regular-scanner "12e+!" 0) => '(digits 2 1 3))
      (check (regular-scanner "?" 0) => #f))))

(export ranked-regular-scanner-tests)

;; gxtest discovers only exported names ending in -test.
(def ranked-regular-scanner-test ranked-regular-scanner-tests)
(export ranked-regular-scanner-test)

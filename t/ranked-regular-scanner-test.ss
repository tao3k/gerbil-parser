(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Differential witness for the mode-local closed-run lexical DFA.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/compiler/machine lexical-dispatch/ranked)
        (only-in :gerbil-parser/src/runtime/scan
                 make-ranked-regular-scanner make-ranked-lexical-scanner-factory scan-whitespace scan-horizontal-whitespace
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
  (and (< at (string-length source))
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
                candidate best)))))))

(def fused-literals
  '(("if" keyword 2 6) ("ifelse" longer 2 7) ("a" short 9 8)
    ("ab" exact 0 9) ("α😀" wide 2 10) ("1e" prefix 6 11) ("1.2" fraction 10 12)))
(def (prefer-oracle left right)
  (if (and right
           (or (not left) (> (cadr right) (cadr left))
               (and (= (cadr right) (cadr left))
                    (or (> (caddr right) (caddr left))
                        (and (= (caddr right) (caddr left)) (< (cadddr right) (cadddr left))))))) right left))
(def (fused-literal-oracle source at admissions)
  (fold (lambda (entry best)
    (let (end (+ at (string-length (car entry))))
      (prefer-oracle best
        (and (vector-ref admissions (cadddr entry)) (<= end (string-length source))
             (string=? (car entry) (substring source at end))
             (list (cadr entry) end (caddr entry) (cadddr entry)))))) #f fused-literals))

(deflanguage-parser-tests ranked-regular-scanner-tests "ranked regular scanner"
  (engine)
    (scenario "DFA agrees with individual scanners at every source offset"
      (each (source) ('("alpha-27 42\r\n" "αβ-٣\t\n?" "7.2 foo" " \t\n"
         "_name-1+other" "1." "1e" "1e+" "1.2e+3"
         "٧.٢E-٣" "8e+4z"))
         (iterate loop ((offset 0))
           (when (< offset (string-length source))
             (scope ((match (regular-scanner source offset)))
               (equal (and match (list (car match) (cadr match)
                                       (caddr match)))
                       (legacy-regular-match source offset)))
             (continue loop (+ offset 1))))))
    (scenario "start-state specialization agrees with independent runs in every rule subset"
      (each (mask) ((iota 64))
        (scope ((scan (make-ranked-regular-scanner
                     (filter (lambda (entry) (odd? (quotient mask (expt 2 (cadddr entry))))) regular-entries))))
          (each (source) ('("alpha-27 42\r\n" "αβ-٣\t\n?" "1." "1e" "1e+" "1.2e+3"
              "٧.٢E-٣" "Ⅳ¼²名字" "8e+4z" "e.E+-1" "_name-1+other" "" "?"))
            (iterate offsets ((at 0))
              (when (< at (string-length source))
                (equal (scan source at)  (masked-run-reference source at mask))
                (continue offsets (+ at 1))))))))
    (scenario "product automaton agrees with independent scans in every regular subset"
      (scope ((factory (make-ranked-lexical-scanner-factory fused-literals)))
        (each (mask) ((iota 64))
          (each (literal-mask) ('(0 64 8191 4096))
            (sequential ((admissions (list->vector (map (lambda (index)
                                                   (odd? (quotient literal-mask (expt 2 index)))) (iota 13))))
                   (entries (filter (lambda (entry) (odd? (quotient mask (expt 2 (cadddr entry))))) regular-entries))
                   (scan (factory entries admissions)))
              (each (source) ('("if ifelse ifelseif" "a ab abc" "α😀 αβ-٣ \t\r\n"
                  "1.2e+3 1e+ 1." "Ⅳ¼² 名字 ?"))
                (iterate offsets ((at 0))
                  (when (<= at (string-length source))
                    (equal (scan source at)
                            (prefer-oracle (masked-run-reference source at mask)
                                             (fused-literal-oracle source at admissions)))
                    (continue offsets (+ at 1))))))))))
    (scenario "mode preparation freezes masks and publishes fresh complete results"
      (sequential ((factory (make-ranked-lexical-scanner-factory fused-literals))
             (admissions (make-vector 13 #t))
             (scan (factory regular-entries admissions))
             (result (scan "if!" 0)))
        (act (vector-set! admissions 6 #f))
        (act (set-car! result 'foreign))
        (equal (scan "if!" 0)  '(keyword 2 2 6))
        (equal (scan "ifelsewhere!" 0)  '(name 11 0 4))))
    (scenario "closed singleton modes own their literal snapshot"
      (sequential ((text (string-copy "α😀")) (entry (list text 'wide 2 0))
             (factory (make-ranked-lexical-scanner-factory (list entry))))
        (act (string-set! text 0 #\x))
        (act (set-car! (cdr entry) 'foreign))
        (scope ((scan (factory '() (vector #t))))
          (equal (scan "α😀!" 0)  '(wide 2 2 0))
          (equal (scan "x😀!" 0)  #f)
          (equal (scan "α" 0)  #f)
          (equal (scan "" 0)  #f))))
    (scenario "regular scanning retains live winner payloads and fresh result cells"
      (sequential ((entry (list 'identifier 'name 0 0))
             (scan (make-ranked-regular-scanner (list entry)))
             (first (scan "alpha!" 0)))
        (equal first  '(name 5 0 0))
        (act (set-car! first 'foreign))
        (act (set-car! (cdr entry) 'updated))
        (equal (scan "a!" 0)  '(updated 1 0 0))
        (equal (scan "α!" 0)  '(updated 1 0 0))
        (equal (scan "!a" 1)  '(updated 2 0 0))
        (equal (scan "" 0)  #f)))
    (scenario "product scanning compare live regular priority against literal candidates"
      (sequential ((entry (list 'identifier 'name 0 0))
             (factory (make-ranked-lexical-scanner-factory '(("a" keyword 2 1))))
             (scan (factory (list entry) (vector #t #t))))
        (equal (scan "a!" 0)  '(keyword 1 2 1))
        (act (set-car! (cddr entry) 9))
        (equal (scan "a!" 0)  '(name 1 9 0))
        (equal (scan "alpha!" 0)  '(name 5 9 0))
        (equal (scan "α!" 0)  '(name 1 9 0))))
    (property "longest match precedes rank and declaration order"
      (bindings)
      (equal (regular-scanner " \t\n" 0)  '(white 3 0 0))
      (equal (regular-scanner "\t\tx" 0)  '(horizontal 2 3 1))
      (equal (regular-scanner "α-2+" 0)  '(name 3 0 4))
      (equal (regular-scanner "12.3e+4!" 0)  '(number 7 0 5))
      (equal (regular-scanner "12e+!" 0)  '(digits 2 1 3))
      (equal (regular-scanner "?" 0)  #f)))


;; gxtest discovers only exported names ending in -test.
(def ranked-regular-scanner-test ranked-regular-scanner-tests)
(export ranked-regular-scanner-test)

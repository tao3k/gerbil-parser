;;; -*- Gerbil -*-
;;; Scan regular owner: original pure lexical algorithms.
(import (only-in :std/vector/vector vector-map/index)
        (only-in "./basic.ss" identifier-start? identifier-rest? horizontal-whitespace? newline?))
(export regular-kind-bit regular-character-mask regular-ascii-first regular-ascii-rest regular-mask regular-accept-mask regular-number-transition regular-transition scan-regular-run make-ranked-regular-scanner)

;;; Closed run expressions share one subset DFA. The number rule has pending
;;; fraction/exponent states; the last accepting state is the maximal match.
;;; Mode-specific winners are compiled once.
(def (regular-kind-bit kind)
  (case kind
    ((whitespace+) 1)
    ((horizontal-whitespace+) 2)
    ((newline+) 4)
    ((decimal-digit+) 8)
    ((identifier) 16)
    ((number) 32)
    (else (error "unsupported regular lexical expression" kind))))

(def (regular-character-mask ch first?)
  (bitwise-ior
   (if (char-whitespace? ch) 1 0)
   (if (horizontal-whitespace? ch) 2 0)
   (if (newline? ch) 4 0)
   (if (char-numeric? ch) 8 0)
   (if (if first? (identifier-start? ch) (identifier-rest? ch)) 16 0)
   (if (and first? (char-numeric? ch)) 32 0)))

(def regular-ascii-first
  (vector-map/index
   (lambda (index _) (regular-character-mask (integer->char index) #t))
   (make-vector 128)))

(def regular-ascii-rest
  (vector-map/index
   (lambda (index _) (regular-character-mask (integer->char index) #f))
   (make-vector 128)))

(def (regular-mask ch first?)
  (let (code (char->integer ch))
    (if (< code 128)
      (vector-ref (if first? regular-ascii-first regular-ascii-rest) code)
      (regular-character-mask ch first?))))

;; Number states: whole=32, dot=64, fraction=128, exponent=256,
;; sign=512, exponent-digits=1024. Only whole/fraction/exponent-digits accept.
(def (regular-accept-mask active)
  (bitwise-ior
   (bitwise-and active 31)
   (if (zero? (bitwise-and active 1184)) 0 32)))

(def (regular-number-transition active ch numeric?)
  (cond
   (numeric?
    (bitwise-ior
     (if (zero? (bitwise-and active 32)) 0 32)
     (if (zero? (bitwise-and active 192)) 0 128)
     (if (zero? (bitwise-and active 1792)) 0 1024)))
   ((char=? ch #\.)
    (if (zero? (bitwise-and active 32)) 0 64))
   ((or (char=? ch #\e) (char=? ch #\E))
    (if (zero? (bitwise-and active 160)) 0 256))
   ((or (char=? ch #\+) (char=? ch #\-))
    (if (zero? (bitwise-and active 256)) 0 512))
   (else 0)))

(def (regular-transition active ch)
  (let (mask (regular-mask ch #f))
    (bitwise-ior
     (bitwise-and active mask)
     (regular-number-transition
      active ch (not (zero? (bitwise-and mask 8)))))))

;;; Private hygienic lowering: arguments are stable caller bindings, except
;;; offset, which is evaluated once by the loop initializer. A subset without
;;; a number state is monotone and needs no acceptance rollback machinery.
(defrules scan-regular-run ()
  ((_ source offset limit initial winners)
   (let loop ((at offset) (active initial))
     (let (next (if (< at limit)
                  (bitwise-and active (regular-mask (string-ref source at) #f))
                  0))
       (if (zero? next)
         (let (winner (vector-ref winners active))
           (list (car winner) at (cadr winner) (caddr winner)))
         (loop (+ at 1) next))))))

;;; entries: (expression-kind token-name precedence declaration-index).
;;; A lexical mode admits whole rules before building this scanner.
(def (make-ranked-regular-scanner entries)
  (let* ((available
          (fold (lambda (entry mask)
                  (bitwise-ior mask (regular-kind-bit (car entry))))
                0 entries))
         (winners
          (vector-map/index
           (lambda (mask _)
             (fold
              (lambda (entry best)
                (if (zero? (bitwise-and mask (regular-kind-bit (car entry))))
                  best
                  (let (candidate (cdr entry))
                    (if (or (not best)
                            (> (cadr candidate) (cadr best))
                            (and (= (cadr candidate) (cadr best))
                                 (< (caddr candidate) (caddr best))))
                      candidate best))))
              #f entries))
           (make-vector (if (zero? (bitwise-and available 32)) 32 64)))))
    (if (zero? (bitwise-and available 32))
      (lambda (source start)
        (let* ((length (string-length source))
               (initial (and (< start length)
                             (bitwise-and available (regular-mask (string-ref source start) #t)))))
          (and initial (not (zero? initial))
               (scan-regular-run source (+ start 1) length initial winners))))
      (lambda (source start)
        (let* ((length (string-length source))
               (initial (and (< start length)
                             (bitwise-and available (regular-mask (string-ref source start) #t)))))
          (and initial (not (zero? initial))
               ;; Initial masks are in 0..63; bit 32 is the number start.
               (if (< initial 32)
                 (scan-regular-run source (+ start 1) length initial winners)
                 (let loop ((offset (+ start 1)) (active initial)
                            (accepted initial) (accepted-end (+ start 1)))
                   (let* ((next (if (< offset length)
                                  (regular-transition active (string-ref source offset)) 0))
                          (accept (regular-accept-mask next)))
                     (cond
                      ((zero? next)
                       (let (winner (vector-ref winners accepted))
                         (list (car winner) accepted-end (cadr winner) (caddr winner))))
                      ((zero? accept) (loop (+ offset 1) next accepted accepted-end))
                      (else (loop (+ offset 1) next accept (+ offset 1)))))))))))))


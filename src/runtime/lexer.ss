;;; Deterministic, lossless reference lexer.

(import ./token)
(export lex-source)

(def (identifier-start? ch)
  (or (char-alphabetic? ch) (char=? ch #\_)))

(def (identifier-rest? ch)
  (or (identifier-start? ch) (char-numeric? ch)))

(def (scan-while source start predicate)
  (let (length (string-length source))
    (let loop ((offset start))
      (if (and (< offset length) (predicate (string-ref source offset)))
        (loop (+ offset 1))
        offset))))

(def (emit source kind start end)
  (make-token kind (substring source start end) start end))

(def (lex-source source)
  (let (length (string-length source))
    (let loop ((offset 0) (tokens '()))
      (if (= offset length)
        (reverse tokens)
        (let (ch (string-ref source offset))
          (cond
           ((char-whitespace? ch)
            (let (end (scan-while source offset char-whitespace?))
              (loop end (cons (emit source 'whitespace offset end) tokens))))
           ((char-numeric? ch)
            (let (end (scan-while source offset char-numeric?))
              (loop end (cons (emit source 'number offset end) tokens))))
           ((identifier-start? ch)
            (let (end (scan-while source offset identifier-rest?))
              (loop end (cons (emit source 'identifier offset end) tokens))))
           ((memv ch '(#\+ #\- #\* #\/ #\( #\)))
            (loop (+ offset 1)
                  (cons (emit source 'punctuation offset (+ offset 1)) tokens)))
           (else
            (loop (+ offset 1)
                  (cons (emit source 'unknown offset (+ offset 1)) tokens)))))))))

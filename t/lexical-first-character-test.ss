(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; Differential witness for admission filters on nonregular lexical rules.
(import :std/test
        :gerbil-parser/src/language/grammar
        (only-in :gerbil-parser/src/compiler/machine lexical-dispatch/ranked)
        (only-in :gerbil-parser/src/compiler/lexical-lexer prepare-generated-lexer)
        (only-in :gerbil-parser/src/runtime/lexer scan-source-token)
        (only-in :gerbil-parser/src/runtime/scan scan-emit)
        (only-in :gerbil-parser/src/runtime/source-scanner
                 make-source-scanner source-scanner-initial-state source-scanner-step
                 source-scan-state-character-offset source-scan-state-byte-offset)
        (only-in :gerbil-parser/src/runtime/token token-kind token-lexeme token-start token-end))

(begin
 (deflanguage first-character-witness
  (syntax
   (lexical
    (root source-file)
    (lex
   (numeric Numeric (number-literal ("0x" "0b" "hex") "_" ("M") #t #t))
   (quoted Quoted (quoted-string "\"" "'"))
   (escaped Escaped (escaped-quoted-string "@"))
   (here Here (heredoc))
   (name Name (identifier))
   (other Other (fallback)))
    (extras)
    (keywords)
    (recoveries)
    (conflicts reject)
    (case-insensitive #f)))
  (rules
   (source-file (node SourceFile
     (repeat (choice numeric quoted escaped here name other))))))
 (bind-fixture-grammar-release first-character-witness "first-character-witness" "v1" "first-character-witness.v1") )

(def (reference-match source offset)
  (lexical-dispatch/ranked source offset
   ((numeric (number-literal ("0x" "0b" "hex") "_" ("M") #t #t))
    (quoted (quoted-string "\"" "'"))
    (escaped (escaped-quoted-string "@"))
    (here (heredoc))
    (name (identifier))
    (other (fallback)))))

(def lexical-first-character-test
  (test-suite "closed lexical first-character admission"
    (test-case "prepared competition preserves evaluation order and empty projections"
      (let (observed '())
        (def (rule name rank first? extent)
          (vector (lambda (terminals) #t)
                  (lambda (source offset)
                    (set! observed (cons name observed))
                    (let (end (extent source offset))
                      (and end (list name end rank))))
                  #f name rank first? #f))
        (let-values (((lexer plans certificates)
                      (prepare-generated-lexer
                       (list
                        (rule 'first 0 char-alphabetic? (lambda (s o) (+ o 1)))
                        (rule 'long 0 (lambda (ch) (char=? ch #\a))
                              (lambda (s o) (and (char=? (string-ref s o) #\a) (+ o 2))))
                        (rule 'priority 9 char-alphabetic? (lambda (s o) (+ o 1))))
                       (lambda () (error "unused certificate preparation")) '#())))
          (for-each
           (lambda (row)
             (set! observed '())
             (let-values (((token end) (lexer (car row) 0 7 #f)))
               (check (list (token-kind token) (token-lexeme token)
                            (token-start token) (token-end token) end)
                      => (cadr row)))
             (check (reverse observed) => (caddr row)))
           '(("ab" (long "ab" 7 9 2) (priority long first))
             ("b" (priority "b" 7 8 1) (priority first))
             ("α" (priority "α" 7 9 1) (priority long first))))
          (set! observed '())
          (check-exception (lexer "?" 0 0 #f) true)
          (check observed => '()))))
    (test-case "engine token bytes preserve 1-4 byte scalars NUL and combining marks"
      (let* ((source (string-append "Aé中😀" (string #\nul) "e" (string (integer->char #x301))))
             (scanner (make-source-scanner source #f
                        (lambda (text start context _mode)
                          (values (and (< start (string-length text)) 'scalar)
                                  (+ start 1) context)))))
        (let loop ((state (source-scanner-initial-state scanner))
                   (index 0) (byte-start 0) (widths '(1 2 3 4 1 1 2)))
          (if (null? widths)
            (let-values (((token final) (source-scanner-step scanner state #f)))
              (check token => #f)
              (check (source-scan-state-character-offset final) => 7)
              (check (source-scan-state-byte-offset final) => 14))
            (let* ((byte-end (+ byte-start (car widths)))
                   (direct (scan-emit source 'scalar index (+ index 1) (+ 7 byte-start))))
              (check (token-start direct) => (+ 7 byte-start))
              (check (token-end direct) => (+ 7 byte-end))
              (let-values (((token next) (source-scanner-step scanner state #f)))
                (check (token-kind token) => 'scalar)
                (check (token-lexeme token) => (substring source index (+ index 1)))
                (check (token-start token) => byte-start)
                (check (token-end token) => byte-end)
                (loop next (+ index 1) byte-end (cdr widths))))))))
    (test-case "filtered scanner preserves maximal munch at every offset"
      (for-each
       (lambda (source)
         (let loop ((offset 0))
           (when (< offset (string-length source))
             (let* ((match (reference-match source offset))
                    (expected (scan-emit source (car match) offset (cadr match) 0)))
               (let-values (((actual end)
                             (scan-source-token first-character-witness-parser
                                                source offset 0 #f)))
                 (check (list (token-kind actual) (token-lexeme actual)
                              (token-end actual) end)
                        => (list (token-kind expected) (token-lexeme expected)
                                 (token-end expected) (cadr match)))))
             (loop (+ offset 1)))))
       '("foo .25 1. 0xAB 0b10 hexAB 12_345M"
         "\"a\"\"b\" 'c' @d\\@e@ @unterminated"
         "αβ λ1 ٣.٢" "<<EOF\nbody\nEOF\n" "plain < > .")))))
(export lexical-first-character-test)

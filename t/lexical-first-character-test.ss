(import (only-in :clan/poo/type False)
        (only-in :clan/poo/mop TypeError?))
(import :gerbil-parser/language-test-support)
(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; Differential witness for admission filters on nonregular lexical rules.
(import :std/test
        :gerbil-parser/src/language/grammar
        (only-in :gerbil-parser/src/runtime/machine lexical-dispatch/ranked)
        (only-in :gerbil-parser/src/runtime/lexical-lexer prepare-generated-lexer)
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

(deflanguage-parser-tests lexical-first-character-test "closed lexical first-character admission"
  (engine)
    (scenario "prepared competition preserves evaluation order and empty projections"
      (scope ((observed '()))
(locals ((rule (lambda ( name rank first? extent)
          (vector (lambda (terminals) #t)
                  (lambda (source offset)
                    (set! observed (cons name observed))
                    (let (end (extent source offset))
                      (and end (list name end rank))))
                  #f name rank first? #f))))

        (scope-values (((lexer plans certificates)
                      (prepare-generated-lexer
                       (list
                        (rule 'first 0 char-alphabetic? (lambda (s o) (+ o 1)))
                        (rule 'long 0 (lambda (ch) (char=? ch #\a))
                              (lambda (s o) (and (char=? (string-ref s o) #\a) (+ o 2))))
                        (rule 'priority 9 char-alphabetic? (lambda (s o) (+ o 1))))
                       (lambda () (error "unused certificate preparation")) '#())))
          (each (row) ('(("ab" (long "ab" 7 9 2) (priority long first))
             ("b" (priority "b" 7 8 1) (priority first))
             ("α" (priority "α" 7 9 1) (priority long first))))
             (act (set! observed '()))
             (scope-values (((token end) (lexer (car row) 0 7 #f)))
               (equal (list (token-kind token) (token-lexeme token)
                            (token-start token) (token-end token) end)
                       (cadr row)))
             (equal (reverse observed)  (caddr row)))
          (act (set! observed '()))
          (raises (lexer "?" 0 0 #f) true)
          (equal observed  '())))))
    (scenario "engine token bytes preserve 1-4 byte scalars NUL and combining marks"
      (sequential ((source (string-append "Aé中😀" (string #\nul) "e" (string (integer->char #x301))))
             (scanner (make-source-scanner False False source #f
                        (lambda (text start context _mode)
                          (values (and (< start (string-length text)) 'scalar)
                                  (+ start 1) context)))))
        (raises (source-scanner-step scanner (source-scanner-initial-state scanner) 'scalar) TypeError?)
        (iterate loop ((state (source-scanner-initial-state scanner))
                   (index 0) (byte-start 0) (widths '(1 2 3 4 1 1 2)))
          (branch (null? widths)
            (scope-values (((token final) (source-scanner-step scanner state #f)))
              (equal token  #f)
              (equal (source-scan-state-character-offset final)  7)
              (equal (source-scan-state-byte-offset final)  14))
            (sequential ((byte-end (+ byte-start (car widths)))
                   (direct (scan-emit source 'scalar index (+ index 1) (+ 7 byte-start))))
              (equal (token-start direct)  (+ 7 byte-start))
              (equal (token-end direct)  (+ 7 byte-end))
              (scope-values (((token next) (source-scanner-step scanner state #f)))
                (equal (token-kind token)  'scalar)
                (equal (token-lexeme token)  (substring source index (+ index 1)))
                (equal (token-start token)  byte-start)
                (equal (token-end token)  byte-end)
                (continue loop next (+ index 1) byte-end (cdr widths))))))))
    (scenario "filtered scanner preserves maximal munch at every offset"
      (each (source) ('("foo .25 1. 0xAB 0b10 hexAB 12_345M"
         "\"a\"\"b\" 'c' @d\\@e@ @unterminated"
         "αβ λ1 ٣.٢" "<<EOF\nbody\nEOF\n" "plain < > ."))
         (iterate loop ((offset 0))
           (when (< offset (string-length source))
             (sequential ((match (reference-match source offset))
                    (expected (scan-emit source (car match) offset (cadr match) 0)))
               (scope-values (((actual end)
                             (scan-source-token first-character-witness-parser
                                                source offset 0 #f)))
                 (equal (list (token-kind actual) (token-lexeme actual)
                              (token-end actual) end)
                         (list (token-kind expected) (token-lexeme expected)
                                 (token-end expected) (cadr match)))))
             (continue loop (+ offset 1)))))))

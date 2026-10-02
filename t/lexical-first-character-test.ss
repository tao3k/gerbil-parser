;;; Differential witness for admission filters on nonregular lexical rules.
(import :std/test
        :gerbil-parser/src/language/grammar
        (only-in :gerbil-parser/src/compiler/machine lexical-dispatch/ranked)
        (only-in :gerbil-parser/src/runtime/lexer scan-source-token)
        (only-in :gerbil-parser/src/runtime/scan scan-emit)
        (only-in :gerbil-parser/src/runtime/token token-kind token-lexeme token-end))

(deflanguage first-character-witness
  (identity "first-character-witness" "v1" "first-character-witness.v1")
  (root source-file)
  (lex
   (numeric Numeric (number-literal ("0x" "0b" "hex") "_" ("M") #t #t))
   (quoted Quoted (quoted-string "\"" "'"))
   (escaped Escaped (escaped-quoted-string "@"))
   (here Here (heredoc))
   (name Name (identifier))
   (other Other (fallback)))
  (rules
   (source-file (node SourceFile
     (repeat (choice numeric quoted escaped here name other)))))
  (extras) (keywords) (recoveries) (conflicts reject) (case-insensitive #f))

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

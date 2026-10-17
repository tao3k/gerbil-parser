;;; -*- Gerbil -*-
;;; Declarative pinned corpus and generated native-route qualification.
(import :gerbil-parser/language-test-support ./parser)
(import (only-in :clan/poo/object .o)
        (only-in :gerbil-parser/language-support/fixture defsyntax-corpus defsyntax-fixture)
        (only-in :gerbil-parser/language-support/development deflanguage-development-loader LanguageDevelopmentLoader.)
        (only-in :gerbil-parser/language-build-support declare-language-build-strategy make-fused-reduction-strategy make-rust-runtime-strategy)
        (only-in :gerbil-parser/src/compiler/hcl-source direct-parse-hcl direct-lex-hcl direct-hcl-grammar-digest))
(export hcl-representative-fixture hcl-official-fixtures hcl-test-language)

(defsyntax-fixture hcl-representative-fixture
  (identity "hcl/v2.24.0/representative"
            "hcl" +hcl-native-syntax-version+ +hcl-syntax-contract+)
  (source "corpus/representative.hcl")
  (expect accepted HclFile
          (Block Attribute TupleExpression ObjectExpression
                 TraversalExpression)))

(defsyntax-corpus hcl-official-fixtures
  (identity "hcl" +hcl-native-syntax-version+ +hcl-syntax-contract+)
  (accepted
   ("hcl/specsuite/comments/hash" hcl-spec-hash-comment
    "corpus/reference/comments/hash_comment.hcl" HclFile ())
   ("hcl/specsuite/comments/multiline" hcl-spec-multiline-comment
    "corpus/reference/comments/multiline_comment.hcl" HclFile ())
   ("hcl/specsuite/comments/slash" hcl-spec-slash-comment
    "corpus/reference/comments/slash_comment.hcl" HclFile ())
   ("hcl/specsuite/empty" hcl-spec-empty
    "corpus/reference/empty.hcl" HclFile ())
   ("hcl/specsuite/expressions/heredoc" hcl-spec-heredoc
    "corpus/reference/expressions/heredoc.hcl"
    HclFile (Attribute ObjectExpression HeredocExpression))
   ("hcl/specsuite/expressions/operators" hcl-spec-operators
    "corpus/reference/expressions/operators.hcl"
    HclFile (Block Attribute BinaryExpression ConditionalExpression))
   ("hcl/specsuite/expressions/primitive-literals" hcl-spec-primitives
    "corpus/reference/expressions/primitive_literals.hcl"
    HclFile (Attribute NumberExpression StringExpression LiteralExpression))
   ("hcl/specsuite/structure/attributes/expected" hcl-spec-attributes-expected
    "corpus/reference/structure/attributes_expected.hcl"
    HclFile (Attribute StringExpression))
   ("hcl/specsuite/structure/attributes/unexpected" hcl-spec-attributes-unexpected
    "corpus/reference/structure/attributes_unexpected.hcl"
    HclFile (Attribute StringExpression))
   ("hcl/specsuite/structure/blocks/empty-oneline" hcl-spec-block-empty-oneline
    "corpus/reference/structure/block_empty_oneline.hcl"
    HclFile (Block))
   ("hcl/specsuite/structure/blocks/empty-multiline" hcl-spec-block-empty-multiline
    "corpus/reference/structure/block_empty_multiline.hcl"
    HclFile (Block))
   ("hcl/specsuite/structure/blocks/single-oneline" hcl-spec-block-single-oneline
    "corpus/reference/structure/block_single_oneline.hcl"
    HclFile (Block Attribute StringExpression)))
  (rejected
   ("hcl/specsuite/structure/attributes/singleline-bad"
    hcl-spec-attribute-singleline-bad
    "corpus/reference/invalid/attribute_singleline_bad.hcl")
   ("hcl/specsuite/structure/blocks/single-oneline-invalid"
    hcl-spec-block-single-oneline-invalid
    "corpus/reference/invalid/block_single_oneline_invalid.hcl")
   ("hcl/specsuite/structure/blocks/single-unclosed"
    hcl-spec-block-single-unclosed
    "corpus/reference/invalid/block_single_unclosed.hcl")))
(deflanguage-development-loader hcl-test-language
  (grammar hcl-language-grammar)
  (parse parse-hcl-test)
  (slots metadata: `((grammar-format . concise-dsl) (reference-commit . ,+hcl-native-syntax-commit+))
         build-strategies: (list (declare-language-build-strategy 'fused-reductions
                                   (make-fused-reduction-strategy hcl-language-grammar))
                                 (declare-language-build-strategy 'rust-runtime
                                   (make-rust-runtime-strategy hcl-language-grammar)))
         fixtures: hcl-official-fixtures
         native-test-profile: (.o profile: 'recursive-source
                                  digest: direct-hcl-grammar-digest
                                  source: direct-parse-hcl lexer: direct-lex-hcl)))

(deflanguage-parser-tests hcl-parser-test "HCL native syntax v2.24.0 official corpus"
  (loader hcl-test-language)
  (installed "HCL machine installs the generated fused step" step)
  (installed "HCL machine installs the Grammar IR recursive source parser" source)
  (strategy-parity "the closed ASCII lexer agrees with canonical tokens"
    (append (sources "key0 = 1\n" "a-b = 123\r\n" "a = 1.2\n"
           "a == 1\n" "a = \"text\"\n" "é = 1\n")
      (seeded 1729 512 (repeat 0 32 (character "abcXYZ_eE0123= \t\r\n"))))
    (routes) (lexical optional) (coverage 64 0))
  (strategy-parity "the 1024-line Basic source retains its exact artifact"
    (indexed 1024 (concat "key" index " = 1\n"))
    (routes entry source-default source-raw source-generic indexed) (accepted #t))
  (strategy-parity "ASCII quoted strings retain ranked tokens and artifacts"
    (sources "name = \"value\"\n"
         "name = \"a\\\"b\"\n"
         "name = \"a\"\"b\"\n"
         "name = \"// not a comment\"\n"
         "name = \"a\nb\"\n")
    (routes entry source-ranked source-raw indexed) (lexical fast) (accepted #t))
  (strategy-parity "unsupported quoted strings fall back to ranked lexing"
    (sources "name = \"雪\"\n" "name = \"unterminated\n")
    (routes source-default source-ranked) (lexical fallback))
  (strategy-parity "generated quoted-string tokens match ranked scanning"
    (seeded 3857 128 (concat "key = \"" (repeat 0 12 (choice "a" "0" " " "\\\\" "\\\"" "//")) "\"\n"))
    (routes source-default source-ranked) (lexical fast) (accepted #t))
  (strategy-parity "1024 ASCII string lines retain the indexed artifact"
    (indexed 1024 (concat "key" index " = \"value\"\n"))
    (routes entry source-ranked source-raw indexed) (accepted #t))
  (strategy-parity "ASCII comment forms retain ranked tokens and artifacts"
    (sources "# heading\nkey = 1\n"
         "key=1 // trailing\r\n"
         "key = 1 /* block\n comment */\n"
         "key = \"// value\" # comment\n"
         "/* leading */\nkey = 1\n")
    (routes entry source-ranked source-raw indexed) (lexical fast) (accepted #t))
  (strategy-parity "Unicode and unterminated comments use ranked fallback"
    (sources "key = 1 # 雪\n" "key = 1 /* unclosed")
    (routes source-default source-ranked) (lexical fallback))
  (strategy-parity "generated comment tokens match ranked scanning"
    (seeded 7103 128
      (bind ((start (choice "#" "//" "/*")))
        (concat "key = 1 " start (repeat 0 16 (character "ab09 #/\t"))
                (when-equal start "/*" "*/") "\r\n")))
    (routes source-default source-ranked) (lexical fast) (accepted #t))
  (strategy-parity "1024 commented lines retain the indexed artifact"
    (indexed 1024 (concat "key" index " = 1 # note\n"))
    (routes entry source-ranked source-raw indexed) (accepted #t))
  (strategy-parity "ASCII fractional and exponent numbers retain artifacts"
    (sources "key = 0.5\n" "key=12e3\n" "key=1.25E-3\n"
         "key=7e+1\n" "key=4.0 # comment\n")
    (routes entry source-ranked source-raw indexed) (lexical fast) (accepted #t))
  (strategy-parity "numeric boundaries and seeded literals match ranked scanning"
    (sources "key=1e\n" "key=1e+\n" "key=1.\n"
         "key=1.2.3\n" "key=1e2foo\n" "key=雪2\n")
    (routes source-default source-ranked) (lexical optional))
  (strategy-parity "seeded numeric literals retain fast tokens and accepted artifacts"
    (seeded 7207 128 (concat "key = " (integer 1 999) "." (integer 0 1000)
                               (if-even "e+" "E-") (integer 0 30) "\n"))
    (routes source-default source-ranked) (lexical fast) (accepted #t))
  (strategy-parity "1024 decimal lines retain the indexed artifact"
    (indexed 1024 (concat "key" index " = 1.25\n"))
    (routes entry source-ranked source-raw indexed) (accepted #t))
  (strategy-parity "simple attributes and late complex fallback retain artifacts"
    (sources "" "\n\n" "name = \"雪\"\r\n" "flag = true\n"
         "é = 2\n" "# comment\nfoo = 2\n"
         "foo = 1 # comment\nbar = \"x\"\n"
         "plain = 1\ncomplex = [1, 2]\n")
    (routes entry source-default source-raw source-generic indexed) (accepted #t))
  (strategy-parity "mixed simple attribute corridor matches generic events"
    (seeded 4919 128
      (repeat 1 8 (concat (choice "a" "foo" "é" "name_2") " = "
                          (choice "1" "23" "foo" "true" "\"x\"" "\"雪\"")
                          (choice "\n" "\r\n" " # comment\n"))))
    (routes source-generic source-default) (accepted #t))
  (strategy-parity "compact HCL preserves canonical and rollback event paths"
    (indexed 128 (concat "key" index "=1\n"))
    (routes entry source-raw indexed) (accepted #t))
  (strategy-parity "closed flat blocks retain ranked tokens and full artifacts"
    (sources "srv {}\n"
         "srv { key = 1 }\n"
         "srv \"web\" { key = 1 }\n"
         "srv \"web\" internal { key = 1 }\n"
         "srv { child { key = 1 } }\n"
         "srv { child { grandchild { key = 1 } } }\n"
         "srv { /* lead */ child { key = 1 } /* trail */ }\n"
         "srv \"web\" { child \"db\" {\r\n key = 1\r\n }\r\n}\n"
         "srv { child {}\n another { key = 2 } }\n"
         "srv /* between */ \"web\" /* label */ internal {\n}\n"
         "srv {\n key = 1\n port = 2\n}\n"
         "# before\nsrv {\r\n name = \"x\" // after\r\n}\r\n"
         "one {\n}\ntwo {\n x = 1.25e+3\n}\n")
    (routes entry source-generic indexed) (lexical fast) (accepted #t))
  (strategy-parity "Unicode block label uses ranked lexer and exact byte offsets"
    (sources "srv \"λ\" internal { child \"μ\" { key = 1 } }\n")
    (routes entry source-generic indexed) (lexical fallback) (accepted #t))
  (strategy-parity "complex nested values and mixed roots keep generic fallback"
    (sources "srv { child { key = [1, 2] } }\n"
         "top = 1\nsrv { key = 2 }\n")
    (routes entry source-generic indexed) (accepted #t))
  (strategy-parity "1024 flat-block lines retain the indexed artifact"
    (append
      (indexed 256 (concat "srv" index " {\nkey = 1\nport = 2\n}\n"))
      (indexed 256 (concat "srv" index " \"web\" internal {\nkey = 1\nport = 2\n}\n")))
    (routes entry source-generic indexed) (lexical fast) (accepted #t))
  (strategy-parity "1024 nested-block lines retain the indexed artifact"
    (indexed 256 (concat "outer" index " \"env\" {\nsrv" index
                                 " \"web\" internal {\nkey = 1\n}}\n"))
    (routes entry source-generic indexed) (lexical fast) (accepted #t))
  (strategy-parity "generated events preserve Unicode byte offsets and trivia"
    (sources "名称 = \"λ中😀\"\n# 注释\n")
    (routes entry indexed) (accepted #t))
  (property "the upstream syntax identity is immutable" (bindings)
    (equal +hcl-native-syntax-version+ "v2.24.0")
    (equal +hcl-native-syntax-commit+ "6b5068090eef06b1f127f61529db5ba0be7ed343")
    (equal +hcl-syntax-contract+ "hcl-native-v2.24.0.v1"))
  (fixture-catalog "official HCL fixture counts" (total 15) (accepted 12) (rejected 3))
  (identity "Loader retains the pinned grammar identity"
    (schema "gerbil-parser.language-entry.v2") (language "hcl")
    (version +hcl-native-syntax-version+) (contract +hcl-syntax-contract+))
  (fixture-parity "complete official HCL specsuite sources parse losslessly" accepted
    (routes entry source-default source-raw indexed) (accepted #t))
  (fixture-parity "official invalid HCL sources fail as one typed artifact" rejected
    (routes source-default) (accepted #f) (diagnostics 1)))

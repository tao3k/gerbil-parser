;;; -*- Gerbil -*-
;;; Declarative BNF/Bound IR identity, static conflict controls and syntax.
(import :gerbil-parser/language-test-support ./parser)
(deflanguage-parser-tests opencypher-parser-test "openCypher 2024.1 generated parser"
  (loader opencypher-language)
  (parser-ir "complete frozen BNF" opencypher-parser-ir
    (rule-count 352) (root-rule 'program) (materialization 'aot-expansion))
  (bound-ir "bound declaration catalog" opencypher-bound-grammar-ir (binding-count 736))
  (bound-rules "source and normalization identities" opencypher-bound-grammar-ir
    (program (source path "grammar-source/openCypher.bnf") (source line 3)
      (references '(((package . gerbil-parser) (grammar . opencypher-grammar)
                     (namespace . rule) (name . |procedure specification|))
                    ((package . gerbil-parser) (grammar . opencypher-grammar)
                     (namespace . rule) (name . |standalone procedure call|)))))
    (|linear statement| (source compatibilityOverlay +opencypher-linear-result-overlay+)
      (source-prefix sourceExpressionDigest "sha256:")
      (source-prefix replacementExpressionDigest "sha256:"))
    (|arithmetic unary| (source compatibilityOverlay +opencypher-nullable-prefix-normalization+)
      (source-prefix replacementExpressionDigest "sha256:"))
    (|boolean factor| (source compatibilityOverlay +opencypher-nullable-prefix-normalization+)
      (source-prefix replacementExpressionDigest "sha256:"))
    (|pattern source| (source compatibilityOverlay +opencypher-nullable-prefix-normalization+)
      (source-prefix replacementExpressionDigest "sha256:"))
    (|list value constructor| (source compatibilityOverlay +opencypher-nullable-prefix-normalization+)
      (source-prefix replacementExpressionDigest "sha256:"))
    (|list literal| (source compatibilityOverlay +opencypher-nullable-prefix-normalization+)
      (source-prefix replacementExpressionDigest "sha256:"))
    (|general literal| (source compatibilityOverlay +opencypher-constructor-deduplication+)
      (source-prefix replacementExpressionDigest "sha256:"))
    (|boolean literal| (source compatibilityOverlay +opencypher-literal-keyword-preference+)
      (source-prefix replacementExpressionDigest "sha256:"))
    (|null literal| (source compatibilityOverlay +opencypher-literal-keyword-preference+)
      (source-prefix replacementExpressionDigest "sha256:"))
    (|graph reference| (source compatibilityOverlay +opencypher-qualified-reference-normalization+)
      (source-prefix replacementExpressionDigest "sha256:"))
    (|procedure reference| (source compatibilityOverlay +opencypher-qualified-reference-normalization+)
      (source-prefix replacementExpressionDigest "sha256:"))
    (|function reference| (source compatibilityOverlay +opencypher-qualified-reference-normalization+)
      (source-prefix replacementExpressionDigest "sha256:"))
    (|non-reserved word| (source precedenceOverlay +opencypher-non-reserved-word-preference+)
      (source-prefix replacementExpressionDigest "sha256:")))
  (lr "static normalization removes nullable prefixes and retains admitted forks" opencypher-parser-ir
    (state-count 1317) (fork-count 321)
    (empty-nullable-prefixes "$arithmetic unary" "$boolean factor" "$pattern source"
                             "$list value constructor" "$list literal")
    (initial-shifts "UNWIND" "WITH" "RETURN"))
  (fixtures "representative and negative fixture conformance")
  (accepted-many "native identifier and numeric forms"
    '("MATCH (`a``b`) RETURN `a``b`\n" "RETURN 1 AS one\n" "RETURN $1 AS value\n"
      "MERGE (n:L) ON CREATE SET n.x = 1 ON MATCH SET n.x = 2 RETURN n\n"
      "MERGE (a)-[r:R]-(b) RETURN r\n" "RETURN 1 AS n UNION ALL RETURN 2 AS n\n"
      "RETURN false = true IS NULL AS value\n"
      "UNWIND [0xFF, 0o77, .5, 1.25e+2, 3.0F] AS n RETURN n\n"))
  (rejected-many "foreign numeric forms remain rejected"
    '("UNWIND [0b101] AS n RETURN n\n" "UNWIND [1.] AS n RETURN n\n"))
  (lr-receipt "300 boolean items complete without speculative paths" opencypher-parser
    (string-append "RETURN [" (string-join (make-list 300 "true") ", ") "]") 1
    (rest '()) (branchesExplored 0) (speculativeBranchesExplored 0)
    (maxSpeculativeDepth 0) (successfulCompletions 1)))

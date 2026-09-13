;;; -*- Gerbil -*-
;;; openCypher 2024.1 official ISO WG3 BNF grammar-source owner.

(import :gerbil-parser/language-support)
(export +opencypher-version+
        +opencypher-commit+
        +opencypher-bnf-digest+
        +opencypher-linear-result-overlay+
        +opencypher-numeric-parameter-overlay+
        +opencypher-merge-actions-overlay+
        +opencypher-undirected-merge-overlay+
        +opencypher-union-all-overlay+
        +opencypher-advanced-predicate-precedence-overlay+
        +opencypher-qualified-reference-normalization+
        +opencypher-nullable-prefix-normalization+
        +opencypher-constructor-deduplication+
        +opencypher-syntax-contract+
        +opencypher-representative-query+
        opencypher-2024-1-bnf
        opencypher-2024-1-language-grammar
        opencypher-2024-1-grammar
        opencypher-2024-1-bound-grammar-ir
        opencypher-2024-1-parser-ir
        opencypher-2024-1-parser)

(def +opencypher-version+ "2024.1")
(def +opencypher-commit+
  "30b451d3b7c94ee5a84a0fdc223947a442dd9493")
(def +opencypher-bnf-digest+
  "sha256:c0b5454f001b59b401756158bf88e27847c8ace71f1abc8df1e05f8b710b9f50")
(def +opencypher-linear-result-overlay+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . linear-result-only)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-numeric-parameter-overlay+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . numeric-parameter)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-merge-actions-overlay+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . repeated-merge-actions)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-undirected-merge-overlay+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . undirected-merge-relationship)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-union-all-overlay+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . union-all)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-advanced-predicate-precedence-overlay+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . advanced-predicate-precedence)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-qualified-reference-normalization+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . qualified-reference-left-factoring)
    (kind . normalization)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-nullable-prefix-normalization+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . nullable-prefix-left-factoring)
    (kind . normalization)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-constructor-deduplication+
  '((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
    (namespace . opencypher)
    (name . value-constructor-deduplication)
    (kind . normalization)
    (sourceVersion . "2024.1")
    (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493")))
(def +opencypher-syntax-contract+ "opencypher-2024.1-syntax.v1")
(def +opencypher-representative-query+ "MATCH (n) RETURN n\n")

(defsyntax-iso-bnf-source opencypher-2024-1-bnf
  (identity "opencypher" +opencypher-version+ +opencypher-commit+)
  (digest +opencypher-bnf-digest+)
  (source "grammar-source/openCypher.bnf"))

(deflanguage-iso-bnf-grammar opencypher-2024-1
  (identity "opencypher" "2024.1" "opencypher-2024.1-syntax.v1")
  (reference "2024.1" "30b451d3b7c94ee5a84a0fdc223947a442dd9493")
  (digest "sha256:c0b5454f001b59b401756158bf88e27847c8ace71f1abc8df1e05f8b710b9f50")
  (source "grammar-source/openCypher.bnf")
  ;; The pinned source defines ISO ellipsis correctly as one-or-more, but its
  ;; linear statement production consequently excludes result-only queries.
  ;; Same-commit TCK scenarios require `RETURN ...`; this overlay is exact-
  ;; source checked and published in Bound IR rather than mutating the BNF.
  (rule-overrides
   (|linear statement|
    ((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
     (namespace . opencypher)
     (name . linear-result-only)
     (sourceVersion . "2024.1")
     (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493"))
    (alias |linear statement|
     (sequence
      (repeat1 (reference |primitive statement|))
      (optional (reference |primitive result statement|))))
    (alias |linear statement|
     (choice
      (reference |primitive result statement|)
      (reference |primitive statement|)
      (sequence
       (reference |primitive statement|)
       (reference |linear statement|)))))
   (|parameter name|
    ((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
     (namespace . opencypher)
     (name . numeric-parameter)
     (sourceVersion . "2024.1")
     (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493"))
    (alias |parameter name| (reference |separated identifier|))
    (alias |parameter name|
     (choice (reference |separated identifier|) (token number))))
   (|merge statement|
    ((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
     (namespace . opencypher)
     (name . repeated-merge-actions)
     (sourceVersion . "2024.1")
     (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493"))
    (alias |merge statement|
     (sequence
      (literal "MERGE")
      (reference |merge graph pattern|)
      (optional (reference |merge action|))))
    (alias |merge statement|
     (sequence
      (literal "MERGE")
      (reference |merge graph pattern|)
      (repeat (reference |merge action|)))))
   (|merge relationship pattern|
    ((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
     (namespace . opencypher)
     (name . undirected-merge-relationship)
     (sourceVersion . "2024.1")
     (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493"))
    (alias |merge relationship pattern|
     (choice
      (reference |merge relationship pointing left|)
      (reference |merge relationship pointing right|)))
    (alias |merge relationship pattern|
     (choice
      (reference |merge relationship pointing left|)
      (reference |merge relationship pointing right|)
      (sequence
       (reference |arrow line|)
       (reference |left bracket|)
       (reference |merge relationship pattern filler|)
       (reference |right bracket|)
       (reference |arrow line|)))))
   (|composite conjunction|
    ((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
     (namespace . opencypher)
     (name . union-all)
     (sourceVersion . "2024.1")
     (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493"))
    (alias |composite conjunction|
     (sequence (literal "UNION") (optional (literal "DISTINCT"))))
    (alias |composite conjunction|
     (sequence
      (literal "UNION")
      (optional (choice (literal "DISTINCT") (literal "ALL"))))))
   (|simple comparison predicate part 2|
    ((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
     (namespace . opencypher)
     (name . advanced-predicate-precedence)
     (sourceVersion . "2024.1")
     (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493"))
    (alias |simple comparison predicate part 2|
     (sequence
      (reference |simple comp op|)
      (reference |advanced comparison predicand|)))
    (alias |simple comparison predicate part 2|
     (sequence
      (reference |simple comp op|)
      (reference |simple comparison predicand|))))
   (|arithmetic unary|
    ((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
     (namespace . opencypher)
     (name . nullable-prefix-left-factoring)
     (kind . normalization)
     (sourceVersion . "2024.1")
     (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493"))
    (alias |arithmetic unary|
     (sequence
      (optional (reference sign))
      (reference |postfix expression|)))
    (alias |arithmetic unary|
     (choice
      (reference |postfix expression|)
      (sequence
       (reference sign)
       (reference |postfix expression|)))))
   (|boolean factor|
    ((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
     (namespace . opencypher)
     (name . nullable-prefix-left-factoring)
     (kind . normalization)
     (sourceVersion . "2024.1")
     (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493"))
    (alias |boolean factor|
     (sequence
      (optional (repeat1 (literal "NOT")))
      (reference |boolean primary|)))
    (alias |boolean factor|
     (choice
      (reference |boolean primary|)
      (sequence
       (repeat1 (literal "NOT"))
       (reference |boolean primary|)))))
   (|pattern source|
    ((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
     (namespace . opencypher)
     (name . nullable-prefix-left-factoring)
     (kind . normalization)
     (sourceVersion . "2024.1")
     (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493"))
    (alias |pattern source|
     (sequence
      (optional
       (sequence
        (reference |binding variable|)
        (reference |equals operator|)))
      (reference |simple path pattern|)))
    (alias |pattern source|
     (choice
      (reference |simple path pattern|)
      (sequence
       (reference |binding variable|)
       (reference |equals operator|)
       (reference |simple path pattern|)))))
   (|list value constructor|
    ((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
     (namespace . opencypher)
     (name . nullable-prefix-left-factoring)
     (kind . normalization)
     (sourceVersion . "2024.1")
     (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493"))
    (alias |list value constructor|
     (sequence
      (reference |left bracket|)
      (optional (reference |list element list|))
      (reference |right bracket|)))
    (alias |list value constructor|
     (choice
      (sequence
       (reference |left bracket|)
       (reference |right bracket|))
      (sequence
       (reference |left bracket|)
       (reference |list element list|)
       (reference |right bracket|)))))
   (|list literal|
    ((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
     (namespace . opencypher)
     (name . nullable-prefix-left-factoring)
     (kind . normalization)
     (sourceVersion . "2024.1")
     (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493"))
    (alias |list literal|
     (sequence
      (reference |left bracket|)
      (optional (reference |list element list literal|))
      (reference |right bracket|)))
    (alias |list literal|
     (choice
      (sequence
       (reference |left bracket|)
       (reference |right bracket|))
      (sequence
       (reference |left bracket|)
       (reference |list element list literal|)
       (reference |right bracket|)))))
   (|general literal|
    ((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
     (namespace . opencypher)
     (name . value-constructor-deduplication)
     (kind . normalization)
     (sourceVersion . "2024.1")
     (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493"))
    (alias |general literal|
     (choice
      (reference |boolean literal|)
      (reference |character string literal|)
      (reference |null literal|)
      (reference |list literal|)
      (reference |map literal|)))
    (alias |general literal|
     (choice
      (reference |boolean literal|)
      (reference |character string literal|)
      (reference |null literal|))))
   (|graph reference|
    ((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
     (namespace . opencypher)
     (name . qualified-reference-left-factoring)
     (kind . normalization)
     (sourceVersion . "2024.1")
     (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493"))
    (alias |graph reference|
     (sequence
      (optional (reference |catalog object parent reference|))
      (reference |graph name|)))
    (alias |graph reference|
     (choice
      (reference |graph name|)
      (sequence
       (reference |catalog object parent reference|)
       (reference |graph name|)))))
   (|procedure reference|
    ((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
     (namespace . opencypher)
     (name . qualified-reference-left-factoring)
     (kind . normalization)
     (sourceVersion . "2024.1")
     (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493"))
    (alias |procedure reference|
     (sequence
      (optional (reference |catalog object parent reference|))
      (reference |procedure name|)))
    (alias |procedure reference|
     (choice
      (reference |procedure name|)
      (sequence
       (reference |catalog object parent reference|)
       (reference |procedure name|)))))
   (|function reference|
    ((schema . "gerbil-parser.iso-bnf-rule-overlay.v1")
     (namespace . opencypher)
     (name . qualified-reference-left-factoring)
     (kind . normalization)
     (sourceVersion . "2024.1")
     (upstreamCommit . "30b451d3b7c94ee5a84a0fdc223947a442dd9493"))
    (alias |function reference|
     (sequence
      (optional (reference |catalog object parent reference|))
      (reference |function name|)))
    (alias |function reference|
     (choice
      (reference |function name|)
      (sequence
       (reference |catalog object parent reference|)
       (reference |function name|))))))
  (entrypoint program)
  (conflicts selective-glr)
  (case-insensitive #t))

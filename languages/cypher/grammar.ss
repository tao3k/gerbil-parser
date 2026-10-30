;;; -*- Gerbil -*-
;;; openCypher 2024.1 official ISO WG3 BNF grammar-source owner.

(import (only-in :gerbil-parser/language-support/grammar deflanguage)
        (only-in :gerbil-parser/language-support/iso-bnf-language
                 iso-bnf))
(export opencypher-syntax opencypher-grammar opencypher-bound-grammar-ir
        opencypher-parser-ir opencypher-parser)

(deflanguage opencypher
 (syntax (iso-bnf
  (reference "2024.1" "30b451d3b7c94ee5a84a0fdc223947a442dd9493")
  (digest "sha256:c0b5454f001b59b401756158bf88e27847c8ace71f1abc8df1e05f8b710b9f50")
  (source "grammar-source/openCypher.bnf")
  (rule-precedences
   (|non-reserved word| (disambiguation non-reserved-word-preference) left -1))
  (entrypoint program)
  (conflicts selective-glr)
  (case-insensitive #t)))
 (rules
  (|linear statement| linear-result-only
 (node |linear statement|
     (seq
      (repeat1 (reference |primitive statement|))
      (optional (reference |primitive result statement|))))
 (node |linear statement|
     (choice
      (reference |primitive result statement|)
      (reference |primitive statement|)
      (seq
       (reference |primitive statement|)
       (reference |linear statement|)))))
  (|parameter name| numeric-parameter
 (node |parameter name| (reference |separated identifier|))
 (node |parameter name|
     (choice (reference |separated identifier|) (token number))))
  (|merge statement| repeated-merge-actions
 (node |merge statement|
     (seq
      (literal "MERGE")
      (reference |merge graph pattern|)
      (optional (reference |merge action|))))
 (node |merge statement|
     (seq
      (literal "MERGE")
      (reference |merge graph pattern|)
      (repeat (reference |merge action|)))))
  (|merge relationship pattern| undirected-merge-relationship
 (node |merge relationship pattern|
     (choice
      (reference |merge relationship pointing left|)
      (reference |merge relationship pointing right|)))
 (node |merge relationship pattern|
     (choice
      (reference |merge relationship pointing left|)
      (reference |merge relationship pointing right|)
      (seq
       (reference |arrow line|)
       (reference |left bracket|)
       (reference |merge relationship pattern filler|)
       (reference |right bracket|)
       (reference |arrow line|)))))
  (|composite conjunction| union-all
 (node |composite conjunction|
     (seq (literal "UNION") (optional (literal "DISTINCT"))))
 (node |composite conjunction|
     (seq
      (literal "UNION")
      (optional (choice (literal "DISTINCT") (literal "ALL"))))))
  (|simple comparison predicate part 2| advanced-predicate-precedence
 (node |simple comparison predicate part 2|
     (seq
      (reference |simple comp op|)
      (reference |advanced comparison predicand|)))
 (node |simple comparison predicate part 2|
     (seq
      (reference |simple comp op|)
      (reference |simple comparison predicand|))))
  (|arithmetic unary| (normalization nullable-prefix-left-factoring)
 (node |arithmetic unary|
     (seq
      (optional (reference sign))
      (reference |postfix expression|)))
 (node |arithmetic unary|
     (choice
      (reference |postfix expression|)
      (seq
       (reference sign)
       (reference |postfix expression|)))))
  (|boolean factor| (normalization nullable-prefix-left-factoring)
 (node |boolean factor|
     (seq
      (optional (repeat1 (literal "NOT")))
      (reference |boolean primary|)))
 (node |boolean factor|
     (choice
      (reference |boolean primary|)
      (seq
       (repeat1 (literal "NOT"))
       (reference |boolean primary|)))))
  (|pattern source| (normalization nullable-prefix-left-factoring)
 (node |pattern source|
     (seq
      (optional
       (seq
        (reference |binding variable|)
        (reference |equals operator|)))
      (reference |simple path pattern|)))
 (node |pattern source|
     (choice
      (reference |simple path pattern|)
      (seq
       (reference |binding variable|)
       (reference |equals operator|)
       (reference |simple path pattern|)))))
  (|list value constructor| (normalization nullable-prefix-left-factoring)
 (node |list value constructor|
     (seq
      (reference |left bracket|)
      (optional (reference |list element list|))
      (reference |right bracket|)))
 (node |list value constructor|
     (choice
      (seq
       (reference |left bracket|)
       (reference |right bracket|))
      (seq
       (reference |left bracket|)
       (reference |list element list|)
       (reference |right bracket|)))))
  (|list literal| (normalization nullable-prefix-left-factoring)
 (node |list literal|
     (seq
      (reference |left bracket|)
      (optional (reference |list element list literal|))
      (reference |right bracket|)))
 (node |list literal|
     (choice
      (seq
       (reference |left bracket|)
       (reference |right bracket|))
      (seq
       (reference |left bracket|)
       (reference |list element list literal|)
       (reference |right bracket|)))))
  (|general literal| (normalization value-constructor-deduplication)
 (node |general literal|
     (choice
      (reference |boolean literal|)
      (reference |character string literal|)
      (reference |null literal|)
      (reference |list literal|)
      (reference |map literal|)))
 (node |general literal|
     (choice
      (reference |boolean literal|)
      (reference |character string literal|)
      (reference |null literal|))))
  (|value specification| (normalization parameter-path-deduplication)
 (node |value specification|
     (choice
      (reference |literal|)
      (reference |general parameter reference|)
      (reference |list value constructor|)
      (reference |map value constructor|)))
 (node |value specification|
     (choice
      (reference |literal|)
      (reference |list value constructor|)
      (reference |map value constructor|))))
  (|boolean literal| (disambiguation literal-keyword-preference)
 (node |boolean literal|
     (choice (literal "TRUE") (literal "FALSE")))
 (node |boolean literal|
     (choice
      (prec left 1 (literal "TRUE"))
      (prec left 1 (literal "FALSE")))))
  (|null literal| (disambiguation literal-keyword-preference)
 (node |null literal| (literal "NULL"))
 (node |null literal| (prec left 1 (literal "NULL"))))
  (|graph reference| (normalization qualified-reference-left-factoring)
 (node |graph reference|
     (seq
      (optional (reference |catalog object parent reference|))
      (reference |graph name|)))
 (node |graph reference|
     (choice
      (reference |graph name|)
      (seq
       (reference |catalog object parent reference|)
       (reference |graph name|)))))
  (|procedure reference| (normalization qualified-reference-left-factoring)
 (node |procedure reference|
     (seq
      (optional (reference |catalog object parent reference|))
      (reference |procedure name|)))
 (node |procedure reference|
     (choice
      (reference |procedure name|)
      (seq
       (reference |catalog object parent reference|)
       (reference |procedure name|)))))
  (|function reference| (normalization qualified-reference-left-factoring)
 (node |function reference|
     (seq
      (optional (reference |catalog object parent reference|))
      (reference |function name|)))
 (node |function reference|
     (choice
      (reference |function name|)
      (seq
       (reference |catalog object parent reference|)
       (reference |function name|)))))))

(import (only-in :gerbil-parser/language-support/grammar-source defsyntax-iso-bnf-source))
(export opencypher-bnf)

(defsyntax-iso-bnf-source opencypher-bnf
  (identity "opencypher" "2024.1" "30b451d3b7c94ee5a84a0fdc223947a442dd9493")
  (digest "sha256:c0b5454f001b59b401756158bf88e27847c8ace71f1abc8df1e05f8b710b9f50")
  (source "grammar-source/openCypher.bnf"))

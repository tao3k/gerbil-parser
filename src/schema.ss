;;; Shared schema and generator metadata. Source names never encode revisions.
;;; Format identities are data; compiler semantics are explicit metadata fields.

(def +grammar-schema+ "gerbil-parser.grammar.v1")
(def +grammar-ir-schema+ "gerbil-parser.grammar-ir.v1")
(def +parser-ir-schema+ "gerbil-parser.parser-ir.v1")
(def +parse-artifact-schema+ "gerbil-parser.parse-artifact.v1")
(def +diagnostic-schema+ "gerbil-parser.diagnostic.v1")
(def +parser-artifact-cache-schema+ "gerbil-parser.parser-artifact-cache.v1")
(def +language-declaration-cache-schema+ "gerbil-parser.language-declaration-cache.v1")
(def +lr-program-cache-schema+ "gerbil-parser.lr-program-cache.v1")
(def +language-grammar-schema+ "gerbil-parser.language-grammar.v1")

(def +language-abi-version+ 2)
(def +artifact-abi-version+ 1)
(def +event-abi-version+ 1)
(def +rust-aot-abi-version+ 1)
(def +abi-descriptor-schema+ "gerbil-parser.native-descriptor.v1")
(def +abi-error-schema+ "gerbil-parser.native-error.v1")
(def +rust-aot-error-schema+ "gerbil-parser.rust-runtime-aot-error.v1")

;; Both compiler cache layers depend on the same lowering semantics. Changing
;; a feature invalidates their keys without renaming an API or format schema.
(def +compiler-generator-revision+
  '((entry-root . authored-alias)
    (repetition-owner . exclusive)
    (nonterminal-vocabulary . declarations)
    (partition-conflict-origin . surviving-member)))

(def (compiler-generator-metadata kind)
  (let (name (case kind
                ((parser) 'gerbil-parser/lalr-generator)
                ((declaration) 'gerbil-parser/language-declaration-generator)
                ((recursive-source) 'gerbil-parser/hcl-attribute-source)
                (else (error "unknown compiler generator" kind))))
    (cons (cons 'name name)
          (if (eq? kind 'recursive-source)
            (cons '(parser-digest . "sha256:fcfa996f3d859f99e2e9a1c626acd5eb9c2de5ad308ab83db248b67e5dd1efeb")
                  +compiler-generator-revision+)
            +compiler-generator-revision+))))

(export +grammar-schema+ +grammar-ir-schema+ +parser-ir-schema+
        +parse-artifact-schema+ +diagnostic-schema+
        +parser-artifact-cache-schema+ +language-declaration-cache-schema+
        +lr-program-cache-schema+ +language-grammar-schema+
        +language-abi-version+ +artifact-abi-version+ +event-abi-version+
        +rust-aot-abi-version+ +abi-descriptor-schema+ +abi-error-schema+
        +rust-aot-error-schema+ compiler-generator-metadata)

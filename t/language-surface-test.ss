;;; -*- Gerbil -*-
;;; Boundary: executable witness for the concise v1 language authoring surface.
;;; Invariant: inferred declarations lower through deflanguage-grammar and
;;; publish the same Grammar IR, Parser IR, and ParseArtifact contracts.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/compiler/parser-ir parser-ir-ref)
        (only-in :gerbil-parser/src/compiler/bound-ir
                 bound-grammar-ir-binding
                 bound-grammar-ir-ref)
        (only-in :gerbil-parser/src/compiler/normalize grammar-ir-ref)
        (only-in :gerbil-parser/src/compiler/machine
                 lexical-choice lexical-dispatch lexical-dispatch/ranked
                 lexical-end)
        (only-in :gerbil-parser/src/language/grammar
                 deflanguage
                 deflanguage-grammar
                 defgrammar-syntax)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-roundtrip parse-artifact-success?)
        (only-in :gerbil-parser/src/runtime/scan
                 make-literal-end-scanner)
        (only-in :gerbil-parser/src/runtime/parser parse-source))

;;; These direct expansion witnesses keep the compiler's closed lexical algebra
;;; reviewable without adding alternate runtime scanner implementations.
(def lexical-end-witness
  (lexical-end "alpha" 0 (identifier)))
(def lexical-choice-witness
  (lexical-choice "alpha" 0 ((number) (identifier))))
(def lexical-dispatch-witness
  (lexical-dispatch "alpha" 0
                    ((number (number)) (identifier (identifier)))))
(def lexical-longest-witness
  (lexical-dispatch "alpha" 0
                    ((unknown (fallback)) (identifier (identifier)))))
(def lexical-precedence-witness
  (lexical-dispatch/ranked
   "+" 0
   ((low (precedence 1 (literals "+")))
    (high (precedence 10 (literals "+"))))))
(def (two-character-scanner source offset)
  (and (<= (+ offset 2) (string-length source)) (+ offset 2)))
(def external-scanner-witness
  (lexical-end "ab" 0 (external v1 two-character-scanner)))
(def literal-trie-witness
  (make-literal-end-scanner '("<" "<=" "MATCH" "MATCHED")))

(defgrammar-syntax (named-node token-name)
  (node SourceFile (field name token-name)))

(deflanguage concise-v1-witness
  (identity "concise-v1-witness" "v1" "concise-v1-witness.v1")
  (root source-file)
  (lex
   (whitespace Whitespace (whitespace+))
   (identifier Identifier (identifier))
   (unknown Unknown (fallback)))
  (rules
   (source-file
    (named-node identifier)))
  (extras whitespace)
  (keywords)
  (recoveries)
  (conflicts reject)
  (case-insensitive #f))

;;; Compatibility witness: the stable verbose v1 form remains executable while
;;; the concise surface is only an authoring projection over the same owner.
(deflanguage-grammar explicit-v1-witness
  (identity "explicit-v1-witness" "v1" "explicit-v1-witness.v1")
  (syntax-kinds
   (SourceFile node (name))
   (Identifier token (text))
   (Unknown token (text)))
  (terminals
   (identifier Identifier)
   (unknown Unknown))
  (lexical-rules
   (identifier (identifier))
   (unknown (fallback)))
  (rules
   (source-file
    (alias SourceFile (field name (token identifier)))))
  (extras)
  (keywords)
  (parser-entrypoints (source-file parse pure))
  (recoveries)
  (flow (source lexical) (lexical cst)))

(def concise-v1-language-surface-tests
  (test-suite "concise v1 language surface"
    (test-case "infers the stable grammar and parser contracts"
      (let* ((source "alpha")
             (artifact (parse-source concise-v1-witness-parser source)))
        (check (grammar-ir-ref concise-v1-witness-grammar 'schema)
               => "gerbil-parser.grammar-ir.v1")
        (check (grammar-ir-ref concise-v1-witness-grammar 'syntax-kinds)
               => '((SourceFile node (name))
                    (Whitespace token (text))
                    (Identifier token (text))
                    (Unknown token (text))))
        (check (parser-ir-ref concise-v1-witness-parser-ir 'root-rule)
               => 'source-file)
        (check (bound-grammar-ir-ref
                concise-v1-witness-bound-grammar-ir 'schema)
               => "gerbil-parser.bound-grammar-ir.v1")
        (check (bound-grammar-ir-ref
                concise-v1-witness-bound-grammar-ir 'expansionLineage)
               => '(deflanguage named-node))
        (check (bound-grammar-ir-ref
                concise-v1-witness-bound-grammar-ir 'bindingCount)
               => 15)
        (check (bound-grammar-ir-ref
                concise-v1-witness-bound-grammar-ir 'referenceCount)
               => 12)
        (let ((source-binding
               (bound-grammar-ir-binding
                concise-v1-witness-bound-grammar-ir 'rule 'source-file))
              (field-binding
               (bound-grammar-ir-binding
                concise-v1-witness-bound-grammar-ir 'field 'name 'SourceFile)))
          (check (bound-grammar-ir-ref source-binding 'references)
                 => '(((package . gerbil-parser)
                       (grammar . concise-v1-witness-grammar)
                       (namespace . terminal)
                       (name . identifier))))
          (check (cdr (assq 'path
                            (bound-grammar-ir-ref source-binding 'source)))
                 => (bound-grammar-ir-ref
                     concise-v1-witness-bound-grammar-ir 'originModule))
          (check (bound-grammar-ir-ref field-binding 'references)
                 => '(((package . gerbil-parser)
                       (grammar . concise-v1-witness-grammar)
                       (namespace . syntax-kind)
                       (name . SourceFile)))))
        (check (substring
                (bound-grammar-ir-ref
                 concise-v1-witness-bound-grammar-ir 'grammarDigest)
                0 7)
               => "sha256:")
        (check (parser-ir-ref explicit-v1-witness-parser-ir 'schema)
               => "gerbil-parser.parser-ir.v1")
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)))
    (test-case "internal lexical expansion preserves ordered dispatch"
      (check lexical-end-witness => 5)
      (check lexical-choice-witness => 5)
      (check lexical-dispatch-witness => '(identifier . 5))
      (check lexical-longest-witness => '(identifier . 5))
      (check lexical-precedence-witness => '(high 1 10))
      (check external-scanner-witness => 2)
      (check (literal-trie-witness "<=" 0) => 2)
      (check (literal-trie-witness "MATCHED suffix" 0) => 7)
      (check (literal-trie-witness "missing" 0) => #f))))

(export concise-v1-language-surface-tests)

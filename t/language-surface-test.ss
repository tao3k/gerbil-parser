(import :gerbil-parser/language-test-support)
(import (only-in :gerbil-parser/src/compiler/language-expander compile-language)
        (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; -*- Gerbil -*-
;;; Boundary: executable witness for the concise language authoring surface.
;;; Invariant: inferred and explicit declarations share canonical admission and
;;; publish the same Grammar IR, Parser IR, and ParseArtifact contracts.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/compiler/parser-ir parser-ir-ref)
        (only-in :gerbil-parser/src/compiler/bound-ir
                 bound-grammar-ir-binding
                 bound-grammar-ir-ref)
        (only-in :gerbil-parser/src/compiler/normalize grammar-ir-ref)
        (only-in :gerbil-parser/src/runtime/machine
                 lexical-choice lexical-dispatch lexical-dispatch/ranked
                 lexical-end install-parser-machine-backends!
                 parser-machine-grammar-digest parser-machine-direct-drive
                 parser-machine-direct-source parser-machine-backend-representation
                 parser-machine-for-current-semantic-backend install-parser-machine-direct-step!)
        (only-in :gerbil-parser/src/runtime/lr-parser current-lr-event-program-enabled?)
        (only-in :gerbil-parser/src/language/grammar
                 deflanguage
                 defgrammar-syntax)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-roundtrip parse-artifact-success?)
        (only-in :gerbil-parser/src/runtime/lexer lex-source)
        (only-in :gerbil-parser/src/runtime/scan
                 make-literal-end-scanner)
        (only-in :gerbil-parser/src/runtime/token
                 token-kind token-start token-end)
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

(begin
 (deflanguage concise-witness
  (syntax
   (lexical
    (root source-file)
    (lex
   (whitespace Whitespace (whitespace+))
   (identifier Identifier (identifier))
   (unknown Unknown (fallback)))
    (extras whitespace)
    (keywords)
    (recoveries)
    (conflicts reject)
    (case-insensitive #f)))
  (rules
   (source-file
    (named-node identifier))))
 (bind-fixture-grammar-release concise-witness "concise-witness" "v1" "concise-witness.v1") )

;;; Independent engine-IR control. This compiler entry is not a public author DSL.
(compile-language explicit-v1-witness
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

;;; A token/rule namespace collision is legal when references are explicit.
;;; Repeated node alternatives infer the union of their public fields.
(begin
 (deflanguage alternative-fields-witness
  (syntax
   (lexical
    (root source-file)
    (lex (word WordToken (identifier))
       (punctuation Punctuation (literals "+"))
       (unknown Unknown (fallback)))
    (extras)
    (keywords)
    (recoveries)
    (conflicts reject)
    (case-insensitive #f)
    (node-fields (Name reserved))))
  (rules
   (source-file (node Root (field value (reference word))))
   (word (choice (node Name (field left (token word)))
                 (node Name (field right (literal "+")))))))
 (bind-fixture-grammar-release alternative-fields-witness "alternative-fields" "v1" "alternative-fields.v1") )

;;; Fields in recursive transparent helpers belong to their enclosing node.
(begin
 (deflanguage transparent-fields-witness
  (syntax
   (lexical
    (root source-file)
    (lex (word WordToken (identifier))
       (punctuation Punctuation (literals "(" ")")))
    (extras)
    (keywords)
    (recoveries)
    (conflicts reject)
    (case-insensitive #f)))
  (rules
   (source-file (node Root helper))
   (helper (choice (field item word)
                   (seq "(" helper ")" (field tail word))))))
 (bind-fixture-grammar-release transparent-fields-witness "transparent-fields" "v1" "transparent-fields.v1") )

(deflanguage-parser-tests concise-language-surface-tests "concise v1 language surface"
  (engine)
    (scenario "transparent recursive references infer fields without crossing nodes"
      (equal (assq 'Root (grammar-ir-ref transparent-fields-witness-grammar 'syntax-kinds))
              '(Root node (item tail)))
      (equal (assq 'Root (grammar-ir-ref alternative-fields-witness-grammar 'syntax-kinds))
              '(Root node (value)))
      (each (source) ('("alpha" "(beta)gamma" "((a)b)c"))
         (scope ((artifact (parse-source transparent-fields-witness-parser source)))
           (equal (parse-artifact-success? artifact)  #t)
           (equal (parse-artifact-roundtrip artifact)  source))))
    (scenario "alternative node schemas and explicit namespaces share one DSL"
      (equal (assq 'Name (grammar-ir-ref alternative-fields-witness-grammar 'syntax-kinds))
              '(Name node (reserved left right)))
      (equal (grammar-ir-ref alternative-fields-witness-grammar 'flow)
              '((source lexical) (lexical parser) (parser cst)))
      (each (source) ('("alpha" "+"))
         (scope ((artifact (parse-source alternative-fields-witness-parser source)))
           (equal (parse-artifact-success? artifact)  #t)
           (equal (parse-artifact-roundtrip artifact)  source))))
    (scenario "infers the stable grammar and parser contracts"
      (sequential ((source "alpha")
             (artifact (parse-source concise-witness-parser source)))
        (equal (grammar-ir-ref concise-witness-grammar 'schema)
                "gerbil-parser.grammar-ir.v1")
        (equal (grammar-ir-ref concise-witness-grammar 'syntax-kinds)
                '((SourceFile node (name))
                    (Whitespace token (text))
                    (Identifier token (text))
                    (Unknown token (text))))
        (equal (parser-ir-ref concise-witness-parser-ir 'root-rule)
                'source-file)
        (equal (bound-grammar-ir-ref
                concise-witness-bound-grammar-ir 'schema)
                "gerbil-parser.bound-grammar-ir.v1")
        (equal (bound-grammar-ir-ref
                concise-witness-bound-grammar-ir 'expansionLineage)
                '(deflanguage named-node))
        (equal (bound-grammar-ir-ref
                concise-witness-bound-grammar-ir 'bindingCount)
                15)
        (equal (bound-grammar-ir-ref
                concise-witness-bound-grammar-ir 'referenceCount)
                12)
        (scope ((source-binding
               (bound-grammar-ir-binding
                concise-witness-bound-grammar-ir 'rule 'source-file))
              (field-binding
               (bound-grammar-ir-binding
                concise-witness-bound-grammar-ir 'field 'name 'SourceFile)))
          (equal (bound-grammar-ir-ref source-binding 'references)
                  '(((package . gerbil-parser)
                       (grammar . concise-witness-grammar)
                       (namespace . terminal)
                       (name . identifier))))
          (equal (cdr (assq 'path
                            (bound-grammar-ir-ref source-binding 'source)))
                  (bound-grammar-ir-ref
                     concise-witness-bound-grammar-ir 'originModule))
          (equal (bound-grammar-ir-ref field-binding 'references)
                  '(((package . gerbil-parser)
                       (grammar . concise-witness-grammar)
                       (namespace . syntax-kind)
                       (name . SourceFile)))))
        (equal (substring
                (bound-grammar-ir-ref
                 concise-witness-bound-grammar-ir 'grammarDigest)
                0 7)
                "sha256:")
        (equal (parser-ir-ref explicit-v1-witness-parser-ir 'schema)
                "gerbil-parser.parser-ir.v1")
        (equal (parse-artifact-success? artifact)  #t)
        (equal (parse-artifact-roundtrip artifact)  source)))
    (property "internal lexical expansion preserves ordered dispatch"
      (bindings)
      (equal lexical-end-witness  5)
      (equal lexical-choice-witness  5)
      (equal lexical-dispatch-witness  '(identifier . 5))
      (equal lexical-longest-witness  '(identifier . 5))
      (equal lexical-precedence-witness  '(high 1 10))
      (equal external-scanner-witness  2)
      (equal (literal-trie-witness "<=" 0)  2)
      (equal (literal-trie-witness "MATCHED suffix" 0)  7)
      (equal (literal-trie-witness "missing" 0)  #f))
    (property "mode dispatch keeps Unicode and fallback byte boundaries"
      (bindings (tokens (lex-source concise-witness-parser "α β?")))
      (equal (map token-kind tokens)
                '(identifier whitespace identifier unknown))
      (equal (map token-start tokens)  '(0 2 3 5))
      (equal (map token-end tokens)  '(2 3 5 6))))


;; gxtest discovers only exported names ending in -test.
(def language-surface-test concise-language-surface-tests)
(export language-surface-test)

;;; Generated source specializations may decline a shape and use ordinary LR.
(def backend-source-fallback (lambda (_machine _source) #f))
(begin
 (deflanguage backend-admission-witness
  (syntax
   (lexical
    (root name)
    (lex (identifier Identifier (identifier)))))
  (rules (name (node Name (field value identifier)))))
 (bind-fixture-grammar-release backend-admission-witness "backend-witness" "v1" "backend-witness.v1")
 (install-parser-machine-backends! backend-admission-witness-parser
 (list (list 'source (parser-machine-grammar-digest backend-admission-witness-parser) backend-source-fallback))))

(def (backend-rejects? thunk)
  (with-catch (lambda (_) #t) (lambda () (thunk) #f)))

(deflanguage-parser-tests language-backend-test "Engine-owned parser backend admission"
  (engine)
    (scenario "captured representations distinguish generated entries from prepared preference"
      (sequential ((machine backend-admission-witness-parser)
             (selected (parameterize ((current-lr-event-program-enabled? #t))
                         (parser-machine-for-current-semantic-backend machine))))
        (equal (eq? machine selected)  #f)
        (equal (parser-machine-backend-representation machine 'prepared)  'recognition)
        (equal (parser-machine-backend-representation selected 'prepared)  'event-program)
        (equal (parser-machine-backend-representation selected 'drive)  #f)
        (equal (parser-machine-backend-representation selected 'source)  'parse-artifact)
        (equal (backend-rejects?
                (lambda ()
                  (install-parser-machine-backends! selected
                    (list (list 'drive (parser-machine-grammar-digest selected) (lambda _ #f))
                          (list 'step (parser-machine-grammar-digest selected) (lambda _ #f))))))  #t)
        (equal (parser-machine-backend-representation selected 'drive)  #f)
        (equal (backend-rejects?
                (lambda () (install-parser-machine-direct-step!
                            selected (parser-machine-grammar-digest selected) (lambda _ #f))))  #t)
        (act (install-parser-machine-backends! selected
          (list (list 'drive (parser-machine-grammar-digest selected)
                      (lambda _ (error "contract control must not execute generated drive"))))))
        (equal (parser-machine-backend-representation selected 'drive)  'recognition)
        (equal (parser-machine-backend-representation machine 'drive)  #f)
        (parameters ((current-lr-event-program-enabled? #f))
          (equal (parser-machine-backend-representation selected 'prepared)  'event-program))
        (equal (backend-rejects? (lambda () (parser-machine-backend-representation #f 'prepared)))  #t)
        (equal (backend-rejects? (lambda () (parser-machine-backend-representation machine 'unknown)))  #t)))
    (scenario "declarative source backend retains the ordinary LR fallback"
      (equal (eq? (parser-machine-direct-source backend-admission-witness-parser)
                  backend-source-fallback)  #t)
      (scope ((artifact (parse-source backend-admission-witness-parser "example")))
        (equal (parse-artifact-success? artifact)  #t)
        (equal (parse-artifact-roundtrip artifact)  "example")))
    (property "a stale later row cannot partially install an earlier backend"
      (bindings)
      (equal
       (backend-rejects?
        (lambda ()
          (install-parser-machine-backends!
           explicit-v1-witness-parser
           (list (list 'drive (parser-machine-grammar-digest explicit-v1-witness-parser)
                       (lambda _ #f))
                 (list 'step "sha256:stale" (lambda _ #f))))))  #t)
      (equal (parser-machine-direct-drive explicit-v1-witness-parser)  #f))
    (scenario "unknown duplicate and occupied backend slots reject"
      (scope ((digest (parser-machine-grammar-digest explicit-v1-witness-parser))
            (procedure (lambda _ #f)))
        (each (rows) ((list (list (list 'unknown digest procedure))
               (list (list 'drive digest procedure) (list 'drive digest procedure))
               (list (list 'drive digest #f))))
           (equal (backend-rejects?
                   (lambda () (install-parser-machine-backends!
                               explicit-v1-witness-parser rows)))  #t)
           (equal (parser-machine-direct-drive explicit-v1-witness-parser)  #f))
        (equal
         (backend-rejects?
          (lambda ()
            (install-parser-machine-backends!
             backend-admission-witness-parser
             (list (list 'source
                         (parser-machine-grammar-digest backend-admission-witness-parser)
                         procedure)))))  #t))))

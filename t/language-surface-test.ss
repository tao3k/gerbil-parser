(import (only-in :gerbil-parser/src/compiler/language-expander compile-language)
        (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; -*- Gerbil -*-
;;; Boundary: executable witness for the concise v1 language authoring surface.
;;; Invariant: inferred and explicit declarations share canonical admission and
;;; publish the same Grammar IR, Parser IR, and ParseArtifact contracts.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/compiler/parser-ir parser-ir-ref)
        (only-in :gerbil-parser/src/compiler/bound-ir
                 bound-grammar-ir-binding
                 bound-grammar-ir-ref)
        (only-in :gerbil-parser/src/compiler/normalize grammar-ir-ref)
        (only-in :gerbil-parser/src/compiler/machine
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
 (deflanguage concise-v1-witness
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
 (bind-fixture-grammar-release concise-v1-witness "concise-v1-witness" "v1" "concise-v1-witness.v1") )

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

(def concise-v1-language-surface-tests
  (test-suite "concise v1 language surface"
    (test-case "transparent recursive references infer fields without crossing nodes"
      (check (assq 'Root (grammar-ir-ref transparent-fields-witness-grammar 'syntax-kinds))
             => '(Root node (item tail)))
      (check (assq 'Root (grammar-ir-ref alternative-fields-witness-grammar 'syntax-kinds))
             => '(Root node (value)))
      (for-each
       (lambda (source)
         (let (artifact (parse-source transparent-fields-witness-parser source))
           (check (parse-artifact-success? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)))
       '("alpha" "(beta)gamma" "((a)b)c")))
    (test-case "alternative node schemas and explicit namespaces share one DSL"
      (check (assq 'Name (grammar-ir-ref alternative-fields-witness-grammar 'syntax-kinds))
             => '(Name node (reserved left right)))
      (check (grammar-ir-ref alternative-fields-witness-grammar 'flow)
             => '((source lexical) (lexical parser) (parser cst)))
      (for-each
       (lambda (source)
         (let (artifact (parse-source alternative-fields-witness-parser source))
           (check (parse-artifact-success? artifact) => #t)
           (check (parse-artifact-roundtrip artifact) => source)))
       '("alpha" "+")))
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
      (check (literal-trie-witness "missing" 0) => #f))
    (test-case "mode dispatch keeps Unicode and fallback byte boundaries"
      (let (tokens (lex-source concise-v1-witness-parser "α β?"))
        (check (map token-kind tokens)
               => '(identifier whitespace identifier unknown))
        (check (map token-start tokens) => '(0 2 3 5))
        (check (map token-end tokens) => '(2 3 5 6))))))

(export concise-v1-language-surface-tests)

;; gxtest discovers only exported names ending in -test.
(def language-surface-test concise-v1-language-surface-tests)
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

(def language-backend-test
  (test-suite "Engine-owned parser backend admission"
    (test-case "captured representations distinguish generated entries from prepared preference"
      (let* ((machine backend-admission-witness-parser)
             (selected (parameterize ((current-lr-event-program-enabled? #t))
                         (parser-machine-for-current-semantic-backend machine))))
        (check (eq? machine selected) => #f)
        (check (parser-machine-backend-representation machine 'prepared) => 'recognition)
        (check (parser-machine-backend-representation selected 'prepared) => 'event-program)
        (check (parser-machine-backend-representation selected 'drive) => #f)
        (check (parser-machine-backend-representation selected 'source) => 'parse-artifact)
        (check (backend-rejects?
                (lambda ()
                  (install-parser-machine-backends! selected
                    (list (list 'drive (parser-machine-grammar-digest selected) (lambda _ #f))
                          (list 'step (parser-machine-grammar-digest selected) (lambda _ #f)))))) => #t)
        (check (parser-machine-backend-representation selected 'drive) => #f)
        (check (backend-rejects?
                (lambda () (install-parser-machine-direct-step!
                            selected (parser-machine-grammar-digest selected) (lambda _ #f)))) => #t)
        (install-parser-machine-backends! selected
          (list (list 'drive (parser-machine-grammar-digest selected)
                      (lambda _ (error "contract control must not execute generated drive")))))
        (check (parser-machine-backend-representation selected 'drive) => 'recognition)
        (check (parser-machine-backend-representation machine 'drive) => #f)
        (parameterize ((current-lr-event-program-enabled? #f))
          (check (parser-machine-backend-representation selected 'prepared) => 'event-program))
        (check (backend-rejects? (lambda () (parser-machine-backend-representation #f 'prepared))) => #t)
        (check (backend-rejects? (lambda () (parser-machine-backend-representation machine 'unknown))) => #t)))
    (test-case "declarative source backend retains the ordinary LR fallback"
      (check (eq? (parser-machine-direct-source backend-admission-witness-parser)
                  backend-source-fallback) => #t)
      (let (artifact (parse-source backend-admission-witness-parser "example"))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => "example")))
    (test-case "a stale later row cannot partially install an earlier backend"
      (check
       (backend-rejects?
        (lambda ()
          (install-parser-machine-backends!
           explicit-v1-witness-parser
           (list (list 'drive (parser-machine-grammar-digest explicit-v1-witness-parser)
                       (lambda _ #f))
                 (list 'step "sha256:stale" (lambda _ #f)))))) => #t)
      (check (parser-machine-direct-drive explicit-v1-witness-parser) => #f))
    (test-case "unknown duplicate and occupied backend slots reject"
      (let ((digest (parser-machine-grammar-digest explicit-v1-witness-parser))
            (procedure (lambda _ #f)))
        (for-each
         (lambda (rows)
           (check (backend-rejects?
                   (lambda () (install-parser-machine-backends!
                               explicit-v1-witness-parser rows))) => #t)
           (check (parser-machine-direct-drive explicit-v1-witness-parser) => #f))
         (list (list (list 'unknown digest procedure))
               (list (list 'drive digest procedure) (list 'drive digest procedure))
               (list (list 'drive digest #f))))
        (check
         (backend-rejects?
          (lambda ()
            (install-parser-machine-backends!
             backend-admission-witness-parser
             (list (list 'source
                         (parser-machine-grammar-digest backend-admission-witness-parser)
                         procedure))))) => #t)))))
(export language-backend-test)

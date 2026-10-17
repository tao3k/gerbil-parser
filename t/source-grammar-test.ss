(import :gerbil-parser/language-test-support)
;;; Public author-entry and rule admission contracts.
(import :std/test
        (only-in :clan/poo/object .o .ref object?)
        (only-in :gerbil-parser/language-support/entry deflanguage-parser-loader language-parser-entry-ref language-metadata-ref)
        (only-in :gerbil-parser/src/language/source declare-source-syntax source-language-digest)
        (only-in :gerbil-parser/src/runtime/source-engines LineSourceStrategy.)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-events parse-artifact-roundtrip)
        (only-in :gerbil-parser/language-support/grammar deflanguage defgrammar-syntax)
        (only-in :gerbil-parser/language-support/shell shell)
        (only-in :gerbil-parser/language-support/command-grammar lower-command-grammar)
        (for-syntax (only-in :gerbil/runtime/syntax SyntaxError-irritants) (only-in :gerbil/expander core-expand stx-source)))

(defsyntax (source-syntax-error stx)
 (syntax-case stx ()
  ((_ declaration)
   (let (message
    (with-catch
     (lambda (condition)
      (unless (syntax-error? condition) (raise condition)) (error-message condition))
     (lambda () (core-expand #'declaration) (error "invalid author declaration admitted"))))
    (datum->syntax #'source-syntax-error (list 'quote message))))))
(defsyntax (source-syntax-blame stx)
  (syntax-case stx ()
    ((_ declaration)
     (let (location (with-catch
       (lambda (condition)
         (unless (syntax-error? condition) (raise condition))
         (let (where (car (SyntaxError-irritants condition)))
           (unless (stx-source where) (error "source grammar blame lost its source location"))
           (syntax->datum where)))
       (lambda () (core-expand #'declaration) (error "invalid author declaration admitted"))))
       (datum->syntax #'source-syntax-blame (list 'quote location))))))
(defgrammar-syntax (nullable-body text)
  (repeat (optional (field keyword text))))
(def (rejects? thunk) (with-catch (lambda (_) #t) (lambda () (thunk) #f)))
(def metadata-syntax
  (declare-source-syntax (.o (:: self LineSourceStrategy.) root-kind: 'Notes required-prefix: "#")))
(def input-version (string-copy "v1"))
(def input-metadata (list (cons 'language "notes") (cons 'version input-version)
                         (cons 'contract "notes.metadata.v1") (cons 'edition 'local)))
(deflanguage-parser-loader value-entry
  (source metadata-syntax) (parse parse-value-entry)
  (slots metadata: input-metadata))
(def value-descriptor (language-parser-entry-ref value-entry 'descriptor))
(def MetadataBase. (.o language: "notes" version: "wrong" contract: "notes.metadata.v1" edition: 'base))
(deflanguage-parser-loader object-entry
  (source metadata-syntax) (parse parse-object-entry)
  (slots metadata: (.o (:: self MetadataBase.) version: "v1" edition: 'local)))
(def object-descriptor (language-parser-entry-ref object-entry 'descriptor))
(deflanguage-parser-tests source-grammar-test "deflanguage source syntax and rule contract"
  (engine)
(property "source errors identify the direct expression or author macro invocation"
      (bindings)
      (equal (source-syntax-blame
    (deflanguage invalid-shell (syntax (shell (operators "(") (regions) (bindings) (words)
      (commands (roles) (descriptor (literal "x")))))
      (rules (bad (node WhileCommand (repeat (optional (field keyword "while"))))))))
     '(repeat (optional (field keyword "while"))))
      (equal (source-syntax-blame
    (deflanguage invalid-shell (syntax (shell (operators "(") (regions) (bindings) (words)
      (commands (roles) (descriptor (literal "x")))))
      (rules (bad (node WhileCommand (nullable-body "while"))))))
     '(nullable-body "while")))
  (scenario "selectors depend on syntax rather than rule and field names"
    (scope ((left (lower-command-grammar '((function-header (node FunctionDefinition (field name word) (field open (operator "(")) (field close (operator ")")) (field body command))))))
          (right (lower-command-grammar '((renamed (node FunctionDefinition (field name word) (field open (operator "(")) (field close (operator ")")) (field body command)))))))
      (equal (cdar left)  (cdar right)))
    (scope ((left (lower-command-grammar '((a (node WhileCommand (field keyword "while"))))))
          (right (lower-command-grammar '((b (node WhileCommand (field renamed-field "while")))))))
      (equal (cadar left)  (cadar right))
      (equal (caddar left)  (caddar right))))
  (scenario "normal metadata values and native POO overrides bind equivalent source identity"
   (scope ((value (language-parser-entry-ref value-entry 'metadata))
         (object (language-parser-entry-ref object-entry 'metadata)))
    (equal (list? value)  #t) (equal (object? object)  #t)
    (each (key) ('(language version contract digest digest-kind edition))
      (equal (language-metadata-ref value key)  (language-metadata-ref object key)))
    (equal (source-language-digest value-descriptor)  (source-language-digest object-descriptor))
    (equal (parse-artifact-events (parse-value-entry "# alpha\n"))
            (parse-artifact-events (parse-object-entry "# alpha\n")))
    (equal (parse-artifact-roundtrip (parse-value-entry "# alpha\n"))  "# alpha\n")
    (act (string-set! input-version 0 #\x))
    (equal (language-parser-entry-ref value-entry 'version)  "v1")))
  (scenario "metadata rejects missing identity, duplicate keys and invalid value shapes"
   (each (value) ('(((language . "notes") (version . "v1"))
       ((language . "notes") (version . "v1") (version . "v2") (contract . "notes.v1"))
       ((language . "notes") (version . "") (contract . "notes.v1"))
       ((language . "notes") (version . "v1") (contract . #f))
       ((1 . "notes")) #f "metadata"))
     (equal (rejects? (lambda ()
       (let () (deflanguage-parser-loader invalid-entry
                 (source metadata-syntax) (parse invalid-parse) (slots metadata: value))
         invalid-entry)))  #t)))
  (property "public entry rejects duplicate blocks and execution-recipe syntax"
      (bindings)
      (equal (string? (source-syntax-error
    (deflanguage bad (syntax (shell)) (syntax (shell)) (rules))))  #t)
      (equal (string? (source-syntax-error
    (deflanguage bad (syntax (shell (strategy arbitrary))) (rules))))  #t))
  (scenario "retired author declarations fail before parser publication"
   (equal (source-syntax-error
     (deflanguage retired (root source-file)
       (lex (identifier Identifier (identifier)))
       (rules (source-file (alias SourceFile (token identifier))))))
      "deflanguage requires exactly one syntax vocabulary and one rules block; flat declarations and release identity are not author syntax")
   (equal (string? (source-syntax-error
     (deflanguage retired (identity "retired" "1" "retired.v1")
       (root source-file) (lex (identifier Identifier (identifier)))
       (rules (source-file (node SourceFile identifier))))))  #t)
   (equal (string? (source-syntax-error
     (deflanguage retired
       (syntax (lexical (root name) (lex (identifier Identifier (identifier)))))
       (rules (name (alias Name identifier))))))  #t)
   (equal (string? (source-syntax-error
     (deflanguage retired
       (syntax (lexical (root name) (lex (identifier Identifier (identifier)))
                        (flow (source lexical))))
       (rules (name (node Name identifier))))))  #t)
   (equal (string? (source-syntax-error
     (deflanguage retired
       (syntax (lexical (root name) (lex (identifier Identifier (identifier)))
                        (backends (source digest procedure))))
       (rules (name (node Name identifier))))))  #t))
  (scenario "command admission rejects duplicates, dangling references and non-consuming repetition"
   (each (rules) ('(((same (node WhileCommand (field keyword "while"))) (same (node UntilCommand (field keyword "until"))))
      ((missing (node WhileCommand (field body (reference absent)))))
      ((nullable (node WhileCommand (repeat (optional (field keyword "while"))))))
      ((recipe (node WhileCommand (take keyword (word "while")))))
      ((unterminated (node WhileCommand (field condition command-list))))))
    (equal (rejects? (lambda () (lower-command-grammar rules)))  #t)))
)

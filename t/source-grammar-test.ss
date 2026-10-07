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
(export source-grammar-test)
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
  (source value-descriptor metadata-syntax) (parse parse-value-entry)
  (metadata input-metadata))
(def MetadataBase. (.o language: "notes" version: "wrong" contract: "notes.metadata.v1" edition: 'base))
(deflanguage-parser-loader object-entry
  (source object-descriptor metadata-syntax) (parse parse-object-entry)
  (metadata (.o (:: self MetadataBase.) version: "v1" edition: 'local)))
(def source-grammar-test
 (test-suite "deflanguage source syntax and rule contract"
(test-case "source errors identify the direct expression or author macro invocation"
  (check (source-syntax-blame
    (deflanguage invalid-shell (syntax (shell (operators "(") (regions) (bindings) (words)
      (commands (roles) (descriptor (literal "x")))))
      (rules (bad (node WhileCommand (repeat (optional (field keyword "while"))))))))
    => '(repeat (optional (field keyword "while"))))
  (check (source-syntax-blame
    (deflanguage invalid-shell (syntax (shell (operators "(") (regions) (bindings) (words)
      (commands (roles) (descriptor (literal "x")))))
      (rules (bad (node WhileCommand (nullable-body "while"))))))
    => '(nullable-body "while")))
  (test-case "selectors depend on syntax rather than rule and field names"
    (let ((left (lower-command-grammar '((function-header (node FunctionDefinition (field name word) (field open (operator "(")) (field close (operator ")")) (field body command))))))
          (right (lower-command-grammar '((renamed (node FunctionDefinition (field name word) (field open (operator "(")) (field close (operator ")")) (field body command)))))))
      (check (cdar left) => (cdar right)))
    (let ((left (lower-command-grammar '((a (node WhileCommand (field keyword "while"))))))
          (right (lower-command-grammar '((b (node WhileCommand (field renamed-field "while")))))))
      (check (cadar left) => (cadar right))
      (check (caddar left) => (caddar right))))
  (test-case "normal metadata values and native POO overrides bind equivalent source identity"
   (let ((value (language-parser-entry-ref value-entry 'metadata))
         (object (language-parser-entry-ref object-entry 'metadata)))
    (check (list? value) => #t) (check (object? object) => #t)
    (for-each (lambda (key)
      (check (language-metadata-ref value key) => (language-metadata-ref object key)))
      '(language version contract digest digest-kind edition))
    (check (source-language-digest value-descriptor) => (source-language-digest object-descriptor))
    (check (parse-artifact-events (parse-value-entry "# alpha\n"))
           => (parse-artifact-events (parse-object-entry "# alpha\n")))
    (check (parse-artifact-roundtrip (parse-value-entry "# alpha\n")) => "# alpha\n")
    (string-set! input-version 0 #\x)
    (check (language-parser-entry-ref value-entry 'version) => "v1")))
  (test-case "metadata rejects missing identity, duplicate keys and invalid value shapes"
   (for-each (lambda (value)
     (check (rejects? (lambda ()
       (let () (deflanguage-parser-loader invalid-entry
                 (source invalid-descriptor metadata-syntax) (parse invalid-parse) (metadata value))
         invalid-entry))) => #t))
     '(((language . "notes") (version . "v1"))
       ((language . "notes") (version . "v1") (version . "v2") (contract . "notes.v1"))
       ((language . "notes") (version . "") (contract . "notes.v1"))
       ((language . "notes") (version . "v1") (contract . #f))
       ((1 . "notes")) #f "metadata")))
  (test-case "public entry rejects duplicate blocks and execution-recipe syntax"
   (check (string? (source-syntax-error
    (deflanguage bad (syntax (shell)) (syntax (shell)) (rules)))) => #t)
   (check (string? (source-syntax-error
    (deflanguage bad (syntax (shell (strategy arbitrary))) (rules)))) => #t))
  (test-case "retired author declarations fail before parser publication"
   (check (source-syntax-error
     (deflanguage retired (root source-file)
       (lex (identifier Identifier (identifier)))
       (rules (source-file (alias SourceFile (token identifier))))))
     => "deflanguage requires exactly one syntax vocabulary and one rules block; flat declarations and release identity are not author syntax")
   (check (string? (source-syntax-error
     (deflanguage retired (identity "retired" "1" "retired.v1")
       (root source-file) (lex (identifier Identifier (identifier)))
       (rules (source-file (node SourceFile identifier)))))) => #t)
   (check (string? (source-syntax-error
     (deflanguage retired
       (syntax (lexical (root name) (lex (identifier Identifier (identifier)))))
       (rules (name (alias Name identifier)))))) => #t)
   (check (string? (source-syntax-error
     (deflanguage retired
       (syntax (lexical (root name) (lex (identifier Identifier (identifier)))
                        (flow (source lexical))))
       (rules (name (node Name identifier)))))) => #t)
   (check (string? (source-syntax-error
     (deflanguage retired
       (syntax (lexical (root name) (lex (identifier Identifier (identifier)))
                        (backends (source digest procedure))))
       (rules (name (node Name identifier)))))) => #t))
  (test-case "command admission rejects duplicates, dangling references and non-consuming repetition"
   (for-each (lambda (rules)
    (check (rejects? (lambda () (lower-command-grammar rules))) => #t))
    '(((same (node WhileCommand (field keyword "while"))) (same (node UntilCommand (field keyword "until"))))
      ((missing (node WhileCommand (field body (reference absent)))))
      ((nullable (node WhileCommand (repeat (optional (field keyword "while"))))))
      ((recipe (node WhileCommand (take keyword (word "while")))))
      ((unterminated (node WhileCommand (field condition command-list)))))))
))

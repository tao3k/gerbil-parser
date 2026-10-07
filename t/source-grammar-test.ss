;;; Public author-entry and rule admission contracts.
(import :std/test
        (only-in :gerbil-parser/language-support deflanguage)
        (only-in :gerbil-parser/language-support/shell shell)
        (only-in :gerbil-parser/language-support/command-grammar lower-command-grammar)
        (for-syntax (only-in :gerbil/expander core-expand)))
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
(def (rejects? thunk) (with-catch (lambda (_) #t) (lambda () (thunk) #f)))
(def source-grammar-test
 (test-suite "deflanguage source syntax and rule contract"
  (test-case "public entry rejects duplicate blocks and execution-recipe syntax"
   (check (string? (source-syntax-error
    (deflanguage bad (identity "bad" "1" "bad.v1") (syntax (shell)) (syntax (shell)) (rules)))) => #t)
   (check (string? (source-syntax-error
    (deflanguage bad (identity "bad" "1" "bad.v1") (syntax (shell (strategy arbitrary))) (rules)))) => #t))
  (test-case "command admission rejects duplicates, dangling references and non-consuming repetition"
   (for-each (lambda (rules)
    (check (rejects? (lambda () (lower-command-grammar rules))) => #t))
    '(((same (node WhileCommand (field keyword "while"))) (same (node UntilCommand (field keyword "until"))))
      ((missing (node WhileCommand (field body (reference absent)))))
      ((nullable (node WhileCommand (repeat (optional (field keyword "while"))))))
      ((recipe (node WhileCommand (take keyword (word "while")))))
      ((unterminated (node WhileCommand (field condition command-list)))))))
))

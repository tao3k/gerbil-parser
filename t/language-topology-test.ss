;;; Repository authoring contract; no implementation or generated products in packs.
(import :std/test)
(export language-topology-test)
(def (scheme-files root)
 (apply append
  (map (lambda (name)
         (let (path (path-expand name root))
           (cond ((eq? (file-info-type (file-info path)) 'directory) (scheme-files path))
                 ((string-suffix? ".ss" name) (list path))
                 (else '())))) (directory-files root))))
(def (forms path)
 (call-with-input-file path (lambda (port)
   (let loop ((result '()))
     (let (form (read port))
       (if (eof-object? form) (reverse result) (loop (cons form result))))))))
(def (declaration? path head)
 (find (lambda (form) (and (pair? form) (eq? (car form) head))) (forms path)))
(def (authoring-form? form interface)
 (and (list? form) (pair? form)
      (or (memq (car form) '(import export))
          (and (eq? (car form) 'def) (not (eq? interface 'tests))
               (= (length form) 3) (symbol? (cadr form))
               (not (and (pair? (caddr form)) (eq? (caaddr form) 'lambda))))
          (memq (car form)
           (case interface
            ((grammar) '(deflanguage deflanguage-source deflanguage-projection
                         deflanguage-module-scanner deftext-profile defregion-plan
                         deflanguage-antlr4-grammar deflanguage-iso-bnf-grammar
                         defsyntax-antlr4-source defsyntax-javacc-source defsyntax-iso-bnf-source
                         defsyntax-fixture defsyntax-corpus))
            ((parser) '(deflanguage-parser-loader deflanguage-source-receipt deflanguage-model-entry defsyntax-corpus))
            ((tests) '(deflanguage-parser-tests)))))))
(def language-topology-test
 (test-suite "three-interface language authoring contract"
  (test-case "reject procedure definitions and unrestricted parser tests"
   (check (and (authoring-form? '(def (scan source) source) 'grammar) #t) => #f)
   (check (and (authoring-form? '(def scan (lambda (source) source)) 'grammar) #t) => #f)
   (check (and (authoring-form? '(def test (test-suite "manual")) 'tests) #t) => #f))
  (test-case "every pack contains exactly grammar, parser and declarative parser tests"
   (for-each
    (lambda (language)
      (let* ((root (path-expand language "languages"))
             (files (scheme-files root)))
       (check (length files) => 3)
       (for-each (lambda (row)
         (check (andmap (lambda (form) (and (authoring-form? form (cdr row)) #t))
                        (forms (path-expand (car row) root))) => #t))
        '(("grammar.ss" . grammar) ("parser.ss" . parser) ("parser-test.ss" . tests)))
       (check (andmap (lambda (name) (and (member (path-expand name root) files) #t))
                      '("grammar.ss" "parser.ss" "parser-test.ss")) => #t)
       (check (and (declaration? (path-expand "parser.ss" root) 'deflanguage-parser-loader) #t) => #t)
       (check (and (declaration? (path-expand "parser-test.ss" root) 'deflanguage-parser-tests) #t) => #t)))
    (filter (lambda (name) (eq? (file-info-type (file-info (path-expand name "languages"))) 'directory))
            (directory-files "languages"))))))

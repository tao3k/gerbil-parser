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
(def (development-import? form)
 (cond ((pair? form) (or (development-import? (car form)) (development-import? (cdr form))))
       ((symbol? form)
        (and (memq form (quote (:gerbil-parser/language-support/development
                               :gerbil-parser/language-support/fixture
                               :gerbil-parser/language-test-support
                               :gerbil-parser/language-build-support))) #t))
       (else #f)))
(def (authoring-form? form interface)
 (and (list? form) (pair? form)
      (or (eq? (car form) 'export)
          (and (eq? (car form) 'import)
               (or (eq? interface 'tests) (not (development-import? form))))
          (and (eq? (car form) 'def) (eq? interface 'parser)
               (= (length form) 3) (symbol? (cadr form))
               (not (and (pair? (caddr form)) (eq? (caaddr form) 'lambda))))
          (and
           (or (not (eq? interface 'grammar))
               (not (eq? (car form) 'deflanguage))
               (and (= (length form) 4)
                    (symbol? (cadr form))
                    (list? (caddr form)) (= (length (caddr form)) 2)
                    (eq? (caaddr form) 'syntax)
                    (pair? (cadr (caddr form)))
                    (list? (cadddr form)) (pair? (cadddr form))
                    (eq? (car (cadddr form)) 'rules)))
           (memq (car form)
            (case interface
             ((grammar) '(deflanguage defgrammar-syntax deflanguage-projection deftext-profile
                          defsyntax-antlr4-source defsyntax-javacc-source defsyntax-iso-bnf-source))
             ((parser) '(deflanguage-parser-loader deflanguage-parser-receipt deflanguage-model-entry
                         defsyntax-javacc-source))
             ((tests) '(defsyntax-fixture defsyntax-corpus deflanguage-development-loader deflanguage-parser-tests))))))))
(def language-topology-test
 (test-suite "three-interface language authoring contract"
  (test-case "reject procedure definitions and unrestricted parser tests"
   (check (and (authoring-form? '(def (scan source) source) 'grammar) #t) => #f)
   (check (and (authoring-form? '(def scan (lambda (source) source)) 'grammar) #t) => #f)
   (check (and (authoring-form? '(def test (test-suite "manual")) 'tests) #t) => #f)
   (check (authoring-form? (quote (defsyntax-corpus corpus)) (quote grammar)) => #f)
   (check (authoring-form? (quote (deflanguage-development-loader entry)) (quote parser)) => #f)
   (check (authoring-form? (quote (import :gerbil-parser/language-support/development)) (quote parser)) => #f))
  (test-case "release identity belongs to the parser interface"
   (check (and (authoring-form? '(def +version+ "1") 'grammar) #t) => #f)
   (check (and (authoring-form? '(def +version+ "1") 'parser) #t) => #t)
   (check (and (authoring-form? '(deflanguage example (identity "x" "1" "x.v1") (root source)) 'grammar) #t) => #f)
   (check (and (authoring-form? '(deflanguage example
  (syntax
   (lexical
    (root source)
    (lex)))
  (rules)) 'grammar) #t) => #t))
  (test-case "author grammar excludes backend profile assembly"
   (for-each (lambda (head)
     (check (and (authoring-form? (list head 'backend) 'grammar) #t) => #f))
     '(deflanguage-source defregion-plan defscanner-profile defresult-profile defpart-profile defcommand-profile defbinding-profile))
   (check (and (authoring-form? '(defgrammar-syntax (loop kind) (node kind)) 'grammar) #t) => #t))
  (test-case "every pack exposes three interfaces with declarative private syntax modules"
   (for-each
    (lambda (language)
      (let* ((root (path-expand language "languages"))
             (files (scheme-files root))
             (interfaces (map (lambda (name) (path-expand name root))
                          '("grammar.ss" "parser.ss" "parser-test.ss")))
             (private (filter (lambda (file) (not (member file interfaces))) files)))
       (check (andmap (lambda (file) (string-prefix? (path-expand "syntax/" root) file)) private) => #t)
       (for-each (lambda (file)
         (check (andmap (lambda (form)
           (and (memq (car form) '(import export deflanguage defgrammar-syntax deftext-profile))
                (authoring-form? form 'grammar) #t)) (forms file)) => #t)) private)
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

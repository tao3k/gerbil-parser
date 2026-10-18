(import :gerbil-parser/language-test-support)
(import (only-in :gerbil-parser/t/fixtures/fixture-release bind-fixture-grammar-release))
;;; -*- Gerbil -*-
;;; Existing concise admission errors retain author occurrence and context.
(import :std/test
        (only-in ../src/language/grammar deflanguage)
        (only-in ../src/compiler/language-expander compile-language)
        (only-in ../src/grammar/algebra grammar-expression? grammar-expression-header?)
        (only-in ./fixtures/language-diagnostics-vocabulary required-items broken-wrapper)
        (rename-in (only-in ./fixtures/language-diagnostics-vocabulary required-items)
                   (required-items renamed-items))
        (for-syntax (only-in :gerbil/expander core-expand)))


(defsyntax (declaration-error stx)
  (def (capture form)
    (with-catch
     (lambda (condition)
       (unless (syntax-error? condition) (raise condition))
       (let* ((details (slot-ref condition 'irritants))
              (blame (car details)) (loc (stx-source blame))
              (start (and loc (##position->filepos (##locat-start-position loc)))))
         (list (error-message condition) (syntax->datum blame)
               (and start (list (##container->path (##locat-container loc))
                                (+ 1 (##filepos-line start))
                                (##filepos-col start)))
               (map syntax->datum (cdr details)))))
     (lambda () (core-expand form) (error "invalid declaration was admitted"))))
  (syntax-case stx (source-free)
    ((_ source-free form)
     (datum->syntax #'declaration-error
       (list 'quote (capture (datum->syntax #'declaration-error (syntax->datum #'form) #f #f)))))
    ((_ form)
     (datum->syntax #'declaration-error (list 'quote (capture #'form))))))

(defrules invalid-language ()
  ((_ expression)
   (begin
 (deflanguage diagnostic-study
  (syntax
   (lexical
    (root source-file)
    (lex (identifier Identifier (identifier)))
    (extras)
    (keywords)
    (recoveries)
    (conflicts reject)
    (case-insensitive #f)))
  (rules (source-file (node SourceFile expression))))
 (bind-fixture-grammar-release diagnostic-study "diagnostic-study" "v1" "diagnostic-study.v1") )))

;; Pass the reader-authored expression through the declaration template without
;; rebuilding its datum, so its original occurrence remains available.
(defsyntax (invalid-expression-error stx)
  (syntax-case stx (source-free)
    ((_ source-free expression)
     #'(declaration-error source-free (invalid-language expression)))
    ((_ expression)
     #'(declaration-error (invalid-language expression)))))

(def direct-reference
  (invalid-expression-error (reference missing-rule)))
(def direct-token
  (invalid-expression-error (token missing-token)))
(def direct-constructor
  (invalid-expression-error (misspelled identifier)))
(def direct-identifier
  (invalid-expression-error missing-name))
(def macro-reference
  (invalid-expression-error
    (required-items argument (reference missing-rule) ",")))
(def renamed-reference
  (invalid-expression-error
    (renamed-items argument (reference missing-rule) ",")))
(def macro-constructor
  (invalid-expression-error (broken-wrapper identifier)))
(def repeated-reference
  (invalid-expression-error (seq (reference repeated-missing) (reference repeated-missing))))
(def source-free-reference
  (invalid-expression-error source-free (reference missing-rule)))

(def forward-field
  (declaration-error
    (begin
 (deflanguage diagnostic-study
  (syntax
   (lexical
    (root source-file)
    (lex (identifier Identifier (identifier)))
    (extras)
    (keywords)
    (recoveries)
    (conflicts reject)
    (case-insensitive #f)))
  (rules (source-file (node SourceFile (reference helper)))
             (helper (field argument))))
 (bind-fixture-grammar-release diagnostic-study "diagnostic-study" "v1" "diagnostic-study.v1") )))

(defrules invalid-helper-error ()
  ((_ helper-expression)
   (declaration-error
     (begin
 (deflanguage diagnostic-study
  (syntax
   (lexical
    (root source-file)
    (lex (identifier Identifier (identifier)))
    (extras)
    (keywords)
    (recoveries)
    (conflicts reject)
    (case-insensitive #f)))
  (rules (source-file (node SourceFile (reference helper)))
              (helper helper-expression)))
 (bind-fixture-grammar-release diagnostic-study "diagnostic-study" "v1" "diagnostic-study.v1") ))))
(def forward-unary (invalid-helper-error (optional)))
(def forward-reference (invalid-helper-error (reference)))

(defrules invalid-root-error ()
  ((_ expression)
   (declaration-error
     (begin
 (deflanguage diagnostic-study
  (syntax
   (lexical
    (root source-file)
    (lex (identifier Identifier (identifier)))
    (extras)
    (keywords)
    (recoveries)
    (conflicts reject)
    (case-insensitive #f)))
  (rules (source-file expression)))
 (bind-fixture-grammar-release diagnostic-study "diagnostic-study" "v1" "diagnostic-study.v1") ))))
(def root-without-node (invalid-root-error identifier))
(def macro-root-without-node
  (invalid-root-error (required-items argument identifier ",")))
(def node-token-collision
  (invalid-expression-error (node Identifier identifier)))
(def multi-body-root
  (declaration-error
    (begin
 (deflanguage diagnostic-study
  (syntax
   (lexical
    (root source-file)
    (lex (identifier Identifier (identifier)))
    (extras)
    (keywords)
    (recoveries)
    (conflicts reject)
    (case-insensitive #f)))
  (rules (source-file identifier identifier)))
 (bind-fixture-grammar-release diagnostic-study "diagnostic-study" "v1" "diagnostic-study.v1") )))
(def source-free-collision
  (invalid-expression-error source-free (node Identifier identifier)))
(defrules token-node ()
  ((_ item) (node Identifier item)))
(def macro-node-token-collision
  (invalid-expression-error (token-node identifier)))

;; One admission matrix covers the author projection and canonical compiler entry.
(defrules invalid-core-error ()
  ((_ expression)
   (declaration-error
     (compile-language diagnostic-study
       (identity "diagnostic-study" "v1" "diagnostic-study.v1")
       (syntax-kinds (SourceFile node ()) (Identifier token (text)))
       (terminals (identifier Identifier))
       (lexical-rules (identifier (identifier)))
       (rules (source-file (alias SourceFile expression)))
       (extras) (keywords) (parser-entrypoints (source-file parse pure))
       (recoveries) (flow (source lexical) (lexical cst))))))

(def scalar-errors
  (list (invalid-expression-error "")
        (invalid-expression-error (literal ""))
        (invalid-expression-error (layout-start ""))
        (invalid-expression-error (layout-next ""))
        (invalid-expression-error (prec sideways 10 identifier))
        (invalid-expression-error (prec left "high" identifier))))
(def scalar-blames
  '("" (literal "") (layout-start "") (layout-next "")
    (prec sideways 10 identifier) (prec left "high" identifier)))
(def core-errors
  (list (invalid-core-error (optional))
        (invalid-core-error (field argument))
        (invalid-core-error (sequence))
        (invalid-core-error (token identifier extra))
        (invalid-core-error (literal ""))
        (invalid-core-error (layout-end ""))
        (invalid-core-error (precedence sideways 10 (token identifier)))
        (invalid-core-error (precedence left "high" (token identifier)))
        (invalid-core-error (optional . identifier))))
(def core-blames
  '((optional) (field argument) (sequence) (token identifier extra)
    (literal "") (layout-end "") (precedence sideways 10 (token identifier))
    (precedence left "high" (token identifier)) (optional . identifier)))
(def dotted-surface
  (invalid-expression-error (optional . identifier)))

;; Default normalization must preserve the declaration the author actually wrote.
(def defaulted-root
  (declaration-error
    (begin
 (deflanguage diagnostic-study
  (syntax
   (lexical
    (root source-file)
    (lex (identifier Identifier (identifier)))))
  (rules (source-file identifier)))
 (bind-fixture-grammar-release diagnostic-study "diagnostic-study" "v1" "diagnostic-study.v1") )))
(def source-free-defaulted-root
  (declaration-error source-free
    (begin
 (deflanguage diagnostic-study
  (syntax
   (lexical
    (root source-file)
    (lex (identifier Identifier (identifier)))))
  (rules (source-file identifier)))
 (bind-fixture-grammar-release diagnostic-study "diagnostic-study" "v1" "diagnostic-study.v1") )))

;; Section admission must reject malformed policies before elaboration, while
;; retaining the exact reader-authored section as the offending syntax.
(defsyntax (invalid-section-error stx)
  (syntax-case stx ()
    ((_ clause)
     #'(declaration-error
         (deflanguage diagnostic-study
           (syntax (lexical
                     (root source-file)
                     (lex (identifier Identifier (identifier)))
                     clause))
           (rules (source-file (node SourceFile identifier))))))))

(def empty-conflicts (invalid-section-error (conflicts)))
(def extra-conflicts (invalid-section-error (conflicts reject reject)))
(def empty-case-policy (invalid-section-error (case-insensitive)))
(def extra-case-policy (invalid-section-error (case-insensitive #f #t)))

(def (source-line location)
  (call-with-input-file (car location)
    (lambda (port)
      (let loop ((line 1))
        (let (text (read-line port))
          (when (eof-object? text) (error "diagnostic line missing" location))
          (if (= line (cadr location)) text (loop (+ line 1))))))))
(defparser-check (check-error result message blame text)
  (equal (car result)  message)
  (equal (cadr result)  blame)
  (equal (pair? (cadddr result))  #t)
  (equal (cadr (car (cadddr result)))  'diagnostic-study)
  (sequential ((location (caddr result)) (line (source-line location))
         (offset (caddr location)))
    (equal (substring line offset (+ offset (string-length text)))  text)))

(deflanguage-parser-tests language-diagnostics-test "concise declaration author diagnostics"
  (engine)
    (scenario "section arity errors retain the authored policy and declaration"
      (verify check-error empty-conflicts "invalid concise language declaration" '(conflicts) "(conflicts)")
      (verify check-error extra-conflicts "invalid concise language declaration" '(conflicts reject reject) "(conflicts reject reject)")
      (verify check-error empty-case-policy "invalid concise language declaration" '(case-insensitive) "(case-insensitive)")
      (verify check-error extra-case-policy "invalid concise language declaration" '(case-insensitive #f #t) "(case-insensitive #f #t)"))
    (scenario "direct unknown targets and constructor retain reader parameters"
      (verify check-error direct-reference "explicit reference names an unknown rule" 'missing-rule "missing-rule")
      (verify check-error direct-token "explicit token names an unknown lexical declaration" 'missing-token "missing-token")
      (verify check-error direct-constructor "unknown concise GrammarExpr constructor" 'misspelled "misspelled")
      (verify check-error direct-identifier "unresolved concise grammar identifier" 'missing-name "missing-name"))
    (scenario "macro and renamed import report the author invocation"
      (verify check-error macro-reference "explicit reference names an unknown rule" '(required-items argument (reference missing-rule) ",") "(required-items")
      (verify check-error renamed-reference "explicit reference names an unknown rule" '(renamed-items argument (reference missing-rule) ",") "(renamed-items"))
    (scenario "invalid generated constructors blame the author call"
      (verify check-error macro-constructor "unknown concise GrammarExpr constructor" (quote (broken-wrapper identifier)) "(broken-wrapper"))
    (scenario "equal invalid expressions select the first occurrence"
      (verify check-error repeated-reference "explicit reference names an unknown rule" 'repeated-missing "repeated-missing")
      (sequential ((location (caddr repeated-reference)) (line (source-line location))
             (offset (caddr location)))
        (equal (substring line (- offset 11) offset)  "(reference ")
        (equal (substring line 0 offset)
                "  (invalid-expression-error (seq (reference ")))
    (scenario "forward helper shape is checked before field inference"
      (verify check-error forward-field "invalid concise field expression" (quote (field argument)) "(field argument"))
    (scenario "malformed forward helpers remain typed syntax errors"
      (verify check-error forward-unary "invalid concise unary grammar expression" (quote (optional)) "(optional")
      (verify check-error forward-reference "explicit reference names an unknown rule" (quote (reference)) "(reference"))
    (scenario "root obligation identifies the authored body"
      (verify check-error root-without-node "concise language root rule must construct one node" 'identifier "identifier")
      (verify check-error macro-root-without-node "concise language root rule must construct one node" '(required-items argument identifier ",") "(required-items")
      (verify check-error multi-body-root "concise language root rule must construct one node" '(source-file identifier identifier) "(source-file identifier identifier)"))
    (scenario "node and token kind collision retains author context"
      (verify check-error node-token-collision "concise syntax kind is both a node and a token" 'Identifier "Identifier")
      (verify check-error macro-node-token-collision "concise syntax kind is both a node and a token" '(token-node identifier) "(token-node"))
    (scenario "canonical scalar constraints fail at concise admission"
      (each (result blame) (scalar-errors scalar-blames)
         (verify check-error result "invalid canonical GrammarExpr metadata" blame (if (string? blame) "\"\"" "(")))
      (verify check-error dotted-surface "invalid concise grammar expression" '(optional . identifier) "(optional"))
    (scenario "canonical entry rejects malformed constructors before list access"
      (each (result blame) (core-errors core-blames)
         (verify check-error result "invalid GrammarExpr declaration" blame "(")))
    (scenario "canonical header checks and recursive membership stay distinct"
      (each (row) ('(((empty) #t) ((literal "a") #t) ((literal "") #f)
         ((layout-start " ") #t) ((layout-next "") #f)
         ((layout-end) #t) ((layout-end ")") #t) ((layout-end "") #f)
         ((token name) #t) ((reference rule) #t) ((sequence (empty)) #t)
         ((choice) #f) ((optional (empty)) #t) ((repeat (empty)) #t)
         ((repeat1 (literal "a")) #t) ((field value (token name)) #t)
         ((alias Node (reference rule)) #t)
         ((precedence left 10 (token name)) #t)
         ((precedence sideways 10 (token name)) #f)
         ((precedence right "high" (token name)) #f)
         ((optional . name) #f) ((optional invalid-child) #f))) (equal (and (grammar-expression? (car row)) #t)  (cadr row)))
      (equal (and (grammar-expression-header? '(optional invalid-child)) #t)  #t)
      ;; Nullability/liveness policy is owned by construction/resolved LR analysis,
      ;; not by canonical structural membership or the new header predicate.
      (equal (grammar-expression? '(repeat (empty)))  #t))
    (scenario "defaulted declarations retain the original author context"
      (verify check-error defaulted-root "concise language root rule must construct one node" 'identifier "identifier")
      (equal (car (cadddr defaulted-root))
              '(deflanguage diagnostic-study
  (syntax
   (lexical
    (root source-file)
    (lex (identifier Identifier (identifier)))))
  (rules (source-file identifier))))
      (equal (car source-free-defaulted-root)
              "concise language root rule must construct one node")
      (equal (caddr source-free-defaulted-root)  #f)
      (equal (cadddr source-free-defaulted-root)  (cadddr defaulted-root)))
    (property "source-free input does not invent author coordinates"
      (bindings)
      (equal (car source-free-reference)  "explicit reference names an unknown rule")
      (equal (cadr source-free-reference)  'missing-rule)
      (equal (caddr source-free-reference)  #f)
      (equal (car source-free-collision)  "concise syntax kind is both a node and a token")
      (equal (cadr source-free-collision)  'Identifier)
      (equal (caddr source-free-collision)  #f)))

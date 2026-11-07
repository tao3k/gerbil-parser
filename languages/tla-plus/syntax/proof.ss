;;; -*- Gerbil -*-
;;; TLA+ recognition declarations; kinds, fields and references are DSL-derived.
(import (only-in :gerbil-parser/language-support/grammar deflanguage) ./lexical)
(export tla-plus-sany-candidate-syntax)
(deflanguage tla-plus-sany-candidate
  (syntax
    (lexical
     (root source-file)
     (lex (module-text ModuleText (module-text "----" "MODULE" "====" "(*" "*)" "\\*" "_"))
          (proof-step-name ProofStepName (text-profile (ref tla-proof-start)))
          (proof-reference ProofReference (text-profile (ref tla-proof-reference)))
          (horizontal-whitespace HorizontalWhitespace (horizontal-whitespace+))
          (newline Newline (newline+))
          (comment Comment (choice (line-comment "\\*") (nested-block-comment "(*" "*)")))
          (string String (quoted-string "\"")) (module-end ModuleEnd (character-run "=" 4))
          (module-border ModuleBorder (character-run "-" 4)) (number Number (number))
          (identifier Identifier (text-profile (ref tla-identifier)))
          (punctuation Punctuation
                       (literals "\\land" "\\lor" "\\cdot" "\\equiv" "-+->" "\\times" "..." "|" "||" "&&" "&" "$$"
                                 "$" "??" "%%" "##" "++" "--" "**" "//" "^^" "!!" "|-" "|=" "-|" "=|" "<:" ":=" "::="
                                 "\\oplus" "\\ominus" "\\odot" "\\oslash" "\\otimes" "\\uplus" "\\sqcap" "\\sqcup" "\\wr"
                                 "\\star" "\\bigcirc" "\\bullet" "\\prec" "\\succ" "\\preceq" "\\succeq" "\\sim" "\\simeq"
                                 "\\ll" "\\gg" "\\asymp" "\\subset" "\\supset" "\\supseteq" "\\approx" "\\cong" "\\sqsubset"
                                 "\\sqsubseteq" "\\sqsupset" "\\sqsupseteq" "\\doteq" "\\propto" "\\mod" "(+)" "(-)" "(.)"
                                 "(/)" "(\\X)" "-." "^+" "^*" "^#" "\\lnot" "\\neg" "<=>" "|->" "[]" "<>" "=>" "~>" "=="
                                 "/\\" "\\/" "\\AA" "\\EE" "\\A" "\\E" "\\notin" "\\intersect" "\\union" "\\cap" "\\cup"
                                 "\\subseteq" "\\X" "\\in" "\\" "\\div" "\\leq" "\\geq" "\\o" "\\circ" "@@" ":>" ".." "<<"
                                 ">>" "]_" ">>_" "<=" ">=" "=<" "/=" "->" "<-" "#" "=" "<" ">" "+" "-" "*" "/" "%" "^" "'"
                                 "~" "@" "[]ASSUME" "[]PROVE" "::" "(" ")" "[" "]" "{" "}" "," ":" "!" "_" ".")))
     (extras horizontal-whitespace newline comment)
     (keywords (module "MODULE") (extends "EXTENDS") (constant "CONSTANT") (constants "CONSTANTS")
               (variable "VARIABLE") (variables "VARIABLES") (theorem "THEOREM") (if "IF") (then "THEN")
               (else "ELSE") (choose "CHOOSE") (let "LET") (in "IN") (case "CASE") (other "OTHER")
               (enabled "ENABLED") (unchanged "UNCHANGED") (weak-fairness "WF_") (strong-fairness "SF_")
               (subset "SUBSET") (union "UNION") (domain "DOMAIN") (local "LOCAL") (recursive "RECURSIVE")
               (instance "INSTANCE") (with "WITH") (except "EXCEPT") (assume "ASSUME")
               (assumption "ASSUMPTION") (axiom "AXIOM") (proof "PROOF") (by "BY") (only "ONLY")
               (obvious "OBVIOUS") (omitted "OMITTED") (qed "QED") (suffices "SUFFICES") (have "HAVE")
               (take "TAKE") (witness "WITNESS") (pick "PICK") (define "DEFINE") (def "DEF") (defs "DEFS")
               (prove "PROVE") (new "NEW") (state "STATE") (action "ACTION") (temporal "TEMPORAL")
               (lambda "LAMBDA") (lemma "LEMMA") (corollary "COROLLARY") (proposition "PROPOSITION")
               (use "USE") (hide "HIDE"))
     (recoveries (module "GERBIL-PARSER-TLA-PLUS-SANY-CANDIDATE" preserve-source))
     (conflicts selective-glr)
     (case-insensitive #f)))
  (rules
    (source-file
     (node SourceFile (optional (field text module-text)) (field module module)
           (repeat (seq (optional (field text module-text)) (field module module)))
           (optional (field text module-text))))
    (module
     (node Module module-border "MODULE" (field name identifier) module-border
           (repeat (field item module-item)) module-end))
    (module-item
     (choice extends-declaration constant-declaration variable-declaration operator-definition
             function-definition infix-operator-definition prefix-operator-definition
             postfix-operator-definition recursive-declaration instance-declaration
             assumption-declaration theorem-declaration use-hide-declaration separator module))
    (extends-declaration
     (node ExtendsDeclaration "EXTENDS" (field module identifier)
           (repeat (seq "," (field module identifier)))))
    (constant-declaration
     (node ConstantDeclaration (choice "CONSTANT" "CONSTANTS") (field name identifier)
           (optional (seq "(" (field parameter "_") (repeat (seq "," (field parameter "_"))) ")"))
           (repeat
            (seq "," (field name identifier)
                 (optional (seq "(" (field parameter "_") (repeat (seq "," (field parameter "_"))) ")"))))))
    (variable-declaration
     (node VariableDeclaration (choice "VARIABLE" "VARIABLES") (field name identifier)
           (repeat (seq "," (field name identifier)))))
    (operator-definition
     (node OperatorDefinition (optional "LOCAL") (field name identifier)
           (optional
            (seq "(" (field parameter formal-parameter)
                 (repeat (seq "," (field parameter formal-parameter))) ")")) "=="
                 (field body (choice instance-expression expression))))
    (formal-parameter
     (choice
      (node FormalParameter (field name (choice identifier "_"))
            (optional (seq "(" (field parameter "_") (repeat (seq "," (field parameter "_"))) ")")))
      (node FormalParameter "_" (field name infix-symbol) "_")))
    (lambda-expression
     (prec right 1
           (node LambdaExpression "LAMBDA" (field name identifier)
                 (repeat (seq "," (field name identifier))) ":" (field body expression))))
    (label-expression
     (prec right 1 (node LabelExpression (field name expression) "::" (field body expression))))
    (function-definition
     (node FunctionDefinition (optional "LOCAL") (field name identifier) "["
           (field binding domain-binding) (repeat (seq "," (field binding domain-binding))) "]" "=="
           (field body expression)))
    (domain-binding
     (choice
      (node DomainBinding (field name identifier) (repeat (seq "," (field name identifier)))
            "\\in" (field domain expression)) tuple-domain-binding))
    (tuple-domain-binding
     (node TupleBinding "<<" (field name identifier) (repeat (seq "," (field name identifier)))
           ">>" "\\in" (field domain expression)))
    (single-domain-binding
     (choice (node DomainBinding (field name identifier) "\\in" (field domain expression))
             tuple-domain-binding))
    (infix-operator-definition
     (node OperatorDefinition (optional "LOCAL") (field parameter formal-parameter)
           (field name infix-symbol) (field parameter formal-parameter) "==" (field body expression)))
    (infix-symbol
     (choice "^" "/" "*" "-" "+" "=" "\\land" "\\lor" "~>" "=>" "\\cdot" "\\equiv" "-+->" "/="
             "\\subseteq" "\\in" "<" "\\leq" ">" "\\geq" "\\times" "\\" "\\intersect" "\\union" "..."
             ".." "|" "||" "&&" "&" "$$" "$" "??" "%%" "%" "##" "++" "--" "**" "//" "^^" "@@" "!!" "|-"
             "|=" "-|" "=|" "<:" ":>" ":=" "::=" "\\oplus" "\\ominus" "\\odot" "\\oslash" "\\otimes"
             "\\uplus" "\\sqcap" "\\sqcup" "\\div" "\\wr" "\\star" "\\o" "\\bigcirc" "\\bullet" "\\prec"
             "\\succ" "\\preceq" "\\succeq" "\\sim" "\\simeq" "\\ll" "\\gg" "\\asymp" "\\subset"
             "\\supset" "\\supseteq" "\\approx" "\\cong" "\\sqsubset" "\\sqsubseteq" "\\sqsupset"
             "\\sqsupseteq" "\\doteq" "\\propto" "/\\" "\\/" "<=>" "#" "<=" "=<" ">=" "\\X" "\\cap"
             "\\cup" "\\mod" "(+)" "(-)" "(.)" "(/)" "(\\X)" "\\circ"))
    (prefix-operator-definition
     (node OperatorDefinition (optional "LOCAL") (field name "-.")
           (field parameter formal-parameter) "==" (field body expression)))
    (postfix-operator-definition
     (node OperatorDefinition (optional "LOCAL") (field parameter formal-parameter)
           (field name (choice "^+" "^*" "^#")) "==" (field body expression)))
    (recursive-declaration
     (node RecursiveDeclaration "RECURSIVE" (field name identifier)
           (optional
            (seq "(" (field parameter formal-parameter)
                 (repeat (seq "," (field parameter formal-parameter))) ")"))
           (repeat (seq "," (field name identifier)))))
    (instance-declaration
     (node InstanceDeclaration (optional "LOCAL") "INSTANCE" (field module identifier)
           (optional
            (seq "WITH" (field substitution substitution)
                 (repeat (seq "," (field substitution substitution)))))))
    (substitution (node Substitution (field name identifier) "<-" (field value expression)))
    (instance-expression
     (node InstanceExpression "INSTANCE" (field module identifier)
           (optional
            (seq "WITH" (field substitution substitution)
                 (repeat (seq "," (field substitution substitution)))))))
    (junction-expression
     (choice
      (node JunctionExpression (field operator (layout-start "/\\")) (field body expression)
            (repeat (seq (field operator (layout-next "/\\")) (field body expression)))
            (layout-end ")" "]" "}" ">>" "]_" ">>_" "," ":" "->" "|->" "THEN" "ELSE" "IN"))
      (node JunctionExpression (field operator (layout-start "\\/")) (field body expression)
            (repeat (seq (field operator (layout-next "\\/")) (field body expression)))
            (layout-end ")" "]" "}" ">>" "]_" ">>_" "," ":" "->" "|->" "THEN" "ELSE" "IN"))))
    (assumption-declaration
     (node AssumptionDeclaration (choice "ASSUME" "ASSUMPTION" "AXIOM")
           (optional (seq (field name identifier) "==")) (field body expression)))
    (theorem-declaration
     (node TheoremDeclaration (choice "THEOREM" "PROPOSITION" "LEMMA" "COROLLARY")
           (optional (seq (field name identifier) "==")) (field body (choice expression assume-prove))
           (optional (field proof proof))))
    (fact-list (seq (field item proof-fact) (repeat (seq "," (field item proof-fact)))))
    (proof-fact (choice expression infix-symbol (seq "MODULE" identifier)))
    (use-hide-declaration
     (node UseHideDeclaration (choice "USE" "HIDE") (optional "ONLY") (optional fact-list)
           (optional (seq (choice "DEF" "DEFS") fact-list))))
    (terminal-proof
     (node TerminalProof (optional "PROOF")
           (choice "OBVIOUS" "OMITTED"
                   (seq "BY" (optional "ONLY") (optional fact-list)
                        (optional (seq (choice "DEF" "DEFS") fact-list))))))
    (proof (choice terminal-proof (node Proof (optional "PROOF") (repeat1 (field step proof-step)))))
    (proof-step
     (node ProofStep (field name (choice proof-step-name proof-reference))
           (field body
                  (choice "QED" use-hide-declaration instance-declaration
                          (seq (optional "DEFINE") (repeat1 let-definition)) proof-command
                          (seq (optional "SUFFICES") (choice assume-prove expression))))
           (optional (field proof terminal-proof))))
    (proof-command
     (node ProofCommand
           (choice (seq (field operator (choice "HAVE" "CASE")) (field item expression))
                   (seq (field operator "WITNESS") fact-list) (seq (field operator "TAKE") proof-bindings)
                   (seq (field operator "PICK") proof-bindings ":" (field item expression)))))
    (proof-bindings
     (choice (seq domain-binding (repeat (seq "," domain-binding)))
             (seq identifier (repeat (seq "," identifier)))))
    (assume-prove
     (node AssumeProve (choice "ASSUME" "[]ASSUME") (field assumption proof-assumption)
           (repeat (seq "," (field assumption proof-assumption))) (choice "PROVE" "[]PROVE")
           (field conclusion expression)))
    (proof-assumption (choice expression assume-prove new-symbol))
    (new-symbol
     (node NewSymbol
           (choice (seq "NEW" (optional "CONSTANT")) "CONSTANT"
                   (seq (optional "NEW") (choice "VARIABLE" "STATE" "ACTION" "TEMPORAL")))
           (field name formal-parameter) (optional (seq "\\in" (field domain expression)))))
    (separator (node Separator module-border))
    (expression
     (choice
      (prec left 30
            (node Expression (field left expression) (field operator (choice "\\land" "\\lor"))
                  (field right expression)))
      (prec left 40
            (node Expression (field left expression) (field operator (choice "\\cdot"))
                  (field right expression)))
      (prec none 15
            (node Expression (field left expression) (field operator (choice "\\equiv" "-+->"))
                  (field right expression)))
      (prec left 60
            (node Expression (field left expression)
                  (field operator (choice "\\times" "|" "||" "%%" "++" "\\oplus" "(+)"))
                  (field right expression)))
      (prec none 50
            (node Expression (field left expression) (field operator (choice "..." "!!" "\\wr"))
                  (field right expression)))
      (prec left 70
            (node Expression (field left expression)
                  (field operator
                         (choice "&&" "&" "**" "\\odot" "\\otimes" "\\star" "\\bigcirc" "\\bullet" "(.)"
                                 "(\\X)")) (field right expression)))
      (prec left 50
            (node Expression (field left expression)
                  (field operator (choice "$$" "$" "??" "##" "\\uplus" "\\sqcap" "\\sqcup"))
                  (field right expression)))
      (prec left 65
            (node Expression (field left expression) (field operator (choice "--" "\\ominus" "(-)"))
                  (field right expression)))
      (prec none 70
            (node Expression (field left expression) (field operator (choice "//" "\\oslash" "(/)"))
                  (field right expression)))
      (prec none 80
            (node Expression (field left expression) (field operator (choice "^^"))
                  (field right expression)))
      (prec none 40
            (node Expression (field left expression)
                  (field operator
                         (choice "|-" "|=" "-|" "=|" ":=" "::=" "\\prec" "\\succ" "\\preceq" "\\succeq" "\\sim"
                                 "\\simeq" "\\ll" "\\gg" "\\asymp" "\\subset" "\\supset" "\\supseteq" "\\approx"
                                 "\\cong" "\\sqsubset" "\\sqsubseteq" "\\sqsupset" "\\sqsupseteq" "\\doteq"
                                 "\\propto")) (field right expression)))
      (prec none 43
            (node Expression (field left expression) (field operator (choice "<:"))
                  (field right expression)))
      (prec none 60
            (node Expression (field left expression) (field operator (choice "\\mod"))
                  (field right expression)))
      (prec right 10
            (node Expression (field left expression) (field operator (choice "<=>" "=>"))
                  (field right expression)))
      (prec none 15
            (node Expression (field left expression) (field operator "~>") (field right expression)))
      (prec left 20
            (node Expression (field left expression) (field operator "\\/") (field right expression)))
      (prec left 30
            (node Expression (field left expression) (field operator "/\\") (field right expression)))
      (prec none 40
            (node Expression (field left expression)
                  (field operator
                         (choice "=" "#" "/=" "<" ">" "<=" "=<" ">=" "\\leq" "\\geq" "\\in" "\\notin"
                                 "\\subseteq")) (field right expression)))
      (prec left 42
            (node Expression (field left expression) (field operator "@@") (field right expression)))
      (prec none 43
            (node Expression (field left expression) (field operator ":>") (field right expression)))
      (prec left 45
            (node Expression (field left expression)
                  (field operator (choice "\\cup" "\\union" "\\cap" "\\intersect" "\\"))
                  (field right expression)))
      (prec left 50
            (node Expression (field left expression) (field operator "..") (field right expression)))
      (prec left 60
            (node Expression (field left expression) (field operator "+") (field right expression)))
      (prec left 65
            (node Expression (field left expression) (field operator "-") (field right expression)))
      (prec left 70
            (node Expression (field left expression)
                  (field operator (choice "*" "\\o" "\\circ" "%" "\\X")) (field right expression)))
      (prec none 70
            (node Expression (field left expression) (field operator (choice "/" "\\div"))
                  (field right expression)))
      (prec none 80
            (node Expression (field left expression) (field operator "^") (field right expression)))
      (prec right 90 prefix-expression) (prec left 100 postfix-expression)
      (prec left 110 function-application) (prec left 110 operator-application)
      (prec left 110 record-field-expression) if-expression (prec right 1 choose-expression)
      (prec right 1 quantified-expression) (prec right 1 let-expression)
      (prec right 1 case-expression) function-constructor function-set-expression
      except-expression record-expression set-filter-expression record-set-expression
      set-map-expression temporal-subscript-expression angle-action-expression fairness-expression
      grouped-expression tuple-expression set-expression name-expression at-expression
      (prec right 1 label-expression) junction-expression qualified-name-expression
      (prec left 120 instance-qualified-expression) number-expression string-expression
      (prec right 1 lambda-expression)))
    (if-expression
     (prec right 1
           (node IfExpression "IF" (field condition expression) "THEN" (field consequent expression)
                 "ELSE" (field alternative expression))))
    (choose-expression
     (prec right 1
           (node ChooseExpression "CHOOSE" (field name identifier)
                 (optional (seq "\\in" (field domain expression))) ":" (field predicate expression))))
    (quantified-expression
     (choice
      (prec right 1
            (node QuantifiedExpression (field quantifier (choice "\\A" "\\E"))
                  (field binding domain-binding) (repeat (seq "," (field binding domain-binding))) ":"
                  (field predicate expression)))
      (prec right 1
            (node QuantifiedExpression (field quantifier (choice "\\A" "\\E" "\\AA" "\\EE"))
                  (field name identifier) (repeat (seq "," (field name identifier))) ":"
                  (field predicate expression)))))
    (let-expression
     (prec right 1
           (node LetExpression "LET" (field definition let-definition)
                 (repeat (field definition let-definition)) "IN" (field body expression))))
    (let-definition
     (choice local-definition local-function-definition infix-operator-definition
             prefix-operator-definition postfix-operator-definition recursive-declaration
             instance-declaration))
    (local-function-definition
     (node LocalFunctionDefinition (field name identifier) "[" (field binding domain-binding)
           (repeat (seq "," (field binding domain-binding))) "]" "==" (field body expression)))
    (local-definition
     (node LocalDefinition (field name identifier)
           (optional
            (seq "(" (field parameter formal-parameter)
                 (repeat (seq "," (field parameter formal-parameter))) ")")) "=="
                 (field body (choice expression instance-expression))))
    (case-expression
     (prec right 1
           (node CaseExpression "CASE" (field arm case-arm) (repeat (seq "[]" (field arm case-arm)))
                 (optional (prec right 1 (seq "[]" "OTHER" "->" (field other expression)))))))
    (case-arm
     (prec right 1 (node CaseArm (field condition expression) "->" (field result expression))))
    (prefix-expression
     (prec right 90
           (node PrefixExpression
                 (field operator
                        (choice "~" "\\lnot" "\\neg" "-." "-" "[]" "<>" "ENABLED" "UNCHANGED" "SUBSET" "UNION"
                                "DOMAIN")) (field operand expression))))
    (postfix-expression
     (prec left 100
           (node PostfixExpression (field operand expression)
                 (field operator (choice "'" "^+" "^*" "^#")))))
    (function-application
     (prec left 110
           (node FunctionApplication (field function expression) "[" (field argument expression)
                 (repeat (seq "," (field argument expression))) "]")))
    (operator-application
     (prec left 110
           (node OperatorApplication (field operator (choice identifier qualified-name-expression)) "("
                 (optional (field argument (choice expression infix-symbol)))
                 (repeat (seq "," (field argument (choice expression infix-symbol)))) ")")))
    (record-field-expression
     (prec left 110
           (node RecordFieldExpression (field record expression) "." (field field identifier))))
    (function-constructor
     (node FunctionConstructor "[" (field binding domain-binding)
           (repeat (seq "," (field binding domain-binding))) "|->" (field body expression) "]"))
    (function-set-expression
     (node FunctionSetExpression "[" (field domain expression) "->" (field codomain expression) "]"))
    (except-expression
     (node ExceptExpression "[" (field base expression) "EXCEPT" (field update except-update)
           (repeat (seq "," (field update except-update))) "]"))
    (except-update
     (node ExceptUpdate "!" (field path except-path) (repeat (field path except-path)) "="
           (field value expression)))
    (except-path
     (choice
      (node ExceptIndex "[" (field index expression) (repeat (seq "," (field index expression)))
            "]") (node ExceptField "." (field name identifier))))
    (at-expression (node AtExpression "@"))
    (record-expression
     (node RecordExpression "[" (field name identifier) "|->" (field value expression)
           (repeat (seq "," (field name identifier) "|->" (field value expression))) "]"))
    (record-set-expression
     (node RecordSetExpression "[" (field name identifier) ":" (field domain expression)
           (repeat (seq "," (field name identifier) ":" (field domain expression))) "]"))
    (set-filter-expression
     (prec dynamic 1
           (node SetFilterExpression "{" (field binding single-domain-binding) ":"
                 (field predicate expression) "}")))
    (set-map-expression
     (node SetMapExpression "{" (field body expression) ":" (field binding domain-binding)
           (repeat (seq "," (field binding domain-binding))) "}"))
    (temporal-subscript-expression
     (node TemporalSubscriptExpression "[" (field action expression) "]_"
           (field subscript reduced-subscript-expression)))
    (angle-action-expression
     (node AngleActionExpression "<<" (field action expression) ">>_"
           (field subscript reduced-subscript-expression)))
    (reduced-subscript-expression
     (choice name-expression qualified-name-expression instance-qualified-expression
             tuple-expression grouped-expression))
    (fairness-expression
     (node FairnessExpression (field operator (choice "WF_" "SF_"))
           (field subscript reduced-subscript-expression) "(" (field action expression) ")"))
    (grouped-expression (node GroupedExpression "(" (field expression expression) ")"))
    (tuple-expression
     (node TupleExpression "<<" (optional (field item expression))
           (repeat (seq "," (field item expression))) ">>"))
    (set-expression
     (node SetExpression "{" (optional (field item expression))
           (repeat (seq "," (field item expression))) "}"))
    (name-expression (node NameExpression (field name (choice identifier proof-reference))))
    (selector
     (choice identifier number infix-symbol ":" "@" "<<" ">>"
             (node SelectorArguments "(" (field argument expression)
                   (repeat (seq "," (field argument expression))) ")")))
    (qualified-name-expression
     (node QualifiedNameExpression (field module identifier)
           (repeat1 (seq "!" (field name selector)))))
    (instance-qualified-expression
     (prec left 120
           (node InstanceQualifiedExpression (field instance operator-application) "!"
                 (field name selector) (repeat (seq "!" (field name selector))))))
    (number-expression (node NumberExpression (field value number)))
    (string-expression (node StringExpression (field value string)))))

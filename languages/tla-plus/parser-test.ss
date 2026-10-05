;;; -*- Gerbil -*-
;;; Declarative layout fields, concurrency and engine receipts.
(import :gerbil-parser/language-test-support ./parser)
(deflanguage-parser-tests tla-plus-layout-parser-test "TLA+ column layout grammar"
  (loader tla-plus-layout-language)
  (accepted "aligned markers form one list" "---- MODULE J ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n"
    (field-counts JunctionExpression body (2)))
  (accepted "nested quantifier closes at outer reference" "---- MODULE J ----\nInit ==\n  /\\ \\E x \\in S :\n       /\\ x = 1\n       /\\ x = 2\n  /\\ TRUE\n====\n"
    (field-counts JunctionExpression body (2 2)))
  (accepted "different columns remain distinct" "---- MODULE J ----\nInit ==\n  /\\ TRUE\n   /\\ FALSE\n====\n"
    (field-counts JunctionExpression body (1)))
  (accepted "CRLF and tabs retain alignment" "---- MODULE J ----\r\nInit ==\r\n\t/\\ TRUE\r\n\t/\\ FALSE\r\n====\r\n"
    (field-counts JunctionExpression body (2)))
  (parallel "nine requests isolate layout frames and columns" 9
    (list "---- MODULE A ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n" "---- MODULE B ----\nInit ==\n  \\/ TRUE\n  \\/ FALSE\n====\n" "---- MODULE C ----\nInit ==\n  /\\ \\E x \\in S :\n       /\\ x = 1\n       /\\ x = 2\n  /\\ TRUE\n====\n"))
  (incremental "edits retain fresh layout semantics" "---- MODULE J ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n"
    (replace "FALSE" "TRUE") (freshFallback? #t))
  (rejected "dangling item remains rejected" "---- MODULE J ----\nInit ==\n  /\\\n====\n")
  (recovery-rejected "recovery retains layout rejection" "---- MODULE J ----\nInit ==\n  /\\\n====\n" (outcome 'disabled)))

(deflanguage-parser-tests tla-plus-core-parser-test "TLA+ core grammar"
  (loader tla-plus-core-language)
  (accepted "modular multiline action contracts keep native source bytes"
    "---- MODULE Modular ----\nVARIABLES active,\n queued\nWork == INSTANCE WorkLifecycle\nInit ==\n  /\\ Work!Init\n  /\\ active = TRUE\nNext == Work!Step \\/\n        (active' = FALSE /\\\n         UNCHANGED <<queued,\n                    active>>)\n====\n"
    (nodes InstanceExpression QualifiedNameExpression JunctionExpression))
  (accepted-many "set union and intersection keep official ASCII spellings"
    (list "---- MODULE SetOperator ----\nLeft == {\"a\"} \\cup {\"b\"}\nRight == Left \\in {\"a\", \"b\"}\n====\n" "---- MODULE SetOperator ----\nLeft == {\"a\"} \\union {\"b\"}\nRight == Left \\in {\"a\", \"b\"}\n====\n" "---- MODULE SetOperator ----\nLeft == {\"a\"} \\cap {\"b\"}\nRight == Left \\in {\"a\", \"b\"}\n====\n" "---- MODULE SetOperator ----\nLeft == {\"a\"} \\intersect {\"b\"}\nRight == Left \\in {\"a\", \"b\"}\n====\n") (nodes Expression))
  (rejected "set operators require a right operand"
    "---- MODULE BrokenSet ----\nLeft == {\"a\"} \\cup\n====\n")
  (accepted "Temporal set maps, products, difference, and subset retain bytes"
    "---- MODULE TemporalSyntax ----\nIds == {o[1] : o \\in Observations}\nRemaining == Ids \\ {id}\nPairs == Ids \\X Cuts\nWithin == Remaining \\subseteq Ids\n====\n"
    (nodes SetMapExpression))
  (rejected-many "incomplete Temporal operators remain syntax errors"
    (list "---- MODULE BrokenTemporal ----\nIds == {o[1] : o \\in}\n====\n" "---- MODULE BrokenTemporal ----\nPairs == Ids \\X\n====\n" "---- MODULE BrokenTemporal ----\nRemaining == Ids \\\n====\n" "---- MODULE BrokenTemporal ----\nWithin == Ids \\subseteq\n====\n"))
  (rejected-many "incomplete modular forms remain syntax errors"
    (list "---- MODULE Broken ----\nWork == INSTANCE\n====\n" "---- MODULE Broken ----\nValue == Work!\n====\n" "---- MODULE Broken ----\nVARIABLES active,\n\n====\n" "---- MODULE Broken ----\nInit ==\n /\\\n====\n" "---- MODULE Broken ----\nNext == TRUE /\\\n\n====\n"))
  (property "native syntax and corpus identities are immutable" (bindings)
    (equal +tla-plus-syntax-source+ "Specifying Systems, Chapter 15: TLAPlusGrammar")
    (equal +tla-plus-sany-release+ "v1.7.4")
    (equal +tla-plus-sany-commit+ "5a47802b5c391f59ecdd44117981f4ff8c0656ba")
    (equal +tla-plus-sany-grammar-blob+ "bf9e7acb5337f4b6c2a4d6a973a1a65c95e72f56")
    (equal +tla-plus-sany-grammar-digest+ "sha256:15edd079cf16cf91556ba66b496c9ffc16f26ab30856753ed58e60b0d54f2d07")
    (equal +tla-plus-examples-commit+ "ceeaa904140e3e03781cb2a79cd6c6d8b8b08e10"))
  (fixture-catalog "core fixture counts and first source digest" (total 6) (accepted 5) (rejected 1)
    (first-digest "sha256:985903176db4725f9cf25df84ad84dcd53ba94be80d26298b87ec86fbc9b08b3"))
  (fixture-group "all admitted modules publish lossless structural CSTs" accepted (root SourceFile))
  (accepted-fixture "nested block comments remain one lossless trivia token" 2 (token-counts (comment 1)))
  (accepted "unary negation is admitted by the native lexical contract"
    "---- MODULE Negation ----\nVARIABLES enabled, ready\nDisabled == ~enabled /\\ ready\nNext == enabled' = FALSE /\\ ready' = TRUE\n====\n")
  (model-receipt "one qualification API composes parser and TLC receipts" "Qualified"
    "---- MODULE Qualified ----\nVARIABLE enabled\nInit == enabled = FALSE\nNext == enabled' = TRUE\n====\n" "INIT Init\nNEXT Next\n"
    (stdout "TLC2 Version fixture\nProgress(3) at 00:00:00: 7 states generated, 7 distinct states found, 5 states left on queue.\nModel checking completed. No error has been found.\n4 states generated, 2 distinct states found, 0 states left on queue.\nThe depth of the complete state graph search is 1.\n" 0) 1
    (schema "gerbil-parser.tla-plus-model-qualification.v1") (admitted #t) (syntax-accepted #t) (roundtrip #t)
    (states-generated 4) (distinct-states 2) (states-left 0) (graph-depth 1))
  (model-receipt "qualification fails closed on misleading TLC completion" "Rejected"
    "---- MODULE Rejected ----\nVARIABLE enabled\nInit == enabled = FALSE\nNext == enabled' = TRUE\n====\n" "INIT Init\nNEXT Next\n"
    (stdout "TLC2 Version fixture\nModel checking completed. No error has been found.\n4 states generated, 2 distinct states found, 0 states left on queue.\nThe depth of the complete state graph search is 1.\n" 7) 1
    (admitted #f) (nonzero exit-status))
  (model-receipt "progress-only output never establishes completed state counts" "Rejected"
    "---- MODULE Rejected ----\nVARIABLE enabled\nInit == enabled = FALSE\nNext == enabled' = TRUE\n====\n" "INIT Init\nNEXT Next\n"
    (stdout "TLC2 Version fixture\nProgress(3) at 00:00:00: 7 states generated, 7 distinct states found, 5 states left on queue.\nModel checking completed. No error has been found.\n" 0) 1
    (admitted #f) (states-generated #f))
  (model-receipt "syntax rejection stops before resolving TLC" "Malformed"
    "---- MODULE Malformed ----\nVARIABLE x\nBroken == IF x = 0 THEN ELSE x\n====\n" "INIT Broken\n" (unresolved) 1
    (admitted #f) (syntax-accepted #f) (tool-path #f) (exit-status #f))
  (rejected "unterminated nested comments fail as one typed artifact"
    "---- MODULE Broken ----\n(* outer (* nested *)\nVARIABLE x\n====\n"
    (diagnostics 1))
  (fixture-group "recognized but malformed expressions fail closed" rejected (diagnostics 1)))

(deflanguage-parser-tests tla-plus-sany-candidate-parser-test "TLA+ SANY candidate grammar"
  (loader tla-plus-sany-candidate-language)
  (distinct-contract "candidate identity is distinct from the published layout contract" tla-plus-layout-language)
  (identity "candidate version remains pinned" (version "p4-draft"))
  (syntax-kind "TerminalProof metadata admits emitted fields" TerminalProof node (fact definition item))
  (accepted "TerminalProof metadata admits the fields emitted by its fact list"
    "---- MODULE P ----\nTHEOREM TRUE\nBY TRUE\n====\n"
    (fields TerminalProof item))
  (accepted "aligned markers make one list"
    "---- MODULE J ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n"
    (field-counts JunctionExpression body (2)))
  (accepted "an explicit action closer ends an aligned list on the same line"
    "---- MODULE J ----\nCONSTANT S\nVARIABLE x\nQ == [][ /\\ (\\A i \\in S : i = i)\n         /\\ (\\A j \\in S : j = j) ]_<<x>>\n====\n"
    (field-counts JunctionExpression body (2)))
  (accepted "nested quantifier lists close at the outer reference"
    "---- MODULE J ----\nInit ==\n  /\\ \\E x \\in S :\n       /\\ x = 1\n       /\\ x = 2\n  /\\ TRUE\n====\n"
    (field-counts JunctionExpression body (2 2)))
  (accepted "different marker columns do not join one list"
    "---- MODULE J ----\nInit ==\n  /\\ TRUE\n   /\\ FALSE\n====\n"
    (field-counts JunctionExpression body (1)))
  (accepted "CRLF and tab stops retain marker alignment"
    "---- MODULE J ----\r\nInit ==\r\n\t/\\ TRUE\r\n\t/\\ FALSE\r\n====\r\n"
    (field-counts JunctionExpression body (2)))
  (accepted "variable-width borders and function-set syntax"
    "-------- MODULE J --------\n----------------\nF == [S -> T]\n==========\n"
    (counts (FunctionSetExpression 1)))
  (accepted "operator constants, EXCEPT updates, and field selection"
    "---- MODULE J ----\nCONSTANTS Send(_, _), T\nF == [pc EXCEPT ![self] = \"b1\"]\nG == participant[i].decision\n====\n"
    (counts (ExceptExpression 1) (RecordFieldExpression 1)))
  (accepted "instantiated module operator qualification"
    "---- MODULE J ----\nSpec == Inner(q)!ISpec\n====\n"
    (counts (InstanceQualifiedExpression 1)))
  (accepted "infix operator definitions and qualified symbolic operators"
    "---- MODULE N ----\na \\div b == R!\\div(a, b)\na % b == a - b * (a \\div b)\na < b == (a \\leq b) /\\ (a # b)\n====\n"
    (counts (OperatorDefinition 3)))
  (accepted "record sets keep colon fields distinct from record values"
    "---- MODULE R ----\nType == [val : Data, rdy : {0, 1}]\nValue == [val |-> x, rdy |-> TRUE]\n====\n"
    (counts (RecordSetExpression 1) (RecordExpression 1)))
  (accepted "temporal leads-to is one lossless infix operator"
    "---- MODULE Leads ----\nLeads == A ~> B\nGrouped == (A /\\ B) ~> C\n====\n"
    (counts (Expression 3)))
  (rejected "action subscripts reject a missing closing delimiter"
    "---- MODULE Bad ----\nP == [A]x\n====\n")
  (accepted "action subscripts require SANY's closing delimiter"
    "---- MODULE Tuple ----\nP == [A]_<<x,y>>\nQ == [][A]_(<<x,y>>)\nR == <><<A>>_x\n====\n"
    (counts (TemporalSubscriptExpression 2) (AngleActionExpression 1)))
  (accepted "function definitions retain domain bindings in LET"
    "---- MODULE Functions ----\nF[x \\in S, y \\in T] == <<x,y>>\nL == LET G[x \\in S] == x H[y \\in T] == y IN <<G,H>>\n====\n"
    (counts (FunctionDefinition 1) (LocalFunctionDefinition 2) (DomainBinding 4)))
  (parallel "concurrent requests keep layout columns and frames isolated" 9 (list "---- MODULE A ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n" "---- MODULE B ----\nInit ==\n  \\/ TRUE\n  \\/ FALSE\n====\n" "---- MODULE C ----\nInit ==\n  /\\ \\E x \\in S :\n       /\\ x = 1\n       /\\ x = 2\n  /\\ TRUE\n====\n"))
  (incremental "edits reparse with the same layout semantics" "---- MODULE J ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n" (replace "FALSE" "TRUE") (freshFallback? #t))
  (rejected "dangling list item remains rejected"
    "---- MODULE J ----\nInit ==\n  /\\\n====\n")
  (recovery-rejected "recovery keeps layout rejection unchanged" "---- MODULE J ----\nInit ==\n  /\\\n====\n" (outcome 'disabled)))

(deflanguage-parser-tests sany-closure-test "SANY corpus syntax families"
  (loader tla-plus-sany-candidate-language)
  (accepted "document text and nested and concatenated modules retain nodes"
    "notes\n---- MODULE 2Outer ----\n---- MODULE Inner ----\nP == TRUE\n====\nQ == \"==== ---- MODULE Fake ----\"\n====\ntrailing notes\n---- MODULE Peer ----\nR == FALSE\n====\nend notes\n"
    (diagnostics 0) (counts (Module 3)))
  (accepted "higher order formals lambdas labels and operator declarations"
    "---- MODULE Higher ----\nApply(Op(_,_), x, y) == Op(x,y)\nf ** g == f\n-. x == x\nx ^+ == x\nP == Apply(**, a, b)\nQ == Apply(LAMBDA x,y: x, a, b)\nR == tag(x):: x = x\nS == LET IsGroup(G, _+_) == G IN IsGroup(S,+)\n====\n"
    (diagnostics 0) (counts (LambdaExpression 1) (LabelExpression 1) (FormalParameter 9)))
  (accepted "CASE application and selection stay in the final arm"
    "---- MODULE Cases ----\nP == CASE A -> B [] OTHER -> F[x]\nQ == CASE A -> B [] OTHER -> F.x\nR == CASE A -> B [] OTHER -> x+y\n====\n"
    (diagnostics 0) (counts (CaseExpression 3) (FunctionApplication 1) (RecordFieldExpression 1)))
  (accepted "outdent restores a precedence suppressed reduction"
    "---- MODULE Outdent ----\nP ==\n  /\\ \\A p \\in S :\n    /\\ IF s # top THEN s.ctxt = p ELSE FALSE\n    => pc[p] = s\n  /\\ TRUE\n====\n"
    (diagnostics 0))
  (accepted "nested IF lists can return through multiple enclosing references"
    "---- MODULE NestedIF ----\ne2(self) == /\\ pc[self] = s\n            /\\ IF unchecked[self] # {}\n                  THEN /\\ \\E i \\in unchecked[self]:\n                            /\\ unchecked = unchecked\n                            /\\ IF num[i] > max[self]\n                                  THEN /\\ max = max\n                                  ELSE /\\ TRUE\n                                       /\\ max = max\n                       /\\ pc = pc\n                  ELSE /\\ pc = pc\n                       /\\ UNCHANGED <<unchecked,max>>\n            /\\ UNCHANGED <<num,flag>>\n====\n"
    (diagnostics 0))
  (accepted "proof levels qualifiers assumptions and commands retain structure"
    "---- MODULE Proofs ----\nLEMMA P == ASSUME NEW x PROVE TRUE\n<1>1. SUFFICES ASSUME TRUE PROVE TRUE\n  OBVIOUS\n<1>2. TRUE\n  <2>1. CASE TRUE\n    BY P!(x)\n  <2>. QED OBVIOUS\n<1>. QED BY <1>1, <1>2\n====\n"
    (diagnostics 0) (counts (ProofStep 5) (AssumeProve 2) (SelectorArguments 1)))
  (native-entry "public loader descriptor and native entry: accepted" "---- MODULE Entry ----\nTHEOREM TRUE\nOBVIOUS\n====\n" accepted)
  (native-entry "public loader descriptor and native entry: rejected" "---- MODULE Entry ----\nTHEOREM TRUE\n<1>1. TRUE OBVIOUS\n====\n" rejected)
  (portable-rejected "build ABI rejects public candidate policy rather than emitting recognition only" "languages/tla-plus/parser.ss#tla-plus-sany-candidate-language-grammar"
    "language parser policy is unsupported by standalone Rust AOT")
  (rejected-many "missing QED inconsistent levels and proof of HAVE reject"
    (list "---- MODULE Bad ----\nTHEOREM TRUE\n<1>1. TRUE OBVIOUS\n====\n" "---- MODULE Bad ----\nTHEOREM TRUE\n<1>1. TRUE\n<2>1. TRUE OBVIOUS\n<3>2. TRUE OBVIOUS\n<2>. QED OBVIOUS\n<1>. QED OBVIOUS\n====\n" "---- MODULE Bad ----\nTHEOREM TRUE\n<1>1. HAVE TRUE\n<2>. QED OBVIOUS\n<1>. QED OBVIOUS\n====\n")))

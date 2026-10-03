#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Structural and rejection regressions for the complete pinned corpus slice.
(import :std/test
        :gerbil-parser/languages/tla-plus/sany-candidate
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/cst)
(export sany-closure-test)

(def (kind-count value kind)
  (cond ((syntax-node? value)
         (+ (if (eq? (syntax-node-kind value) kind) 1 0)
            (apply + (map (cut kind-count <> kind) (syntax-node-children value)))))
        ((syntax-field? value)
         (apply + (map (cut kind-count <> kind) (syntax-field-children value))))
        (else 0)))

(def (accepted-tree source)
  (let (artifact (parse-tla-plus-sany-candidate source))
    (unless (parse-artifact-success? artifact)
      (error "SANY closure fixture rejected"
             (parse-artifact-ref artifact 'diagnostics)))
    (check (parse-artifact-success? artifact) => #t)
    (check (parse-artifact-valid? artifact) => #t)
    (check (parse-artifact-roundtrip artifact) => source)
    (parse-artifact->cst artifact)))

(def sany-closure-test
  (test-suite "SANY corpus syntax families"
    (test-case "document text and nested and concatenated modules retain nodes"
      (let* ((source "notes\n---- MODULE 2Outer ----\n---- MODULE Inner ----\nP == TRUE\n====\nQ == \"==== ---- MODULE Fake ----\"\n====\ntrailing notes\n---- MODULE Peer ----\nR == FALSE\n====\nend notes\n")
             (tree (accepted-tree source)))
        (check (kind-count tree 'Module) => 3)))
    (test-case "higher order formals lambdas labels and operator declarations"
      (let (tree (accepted-tree
        "---- MODULE Higher ----\nApply(Op(_,_), x, y) == Op(x,y)\nf ** g == f\n-. x == x\nx ^+ == x\nP == Apply(**, a, b)\nQ == Apply(LAMBDA x,y: x, a, b)\nR == tag(x):: x = x\nS == LET IsGroup(G, _+_) == G IN IsGroup(S,+)\n====\n"))
        (check (kind-count tree 'LambdaExpression) => 1)
        (check (kind-count tree 'LabelExpression) => 1)
        (check (kind-count tree 'FormalParameter) => 9)))
    (test-case "CASE application and selection stay in the final arm"
      (let (tree (accepted-tree
        "---- MODULE Cases ----\nP == CASE A -> B [] OTHER -> F[x]\nQ == CASE A -> B [] OTHER -> F.x\nR == CASE A -> B [] OTHER -> x+y\n====\n"))
        (check (kind-count tree 'CaseExpression) => 3)
        (check (kind-count tree 'FunctionApplication) => 1)
        (check (kind-count tree 'RecordFieldExpression) => 1)))
    (test-case "outdent restores a precedence suppressed reduction"
      (accepted-tree
       "---- MODULE Outdent ----\nP ==\n  /\\ \\A p \\in S :\n    /\\ IF s # top THEN s.ctxt = p ELSE FALSE\n    => pc[p] = s\n  /\\ TRUE\n====\n"))
    (test-case "nested IF lists can return through multiple enclosing references"
      (accepted-tree
       "---- MODULE NestedIF ----\ne2(self) == /\\ pc[self] = s\n            /\\ IF unchecked[self] # {}\n                  THEN /\\ \\E i \\in unchecked[self]:\n                            /\\ unchecked = unchecked\n                            /\\ IF num[i] > max[self]\n                                  THEN /\\ max = max\n                                  ELSE /\\ TRUE\n                                       /\\ max = max\n                       /\\ pc = pc\n                  ELSE /\\ pc = pc\n                       /\\ UNCHANGED <<unchecked,max>>\n            /\\ UNCHANGED <<num,flag>>\n====\n"))
    (test-case "proof levels qualifiers assumptions and commands retain structure"
      (let (tree (accepted-tree
        "---- MODULE Proofs ----\nLEMMA P == ASSUME NEW x PROVE TRUE\n<1>1. SUFFICES ASSUME TRUE PROVE TRUE\n  OBVIOUS\n<1>2. TRUE\n  <2>1. CASE TRUE\n    BY P!(x)\n  <2>. QED OBVIOUS\n<1>. QED BY <1>1, <1>2\n====\n"))
        (check (kind-count tree 'ProofStep) => 5)
        (check (kind-count tree 'AssumeProve) => 2)
        (check (kind-count tree 'SelectorArguments) => 1)))
    (test-case "missing QED inconsistent levels and proof of HAVE reject"
      (for-each
       (lambda (proof)
         (let (artifact (parse-tla-plus-sany-candidate
                         (string-append "---- MODULE Bad ----\nTHEOREM TRUE\n" proof "\n====\n")))
           (check (parse-artifact-success? artifact) => #f)
           (check (parse-artifact-valid? artifact) => #t)))
       '("<1>1. TRUE OBVIOUS"
         "<1>1. TRUE\n<2>1. TRUE OBVIOUS\n<3>2. TRUE OBVIOUS\n<2>. QED OBVIOUS\n<1>. QED OBVIOUS"
         "<1>1. HAVE TRUE\n<2>. QED OBVIOUS\n<1>. QED OBVIOUS")))))

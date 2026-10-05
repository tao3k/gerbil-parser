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

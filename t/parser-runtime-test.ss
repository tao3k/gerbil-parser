#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/src/runtime/parser
        :gerbil-parser/src/reference/arithmetic)

(def parser-runtime-tests
  (test-suite "parser runtime"
    (test-case "source is lossless and precedence is accepted"
      (let (receipt (parse-source arithmetic-parser " 1 + 2 * value "))
        (check (parse-success? receipt) => #t)
        (check (parse-roundtrip receipt) => " 1 + 2 * value ")
        (check (cdr (assq 'kind (parse-receipt-ref receipt 'tree)))
               => 'BinaryExpression)))
    (test-case "generated prefix dispatch and grouping"
      (let (receipt (parse-source arithmetic-parser "-(1 + 2)"))
        (check (parse-success? receipt) => #t)
        (check (parse-roundtrip receipt) => "-(1 + 2)")))
    (test-case "failure emits exactly one typed diagnostic"
      (let* ((receipt (parse-source arithmetic-parser "1 + @"))
             (diagnostics (parse-receipt-ref receipt 'diagnostics)))
        (check (parse-success? receipt) => #f)
        (check (length diagnostics) => 1)
        (check (cdr (assq 'schema (car diagnostics)))
               => "gerbil-parser.diagnostic.v1")
        (check (cdr (assq 'code (car diagnostics)))
               => "GERBIL-PARSER-EXPRESSION")
        (check (parse-roundtrip receipt) => "1 + @")))))

(run-tests! parser-runtime-tests)

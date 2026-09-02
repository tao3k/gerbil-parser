#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/src/runtime/parser
        :gerbil-parser/src/runtime/cst
        :gerbil-parser/src/runtime/token
        :gerbil-parser/src/reference/arithmetic)

(def parser-runtime-tests
  (test-suite "parser runtime"
    (test-case "source is lossless and precedence is accepted"
      (let (receipt (parse-source arithmetic-parser " 1 + 2 * value "))
        (check (parse-success? receipt) => #t)
        (check (parse-roundtrip receipt) => " 1 + 2 * value ")
        (let* ((root (parse-receipt-ref receipt 'tree))
               (expression (cdar (syntax-node-fields root))))
          (check (syntax-node-kind root) => 'SourceFile)
          (check (caar (syntax-node-fields root)) => 'expression)
          (check (syntax-node-kind expression) => 'BinaryExpression)
          (check (map car (syntax-node-fields expression))
                 => '(left operator right)))))
    (test-case "generated prefix dispatch and grouping"
      (let (receipt (parse-source arithmetic-parser "-(1 + 2)"))
        (check (parse-success? receipt) => #t)
        (check (parse-roundtrip receipt) => "-(1 + 2)")))
    (test-case "generated lexer preserves Unicode and UTF-8 byte spans"
      (let* ((english-name (string #\n #\a #\x00ef #\v #\e))
             (source (string-append english-name " + 1"))
             (receipt (parse-source arithmetic-parser source))
             (tokens (parse-receipt-ref receipt 'tokens))
             (identifier-token (car tokens)))
        (check (parse-success? receipt) => #t)
        (check (parse-roundtrip receipt) => source)
        (check (map token-kind tokens)
               => '(identifier whitespace punctuation whitespace number))
        (check (token-start identifier-token) => 0)
        (check (token-end identifier-token) => 6)
        (check (token-start (caddr tokens)) => 7)
        (check (token-end (car (reverse tokens))) => 10)))
    (test-case "generated literal dispatch uses deterministic longest match"
      (let* ((receipt (parse-source arithmetic-parser "1 ** 2"))
             (tokens (parse-receipt-ref receipt 'tokens))
             (diagnostics (parse-receipt-ref receipt 'diagnostics)))
        (check (map token-lexeme tokens) => '("1" " " "**" " " "2"))
        (check (token-kind (caddr tokens)) => 'punctuation)
        (check (token-start (caddr tokens)) => 2)
        (check (token-end (caddr tokens)) => 4)
        (check (length diagnostics) => 1)))
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

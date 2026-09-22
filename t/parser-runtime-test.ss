#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/cst
        :gerbil-parser/src/runtime/parser
        :gerbil-parser/src/runtime/scan
        :gerbil-parser/src/runtime/token
        :gerbil-parser/languages/arithmetic/v1/parser)

(def (artifact-token-events artifact)
  (let loop ((rest (parse-artifact-events artifact)) (found '()))
    (cond
     ((null? rest) (reverse found))
     ((token-event? (car rest))
      (loop (cdr rest) (cons (car rest) found)))
     (else (loop (cdr rest) found)))))

(def (find-field children name)
  (let loop ((rest children))
    (cond
     ((null? rest) #f)
     ((and (syntax-field? (car rest))
           (eq? (syntax-field-name (car rest)) name))
      (car rest))
     (else (loop (cdr rest))))))

(def (node-field-value node name)
  (let (field (find-field (syntax-node-children node) name))
    (and field (car (syntax-field-children field)))))

;; diagnostic-ref
;; : (forall (a) (-> [(Pair Symbol a)] Symbol a))
;; : (-> Diagnostic Symbol Datum)
(def (diagnostic-ref diagnostic key)
  (alet (entry (assq key diagnostic))
    (cdr entry)))

(def parser-runtime-tests
  (test-suite "parser runtime"
    (test-case "ParseArtifact is lossless and CST is an event projection"
      (let* ((source " 1 + (2 * value) ")
             (artifact (parse-source arithmetic-parser source))
             (events (parse-artifact-events artifact))
             (token-events (artifact-token-events artifact))
             (root (parse-artifact->cst artifact))
             (root-children (syntax-node-children root))
             (expression-field (find-field root-children 'expression))
             (expression (car (syntax-field-children expression-field)))
             (right-field
              (find-field (syntax-node-children expression) 'right)))
        (check (parse-artifact-ref artifact 'schema)
               => +parse-artifact-schema+)
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check (parse-artifact-ref artifact 'sourceByteLength) => 17)
        (check (map token-event-lexeme token-events)
               => '(" " "1" " " "+" " " "(" "2" " " "*" " "
                    "value" ")" " "))
        (check (map token-event-id token-events)
               => '(0 1 2 3 4 5 6 7 8 9 10 11 12))
        (check (event-kind (car events)) => 'start-node)
        (check (event-kind (car (reverse events))) => 'finish-node)
        (check (syntax-node-kind root) => 'SourceFile)
        (check (syntax-node-start root) => 0)
        (check (syntax-node-end root) => 17)
        (check (token? (car root-children)) => #t)
        (check (syntax-field-name expression-field) => 'expression)
        (check (syntax-node-kind expression) => 'Expression)
        (check (syntax-field? right-field) => #t)))
    (test-case "generated prefix dispatch and grouping remain lossless"
      (let* ((source "-(1 + 2)")
             (artifact (parse-source arithmetic-parser source)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check (map token-event-lexeme (artifact-token-events artifact))
               => '("-" "(" "1" " " "+" " " "2" ")"))))
    (test-case "LR precedence and left associativity determine CST shape"
      (let* ((precedence-root
              (parse-artifact->cst
               (parse-source arithmetic-parser "1 + 2 * 3")))
             (precedence-expression
              (node-field-value precedence-root 'expression))
             (precedence-right
              (node-field-value precedence-expression 'right))
             (associative-root
              (parse-artifact->cst
               (parse-source arithmetic-parser "1 - 2 - 3")))
             (associative-expression
              (node-field-value associative-root 'expression))
             (associative-left
              (node-field-value associative-expression 'left)))
        (check (syntax-node-kind precedence-expression) => 'Expression)
        (check (token-lexeme
                (node-field-value precedence-expression 'operator))
               => "+")
        (check (syntax-node-kind precedence-right) => 'Expression)
        (check (token-lexeme (node-field-value precedence-right 'operator))
               => "*")
        (check (syntax-node-kind associative-expression) => 'Expression)
        (check (token-lexeme
                (node-field-value associative-expression 'operator))
               => "-")
        (check (syntax-node-kind associative-left) => 'Expression)
        (check (token-lexeme (node-field-value associative-left 'operator))
               => "-")))
    (test-case "Unicode English identifiers use UTF-8 byte spans"
      (let* ((english-name (string #\n #\a #\x00ef #\v #\e))
             (source (string-append english-name " + 1"))
             (artifact (parse-source arithmetic-parser source))
             (tokens (artifact-token-events artifact))
             (identifier-event (car tokens)))
        (check (parse-artifact-success? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check (map token-event-token-kind tokens)
               => '(identifier whitespace punctuation whitespace number))
        (check (event-start identifier-event) => 0)
        (check (event-end identifier-event) => 6)
        (check (event-start (caddr tokens)) => 7)
        (check (event-end (car (reverse tokens))) => 10)))
    (test-case "generated literal dispatch uses deterministic longest match"
      (let* ((artifact (parse-source arithmetic-parser "1 ** 2"))
             (tokens (artifact-token-events artifact))
             (diagnostics (parse-artifact-ref artifact 'diagnostics)))
        (check (map token-event-lexeme tokens) => '("1" " " "**" " " "2"))
        (check (token-event-token-kind (caddr tokens)) => 'punctuation)
        (check (event-start (caddr tokens)) => 2)
        (check (event-end (caddr tokens)) => 4)
        (check (parse-artifact-success? artifact) => #f)
        (check (parse-artifact-valid? artifact) => #t)
        (check (length diagnostics) => 1)))
    (test-case "quoted scanners admit doubled ISO delimiters losslessly"
      (check (scan-quoted-string "`a``b`" 0 "`") => 6)
      (check (scan-quoted-string "'Ada''s graph'" 0 "'") => 14)
      (check (scan-quoted-string "`unterminated" 0 "`") => #f))
    (test-case "profiled numeric scanners preserve declared radix boundaries"
      (let ((prefixes '("0x" "0o" "0b"))
            (suffixes '("M" "F" "D")))
        (check (scan-number-literal/profile
                "0xCA_FE" 0 prefixes "_" suffixes #t #t)
               => 7)
        (check (scan-number-literal/profile
                "0o7_55" 0 prefixes "_" suffixes #t #t)
               => 6)
        (check (scan-number-literal/profile
                "0b10_01" 0 prefixes "_" suffixes #t #t)
               => 7)
        (check (scan-number-literal/profile
                "1_000.5E+2F" 0 prefixes "_" suffixes #t #t)
               => 11)
        (check (scan-number-literal/profile
                ".5M" 0 prefixes "_" suffixes #t #t)
               => 3)
        (check (scan-number-literal/profile
                "0x" 0 prefixes "_" suffixes #t #t)
               => 1)
        (check (scan-number-literal/profile
                "1." 0 prefixes "_" suffixes #t #f)
               => 1)))
    (test-case "failure emits one typed terminal and no partial CST"
      (let* ((source "1 + @")
             (artifact (parse-source arithmetic-parser source))
             (events (parse-artifact-events artifact))
             (diagnostics (parse-artifact-ref artifact 'diagnostics)))
        (check (parse-artifact-success? artifact) => #f)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-roundtrip artifact) => source)
        (check (map event-kind events) => '(token token token token token))
        (check (length diagnostics) => 1)
        (check (diagnostic-ref (car diagnostics) 'schema)
               => +diagnostic-schema+)
        (check (diagnostic-ref (car diagnostics) 'code)
               => "GERBIL-PARSER-EXPRESSION")
        (check (diagnostic-ref (car diagnostics) 'reasonKind)
               => 'parse-rejected)
        (check (diagnostic-ref (car diagnostics) 'failureKind)
               => 'lr-no-action)
        (check (diagnostic-ref (car diagnostics) 'byteOffset) => 4)
        (check (diagnostic-ref (car diagnostics) 'tokenKind) => 'unknown)
        (check (diagnostic-ref (car diagnostics) 'tokenLexeme) => "@")
        (check (pair? (diagnostic-ref
                       (car diagnostics) 'expectedTerminals))
               => #t)
        (check-exception (parse-artifact->cst artifact) true)))))

(export parser-runtime-tests)

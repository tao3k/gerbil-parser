#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-

(import :std/test
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        :gerbil-parser/src/runtime/artifact
        :gerbil-parser/src/runtime/cst
        :gerbil-parser/src/runtime/parser
        :gerbil-parser/src/runtime/scan
        (only-in :gerbil-parser/src/runtime/lexer lex-source)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-prepare lr-parse/prepared/receipt)
        (only-in :gerbil-parser/src/runtime/significant
                 parser-significant-tokens parser-significant-joined)
        :gerbil-parser/src/runtime/token
        :gerbil-parser/languages/arithmetic/parser)

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

(deflanguage-parser-tests parser-runtime-tests "parser runtime"
  (engine)
    (scenario "fork projection preserves significant token order"
      (sequential ((tokens (lex-source arithmetic-parser "1 + 2"))
             (prefix-reversed (list (caddr tokens) (cadr tokens)
                                    (car tokens)))
             (suffix (cdddr tokens)))
        (scope-values (((significant rest)
                      (parser-significant-joined
                       arithmetic-parser prefix-reversed suffix)))
          (equal significant
                  (parser-significant-tokens arithmetic-parser tokens))
          (equal rest
                  (parser-significant-tokens arithmetic-parser suffix)))))
    (property "ParseArtifact is lossless and CST is an event projection"
      (bindings (source " 1 + (2 * value) ") (artifact (parse-source arithmetic-parser source)) (events (parse-artifact-events artifact)) (token-events (artifact-token-events artifact)) (root (parse-artifact->cst artifact)) (root-children (syntax-node-children root)) (expression-field (find-field root-children 'expression)) (expression (car (syntax-field-children expression-field))) (right-field
              (find-field (syntax-node-children expression) 'right)))
      (equal (parse-artifact-ref artifact 'schema)
                +parse-artifact-schema+)
      (equal (parse-artifact-success? artifact)  #t)
      (equal (parse-artifact-valid? artifact)  #t)
      (equal (parse-artifact-roundtrip artifact)  source)
      (equal (parse-artifact-ref artifact 'sourceByteLength)  17)
      (equal (map token-event-lexeme token-events)
                '(" " "1" " " "+" " " "(" "2" " " "*" " "
                    "value" ")" " "))
      (equal (map token-event-id token-events)
                '(0 1 2 3 4 5 6 7 8 9 10 11 12))
      (equal (event-kind (car events))  'start-node)
      (equal (event-kind (car (reverse events)))  'finish-node)
      (equal (syntax-node-kind root)  'SourceFile)
      (equal (syntax-node-start root)  0)
      (equal (syntax-node-end root)  17)
      (equal (token? (car root-children))  #t)
      (equal (syntax-field-name expression-field)  'expression)
      (equal (syntax-node-kind expression)  'Expression)
      (equal (syntax-field? right-field)  #t))
    (property "generated prefix dispatch and grouping remain lossless"
      (bindings (source "-(1 + 2)") (artifact (parse-source arithmetic-parser source)))
      (equal (parse-artifact-success? artifact)  #t)
      (equal (parse-artifact-roundtrip artifact)  source)
      (equal (map token-event-lexeme (artifact-token-events artifact))
                '("-" "(" "1" " " "+" " " "2" ")")))
    (property "LR precedence and left associativity determine CST shape"
      (bindings (precedence-root
              (parse-artifact->cst
               (parse-source arithmetic-parser "1 + 2 * 3"))) (precedence-expression
              (node-field-value precedence-root 'expression)) (precedence-right
              (node-field-value precedence-expression 'right)) (associative-root
              (parse-artifact->cst
               (parse-source arithmetic-parser "1 - 2 - 3"))) (associative-expression
              (node-field-value associative-root 'expression)) (associative-left
              (node-field-value associative-expression 'left)))
      (equal (syntax-node-kind precedence-expression)  'Expression)
      (equal (token-lexeme
                (node-field-value precedence-expression 'operator))
                "+")
      (equal (syntax-node-kind precedence-right)  'Expression)
      (equal (token-lexeme (node-field-value precedence-right 'operator))
                "*")
      (equal (syntax-node-kind associative-expression)  'Expression)
      (equal (token-lexeme
                (node-field-value associative-expression 'operator))
                "-")
      (equal (syntax-node-kind associative-left)  'Expression)
      (equal (token-lexeme (node-field-value associative-left 'operator))
                "-"))
    (property "Unicode English identifiers use UTF-8 byte spans"
      (bindings (english-name (string #\n #\a #\x00ef #\v #\e)) (source (string-append english-name " + 1")) (artifact (parse-source arithmetic-parser source)) (tokens (artifact-token-events artifact)) (identifier-event (car tokens)))
      (equal (parse-artifact-success? artifact)  #t)
      (equal (parse-artifact-roundtrip artifact)  source)
      (equal (map token-event-token-kind tokens)
                '(identifier whitespace punctuation whitespace number))
      (equal (event-start identifier-event)  0)
      (equal (event-end identifier-event)  6)
      (equal (event-start (caddr tokens))  7)
      (equal (event-end (car (reverse tokens)))  10))
    (property "generated literal dispatch uses deterministic longest match"
      (bindings (artifact (parse-source arithmetic-parser "1 ** 2")) (tokens (artifact-token-events artifact)) (diagnostics (parse-artifact-ref artifact 'diagnostics)))
      (equal (map token-event-lexeme tokens)  '("1" " " "**" " " "2"))
      (equal (token-event-token-kind (caddr tokens))  'punctuation)
      (equal (event-start (caddr tokens))  2)
      (equal (event-end (caddr tokens))  4)
      (equal (parse-artifact-success? artifact)  #f)
      (equal (parse-artifact-valid? artifact)  #t)
      (equal (length diagnostics)  1))
    (property "quoted scanners admit doubled ISO delimiters losslessly"
      (bindings)
      (equal (scan-quoted-string "`a``b`" 0 "`")  6)
      (equal (scan-quoted-string "'Ada''s graph'" 0 "'")  14)
      (equal (scan-quoted-string "`unterminated" 0 "`")  #f))
    (scenario "profiled numeric scanners preserve declared radix boundaries"
      (scope ((prefixes '("0x" "0o" "0b"))
            (suffixes '("M" "F" "D")))
        (equal (scan-number-literal/profile
                "0xCA_FE" 0 prefixes "_" suffixes #t #t)
                7)
        (equal (scan-number-literal/profile
                "0o7_55" 0 prefixes "_" suffixes #t #t)
                6)
        (equal (scan-number-literal/profile
                "0b10_01" 0 prefixes "_" suffixes #t #t)
                7)
        (equal (scan-number-literal/profile
                "1_000.5E+2F" 0 prefixes "_" suffixes #t #t)
                11)
        (equal (scan-number-literal/profile
                ".5M" 0 prefixes "_" suffixes #t #t)
                3)
        (equal (scan-number-literal/profile
                "0x" 0 prefixes "_" suffixes #t #t)
                1)
        (equal (scan-number-literal/profile
                "1." 0 prefixes "_" suffixes #t #f)
                1)))
    (property "many distinct GLR completions remain ambiguous"
      (bindings (names
              (map (lambda (n)
                     (string->symbol
                      (string-append "path" (number->string n))))
                   (iota 12))) (rules
              (cons
               (list 'source-file
                     (cons 'choice
                           (map (lambda (name) (list 'reference name))
                                names)))
               (map (lambda (name)
                      (list name
                            (list 'alias name
                                  (list 'field 'value
                                        (list 'token 'identifier)))))
                    names))) (runtime
              (lr-prepare
               (compile-lr-spec rules 'source-file 'selective-glr))) (failure
              (with-catch
               (lambda (condition)
                 (let (irritants (error-irritants condition))
                   (and (pair? irritants) (car irritants))))
               (lambda ()
                 (call-with-values
                  (lambda ()
                    (lr-parse/prepared/receipt
                     runtime (list (make-token 'identifier "x" 0 1))))
                  (lambda _ #f))))))
      (equal (diagnostic-ref failure 'failureKind)
                'selective-glr-ambiguity)
      (equal (diagnostic-ref failure 'distinctCompletions)  12))
    (scenario "failure emits one typed terminal and no partial CST"
      (sequential ((source "1 + @")
             (artifact (parse-source arithmetic-parser source))
             (events (parse-artifact-events artifact))
             (diagnostics (parse-artifact-ref artifact 'diagnostics)))
        (equal (parse-artifact-success? artifact)  #f)
        (equal (parse-artifact-valid? artifact)  #t)
        (equal (parse-artifact-roundtrip artifact)  source)
        (equal (map event-kind events)  '(token token token token token))
        (equal (length diagnostics)  1)
        (equal (diagnostic-ref (car diagnostics) 'schema)
                +diagnostic-schema+)
        (equal (diagnostic-ref (car diagnostics) 'code)
                "GERBIL-PARSER-EXPRESSION")
        (equal (diagnostic-ref (car diagnostics) 'reasonKind)
                'parse-rejected)
        (equal (diagnostic-ref (car diagnostics) 'failureKind)
                'lr-no-action)
        (equal (diagnostic-ref (car diagnostics) 'byteOffset)  4)
        (equal (diagnostic-ref (car diagnostics) 'tokenKind)  'unknown)
        (equal (diagnostic-ref (car diagnostics) 'tokenLexeme)  "@")
        (equal (pair? (diagnostic-ref
                       (car diagnostics) 'expectedTerminals))
                #t)
        (raises (parse-artifact->cst artifact) true))))

(def parser-runtime-test parser-runtime-tests)

(export  parser-runtime-test)

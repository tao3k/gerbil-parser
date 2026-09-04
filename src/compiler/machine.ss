;;; -*- Gerbil -*-
;;; Hygienic LexicalExpr expansion and deterministic LALR(1) machine binding.

(import ./lr
        ../runtime/identity
        ../runtime/scan
        ../runtime/token)
(export defgeneral-parser-machine
        parser-machine?
        parser-machine-ir
        parser-machine-grammar-digest
        parser-machine-lex
        parser-machine-trivia
        parser-machine-parse)

(defstruct parser-machine (ir grammar-digest lex trivia parse)
  transparent: #t)

(defrules lexical-end
  (whitespace+ horizontal-whitespace+ newline+ decimal-digit+ number identifier
   heredoc
   quoted-string line-comment block-comment nested-block-comment
   choice literals fallback)
  ((_ source offset (whitespace+))
   (scan-whitespace source offset))
  ((_ source offset (horizontal-whitespace+))
   (scan-horizontal-whitespace source offset))
  ((_ source offset (newline+))
   (scan-newline source offset))
  ((_ source offset (decimal-digit+))
   (scan-decimal-digits source offset))
  ((_ source offset (number))
   (scan-number-literal source offset))
  ((_ source offset (identifier))
   (scan-identifier source offset))
  ((_ source offset (quoted-string delimiter ...))
   (scan-quoted-strings source offset (list delimiter ...)))
  ((_ source offset (heredoc))
   (scan-heredoc source offset))
  ((_ source offset (line-comment start ...))
   (scan-line-comment source offset (list start ...)))
  ((_ source offset (block-comment opening closing))
   (scan-block-comment source offset opening closing))
  ((_ source offset (nested-block-comment opening closing))
   (scan-nested-block-comment source offset opening closing))
  ((_ source offset (choice expression ...))
   (lexical-choice source offset (expression ...)))
  ((_ source offset (literals value ...))
   (let (matched (scan-longest-literal source offset (list value ...)))
     (and matched (+ offset (string-length matched)))))
  ((_ source offset (fallback))
   (+ offset 1)))

(defrules lexical-choice
  ()
  ((_ source offset ()) #f)
  ((_ source offset (expression rest ...))
   (or (lexical-end source offset expression)
       (lexical-choice source offset (rest ...)))))

(defrules lexical-dispatch
  ()
  ((_ source offset ()) #f)
  ((_ source offset ((name expression) row ...))
   (let (end (lexical-end source offset expression))
     (if end
       (cons 'name end)
       (lexical-dispatch source offset (row ...))))))

(defrules generated-lexer
  (lexical-rules)
  ((_ (lexical-rules row ...))
   (lambda (source)
     (let (length (string-length source))
       (let loop ((offset 0) (byte-offset 0) (tokens '()))
         (if (= offset length)
           (reverse tokens)
           (let (match (lexical-dispatch source offset (row ...)))
             (unless match
               (error "no lexical rule matched source" offset))
             (let (output-token
                   (scan-emit source (car match) offset (cdr match)
                              byte-offset))
               (loop (cdr match) (token-end output-token)
                     (cons output-token tokens))))))))))

(defrules defgeneral-parser-machine
  (lexical-rules rules extras parser-entrypoints)
  ((_ binding parser-ir
      (lexical-rules lexical-row ...)
      (rules (rule-name rule-expression) ...)
      (extras extra-name ...)
      (parser-entrypoints
       (root-rule root-action root-effect) entry-row ...))
   (def binding
     (make-parser-machine
      parser-ir
      (sha256-text
       (call-with-output-string
        (lambda (port) (write parser-ir port))))
      (generated-lexer (lexical-rules lexical-row ...))
      (lambda (input-token)
        (memq (token-kind input-token) '(extra-name ...)))
      (lambda (tokens)
        (lr-parse (cdr (assq 'lr-spec parser-ir)) tokens))))))

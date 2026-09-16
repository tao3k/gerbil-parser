;;; -*- Gerbil -*-
;;; Hygienic LexicalExpr expansion and deterministic LALR(1) machine binding.

(import (only-in :std/misc/vector vector-map/index)
        (only-in :std/srfi/1 any filter-map fold member)
        (only-in ../runtime/lr-parser
                 lr-lexical-mode-id lr-lexical-mode-terminals
                 lr-prepare lr-parse/prepared
                 lr-runtime-lexical-mode-catalog)
        (only-in ../runtime/identity sha256-text)
        (only-in ../runtime/scan
                 scan-block-comment scan-decimal-digits scan-heredoc
                 scan-horizontal-whitespace scan-identifier scan-line-comment
                 make-literal-end-scanner scan-longest-literal
                 scan-nested-block-comment scan-newline
                 scan-number-literal scan-number-literal/profile
                 scan-quoted-strings scan-whitespace
                 scan-emit)
        (only-in ../runtime/token token-end token-kind))
(export defgeneral-parser-machine
        lexical-end
        lexical-choice
        lexical-dispatch
        lexical-dispatch/ranked
        parser-machine?
        parser-machine-ir
        parser-machine-grammar-digest
        parser-machine-lex
        parser-machine-trivia
        parser-machine-runtime
        parser-machine-parse)

;; parser-machine
;;   : ParserMachine
;;   | doc m%
;;       Holds the immutable AOT IR and its lexer/parser entry procedures.
;;
;;       # Examples
;;
;;       ```scheme
;;       (parser-machine? generated-machine)
;;       ;; => #t for a generated parser machine
;;       ```
;;     %
(defstruct parser-machine (ir grammar-digest lex trivia runtime parse)
  transparent: #t)

;;; Expands one closed lexical algebra case into its ordinary scanner call.
;;; The templates preserve source/offset bindings; runtime behavior stays in scan.ss.
;; lexical-end
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       `lexical-end` expands a declared lexical expression.
;;
;;       # Examples
;;
;;       ```scheme
;;       (lexical-end source offset (identifier))
;;       ;; => (scan-identifier source offset)
;;       ```
;;     %
(defrules lexical-end
  (whitespace+ horizontal-whitespace+ newline+ decimal-digit+ number identifier
   heredoc number-literal
   quoted-string line-comment block-comment nested-block-comment
   choice literals fallback precedence external)
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
  ((_ source offset
      (number-literal (prefix ...) separator (suffix ...)
                      leading-period? trailing-period?))
   (scan-number-literal/profile
    source offset (list prefix ...) separator (list suffix ...)
    leading-period? trailing-period?))
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
  ((_ source offset (precedence _rank expression))
   (lexical-end source offset expression))
  ((_ source offset (external _version scanner))
   (scanner source offset))
  ((_ source offset (literals value ...))
   (let (matched (scan-longest-literal source offset (list value ...)))
     (and matched (+ offset (string-length matched)))))
  ((_ source offset (fallback))
   (+ offset 1)))

;; : (-> (OrFalse Fixnum) (OrFalse Fixnum) (OrFalse Fixnum))
(def (prefer-longest-end current candidate)
  (cond
   ((not current) candidate)
   ((not candidate) current)
   ((>= current candidate) current)
   (else candidate)))

;;; Chooses the longest lexical alternative; declaration order breaks ties.
;; lexical-choice
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       `lexical-choice` expands an ordered lexical alternative.
;;
;;       # Examples
;;
;;       ```scheme
;;       (lexical-choice source offset ((identifier) (fallback)))
;;       ;; => first matching end offset
;;       ```
;;     %
(defrules lexical-choice
  ()
  ((_ source offset ()) #f)
  ((_ source offset (expression rest ...))
   (prefer-longest-end
    (lexical-end source offset expression)
    (lexical-choice source offset (rest ...)))))

;;; Projects optional lexical precedence without inspecting scanner results.
;; lexical-expression-rank
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       Expands the precedence rank carried by one lexical expression.
;;
;;       # Examples
;;
;;       ```scheme
;;       (lexical-expression-rank (precedence 10 (identifier)))
;;       ;; => 10
;;       ```
;;     %
(defrules lexical-expression-rank (precedence)
  ((_ (precedence rank _expression)) rank)
  ((_ _expression) 0))

;;; Materializes one scanner per declared lexical rule. Static literal
;;; catalogs compile to a trie here, while the parser machine is initialized,
;;; instead of linearly probing every literal for every source token.
(defrules lexical-scanner (literals precedence)
  ((_ (literals value ...))
   (make-literal-end-scanner '(value ...)))
  ((_ (precedence _rank expression))
   (lexical-scanner expression))
  ((_ expression)
   (lambda (source offset)
     (lexical-end source offset expression))))

;; prefer-ranked-match
;;   : (-> (OrFalse List) (OrFalse List) (OrFalse List))
;;   | doc m%
;;       Chooses one scanner match by length, precedence, then declaration.
;;
;;       # Examples
;;
;;       ```scheme
;;       (prefer-ranked-match '(left 4 1) '(right 4 2))
;;       ;; => (right 4 2)
;;       ```
;;     %
(def (prefer-ranked-match current candidate)
  (cond
   ((not current) candidate)
   ((not candidate) current)
   ((> (cadr current) (cadr candidate)) current)
   ((< (cadr current) (cadr candidate)) candidate)
   ((>= (caddr current) (caddr candidate)) current)
   (else candidate)))

;;; Keeps large literal catalogs as immutable data instead of expanding one C
;;; branch per literal.  Capability checks run only while preparing interned LR
;;; lexical modes, so SRFI-1's maintained list search avoids code-size growth
;;; without entering the source-scanning hot path.
(def (lexical-literal-admitted? literal values case-insensitive?)
  (member literal values (if case-insensitive? string-ci=? string=?)))

;;; AOT capability predicate for literal-producing lexical algebra. Runtime
;;; mode selection never probes scanners with synthetic input: closed literal
;;; and choice forms lower to ordinary comparisons, while opaque external
;;; scanners are conservatively admitted.
(defrules lexical-expression-admits-literal?
  (choice literals precedence external)
  ((_ literal case-insensitive? (literals value ...))
   (lexical-literal-admitted? literal '(value ...) case-insensitive?))
  ((_ literal case-insensitive? (choice expression ...))
   (or (lexical-expression-admits-literal?
        literal case-insensitive? expression) ...))
  ((_ literal case-insensitive? (precedence _rank expression))
   (lexical-expression-admits-literal?
    literal case-insensitive? expression))
  ((_ _literal _case-insensitive? (external _version _scanner)) #t)
  ((_ _literal _case-insensitive? _expression) #f))

;;; A lexical mode admits complete rules, not already-produced lexemes. This
;;; preserves each admitted rule's ordinary longest-match behavior on source
;;; (for example a rule containing both "*" and "**").
(defrules lexical-rule-admitted?
  ()
  ((_ terminals name expression extras case-insensitive?)
   (or (not terminals)
       (memq 'name extras)
       (any
        (lambda (terminal)
          (and (pair? terminal)
               (eq? (car terminal) 'terminal)
               (case (cadr terminal)
                 ((token) (eq? (caddr terminal) 'name))
                 ((literal)
                  (lexical-expression-admits-literal?
                   (caddr terminal) case-insensitive? expression))
                 (else #f))))
        terminals))))

;;; One AOT-generated capability/scanner pair. Capability checks run once per
;;; interned LR mode; the scanner remains the ordinary generated lexical rule.
(defrules generated-lexical-rule
  ()
  ((_ (name expression) extras case-insensitive?)
   (let (scanner (lexical-scanner expression))
     (cons
      (lambda (terminals)
        (lexical-rule-admitted?
         terminals name expression extras case-insensitive?))
      (lambda (source offset)
        (let (end (scanner source offset))
          (and end
               (list 'name end (lexical-expression-rank expression)))))))))

;;; Returns name, end offset, and precedence for generated-lexer. Longest
;;; consumption wins globally; lexical precedence breaks equal-length ties.
;; lexical-dispatch/ranked
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       Expands global longest-match dispatch with lexical tie precedence.
;;
;;       # Examples
;;
;;       ```scheme
;;       (lexical-dispatch/ranked source offset rows)
;;       ;; => (token-name end-offset precedence) or #f
;;       ```
;;     %
(defrules lexical-dispatch/ranked
  ()
  ((_ source offset ()) #f)
  ((_ source offset ((name expression) row ...))
   (let ((end (lexical-end source offset expression))
         (rest (lexical-dispatch/ranked source offset (row ...))))
     (prefer-ranked-match
      (and end (list 'name end (lexical-expression-rank expression)))
      rest))))

;;; Binds the matched declaration name to its end offset without runtime macro state.
;; lexical-dispatch
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       `lexical-dispatch` expands the ordered token-rule dispatch.
;;
;;       # Examples
;;
;;       ```scheme
;;       (lexical-dispatch source offset ((Identifier (identifier))))
;;       ;; => (Identifier . end-offset)
;;       ```
;;     %
(defrules lexical-dispatch
  ()
  ((_ source offset ()) #f)
  ((_ source offset rows)
   (let (match (lexical-dispatch/ranked source offset rows))
     (and match (cons (car match) (cadr match))))))

;;; Generates only the source traversal shell; token construction remains scan-owned.
;;; Declaration order is observable only after length and precedence tie.
;; generated-lexer
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       `generated-lexer` expands lexical rows into a source-to-token procedure.
;;
;;       # Examples
;;
;;       ```scheme
;;       (generated-lexer (lexical-rules (Identifier (identifier))))
;;       ;; => source-to-token procedure
;;       ```
;;     %
(defrules generated-lexer
  (lexical-rules extras)
  ((_ (lexical-rules row ...) (extras extra-name ...) case-insensitive?
      mode-catalog)
   (let* ((rules
           (list (generated-lexical-rule
                  row '(extra-name ...) case-insensitive?) ...))
          (all-scanners (map cdr rules))
          (mode-scanners
           (vector-map/index
            (lambda (_index mode)
              (filter-map
               (lambda (rule)
                 (and ((car rule) (lr-lexical-mode-terminals mode))
                      (cdr rule)))
               rules))
            mode-catalog)))
     (letrec
       ((scan-scanners
         (lambda (scanners source offset)
           (fold
            (lambda (scanner selected)
              (prefer-ranked-match selected (scanner source offset)))
            #f scanners)))
        (scan-one
         (lambda (source offset byte-offset mode)
           (let (match
                 (or (if mode
                       (scan-scanners
                        (vector-ref mode-scanners
                                    (lr-lexical-mode-id mode))
                        source offset)
                       (scan-scanners all-scanners source offset))
                     ;; A mode miss must still materialize the offending token
                     ;; for the LR failure frontier and lossless diagnostics.
                     ;; Successful directed scans never enter this cold path.
                     (and mode
                          (scan-scanners all-scanners source offset))))
             (unless match
               (error "no lexical rule matched parser-directed source"
                      offset mode))
             (let (output-token
                   (scan-emit source (car match) offset (cadr match)
                              byte-offset))
               (values output-token (cadr match))))))
        (scan-from
         (lambda (source initial-offset initial-byte-offset)
           (let (length (string-length source))
             (let loop ((offset initial-offset)
                        (byte-offset initial-byte-offset)
                        (tokens '()))
               (if (= offset length)
                 (reverse tokens)
                 (let-values (((output-token end)
                               (scan-one source offset byte-offset #f)))
                   (loop end (token-end output-token)
                         (cons output-token tokens)))))))))
     (case-lambda
      ((source) (scan-from source 0 0))
      ((source offset byte-offset)
       (scan-from source offset byte-offset))
      ((source offset byte-offset mode)
       (scan-one source offset byte-offset mode)))))))

;;; Connects immutable parser IR to generated lexer and LR runtime entrypoints once.
;;; Runtime calls receive the compiled machine and never re-enter grammar expansion.
;; defgeneral-parser-machine
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       `defgeneral-parser-machine` expands a complete parser machine binding.
;;
;;       # Examples
;;
;;       ```scheme
;;       (defgeneral-parser-machine parser parser-ir ...)
;;       ;; => immutable parser-machine binding
;;       ```
;;     %
(defrules defgeneral-parser-machine
  (lexical-rules rules extras parser-entrypoints)
  ((_ binding parser-ir
      (lexical-rules lexical-row ...)
      (rules (rule-name rule-expression) ...)
      (extras extra-name ...)
      (parser-entrypoints
       (root-rule root-action root-effect) entry-row ...))
   (def binding
     (let (runtime
           (lr-prepare (cdr (assq 'lr-spec parser-ir))))
       (make-parser-machine
        parser-ir
        (sha256-text
         (call-with-output-string
          (lambda (port) (write parser-ir port))))
        (generated-lexer
         (lexical-rules lexical-row ...)
         (extras extra-name ...)
         (cdr (assq 'case-insensitive? parser-ir))
         (lr-runtime-lexical-mode-catalog runtime))
        (lambda (input-token)
          (memq (token-kind input-token) '(extra-name ...)))
        runtime
        (lambda (tokens . maybe-observability)
          (lr-parse/prepared
           runtime tokens
           (if (null? maybe-observability)
             #f
             (car maybe-observability)))))))))

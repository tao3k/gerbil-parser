;;; -*- Gerbil -*-
;;; Hygienic LexicalExpr expansion and deterministic LALR(1) machine binding.

(import (only-in :std/vector/vector vector-map/index)
        (only-in ../runtime/lr-parser
                 lr-lexical-mode-id lr-lexical-mode-terminals
                 lr-prepare lr-parse/prepared
                 lr-runtime-lexical-mode-catalog
                 install-lr-runtime-direct-step!)
        (only-in ../runtime/scan
                 scan-block-comment scan-decimal-digits scan-heredoc
                 scan-horizontal-whitespace scan-identifier scan-line scan-line-comment
                 scan-character-run
                 scan-until-delimiters
                 make-literal-end-scanner make-ranked-literal-scanner
                 make-ranked-regular-scanner
                 scan-longest-literal
                 scan-nested-block-comment scan-newline
                 scan-number-literal scan-number-literal/profile
                 scan-escaped-quoted-strings scan-quoted-strings scan-whitespace
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
        parser-machine-parse
        parser-machine-direct-drive
        parser-machine-direct-source
        install-parser-machine-direct-drive!
        install-parser-machine-direct-source!
        install-parser-machine-direct-step!)

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
(defstruct parser-machine
  (ir grammar-digest lex trivia runtime parse direct-drive direct-source)
  transparent: #t)

;;; A generated driver is admitted only for the exact Parser IR whose digest
;;; was embedded in its source. Install during language-module initialization,
;;; before the machine is shared with parser requests.
(def (install-parser-machine-direct-drive! machine digest drive)
  (unless (and (parser-machine? machine)
               (string? digest)
               (equal? digest (parser-machine-grammar-digest machine))
               (procedure? drive)
               (not (parser-machine-direct-drive machine)))
    (error "generated LR driver does not match parser machine" digest))
  (parser-machine-direct-drive-set! machine drive))

;;; Grammar-derived source parsers may admit a complete artifact directly for
;;; fresh unobserved requests. Returning #f leaves the ordinary LR path to
;;; own rejection, diagnostics, checkpoints, and unsupported source shapes.
(def (install-parser-machine-direct-source! machine digest parse)
  (unless (and (parser-machine? machine)
               (string? digest)
               (equal? digest (parser-machine-grammar-digest machine))
               (procedure? parse)
               (not (parser-machine-direct-source machine)))
    (error "generated source parser does not match parser machine" digest))
  (parser-machine-direct-source-set! machine parse))

(def (install-parser-machine-direct-step! machine digest step)
  (unless (and (parser-machine? machine)
               (string? digest)
               (equal? digest (parser-machine-grammar-digest machine))
               (procedure? step))
    (error "generated LR step does not match parser machine" digest))
  (install-lr-runtime-direct-step!
   (parser-machine-runtime machine) step))

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
  (whitespace+ horizontal-whitespace+ newline+ line decimal-digit+ number identifier
   heredoc number-literal
   quoted-string escaped-quoted-string until-delimiters
   line-comment block-comment nested-block-comment
   choice literals fallback precedence external character-run)
  ((_ source offset (whitespace+))
   (scan-whitespace source offset))
  ((_ source offset (horizontal-whitespace+))
   (scan-horizontal-whitespace source offset))
  ((_ source offset (newline+))
   (scan-newline source offset))
  ((_ source offset (line))
   (scan-line source offset))
  ((_ source offset (character-run character minimum))
   (scan-character-run source offset character minimum))
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
  ((_ source offset (escaped-quoted-string delimiter ...))
   (scan-escaped-quoted-strings source offset (list delimiter ...)))
  ((_ source offset (until-delimiters characters))
   (scan-until-delimiters source offset characters))
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

;; Only a complete literal catalog can join the shared mode trie. Choices and
;; external scanners retain their ordinary generated procedures.
(defrules lexical-static-literals (literals precedence)
  ((_ (literals value ...)) '(value ...))
  ((_ (precedence _rank expression))
   (lexical-static-literals expression))
  ((_ _expression) #f))

;; Closed run primitives join the shared mode DFA. The number rule adds
;; non-accepting fraction/exponent states; other expressions retain scanners.
(defrules lexical-regular-kind
  (whitespace+ horizontal-whitespace+ newline+ decimal-digit+
   number identifier precedence)
  ((_ (whitespace+)) 'whitespace+)
  ((_ (horizontal-whitespace+)) 'horizontal-whitespace+)
  ((_ (newline+)) 'newline+)
  ((_ (decimal-digit+)) 'decimal-digit+)
  ((_ (number)) 'number)
  ((_ (identifier)) 'identifier)
  ((_ (precedence _rank expression))
   (lexical-regular-kind expression))
  ((_ _expression) #f))

;;; The first transition of the closed regular scanners can be decided once
;;; for each ASCII character and LR lexical mode. Unknown/nonregular forms
;;; remain candidates, so this filter cannot change maximal-munch decisions.
(defrules lexical-first-character?
  (whitespace+ horizontal-whitespace+ newline+ decimal-digit+ number
   identifier number-literal quoted-string escaped-quoted-string heredoc
   line-comment block-comment
   nested-block-comment precedence choice character-run)
  ((_ ch (whitespace+)) (char-whitespace? ch))
  ((_ ch (horizontal-whitespace+))
   (or (char=? ch #\space) (char=? ch #\tab)))
  ((_ ch (newline+))
   (or (char=? ch #\newline) (char=? ch #\return)))
  ((_ ch (decimal-digit+)) (char-numeric? ch))
  ((_ ch (number)) (char-numeric? ch))
  ((_ ch (identifier))
   (or (char-alphabetic? ch) (char=? ch #\_)))
  ((_ ch (character-run character _minimum))
   (char=? ch (string-ref character 0)))
  ((_ ch (number-literal (prefix ...) _separator _suffixes
                         leading-period? _trailing-period?))
   (or (and (char>=? ch #\0) (char<=? ch #\9))
       (and leading-period? (char=? ch #\.))
       (or (zero? (string-length prefix))
           (char=? ch (string-ref prefix 0))) ...))
  ((_ ch (quoted-string delimiter ...))
   (or (or (zero? (string-length delimiter))
           (char=? ch (string-ref delimiter 0))) ...))
  ((_ ch (escaped-quoted-string delimiter ...))
   (or (or (zero? (string-length delimiter))
           (char=? ch (string-ref delimiter 0))) ...))
  ((_ ch (heredoc)) (char=? ch #\<))
  ((_ ch (line-comment prefix ...))
   (or (char=? ch (string-ref prefix 0)) ...))
  ((_ ch (block-comment opening _closing))
   (char=? ch (string-ref opening 0)))
  ((_ ch (nested-block-comment opening _closing))
   (char=? ch (string-ref opening 0)))
  ((_ ch (precedence _rank expression))
   (lexical-first-character? ch expression))
  ((_ ch (choice expression ...))
   (or (lexical-first-character? ch expression) ...))
  ((_ _ch _expression) #t))

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

(def (prefer-generated-match current candidate)
  (cond
   ((not current) candidate)
   ((not candidate) current)
   ((> (cadr current) (cadr candidate)) current)
   ((< (cadr current) (cadr candidate)) candidate)
   ((> (caddr current) (caddr candidate)) current)
   ((< (caddr current) (caddr candidate)) candidate)
   ((< (cadddr current) (cadddr candidate)) current)
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
                 ((literal layout-start layout-next)
                  (lexical-expression-admits-literal?
                   (caddr terminal) case-insensitive? expression))
                 (else #f))))
        terminals))))

;;; One AOT-generated capability/scanner pair. Capability checks run once per
;;; interned LR mode; the scanner remains the ordinary generated lexical rule.
(defrules generated-lexical-rule
  ()
  ((_ (name expression) extras case-insensitive?)
   (let* ((literals (lexical-static-literals expression))
          (regular-kind (lexical-regular-kind expression))
          (scanner (and (not literals) (not regular-kind)
                        (lexical-scanner expression))))
     (vector
      (lambda (terminals)
        (lexical-rule-admitted?
         terminals name expression extras case-insensitive?))
      (lambda (source offset)
        (let (end (and scanner (scanner source offset)))
          (and end
               (list 'name end (lexical-expression-rank expression)))))
      literals
      'name
      (lexical-expression-rank expression)
      (lambda (ch) (lexical-first-character? ch expression))
      regular-kind))))

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
   (let* ((bare-rules
           (list (generated-lexical-rule
                  row '(extra-name ...) case-insensitive?) ...))
          (rules
           (let loop ((remaining bare-rules) (ordinal 0) (found '()))
             (if (null? remaining)
               (reverse found)
               (let* ((rule (car remaining))
                      (scanner (vector-ref rule 1))
                      (ranked-scanner
                       (and scanner
                            (lambda (source offset)
                              (let (match (scanner source offset))
                                (and match
                                     (list (car match) (cadr match)
                                           (caddr match) ordinal)))))))
                 (loop (cdr remaining) (+ ordinal 1)
                       (cons
                        (vector (vector-ref rule 0) ranked-scanner
                                (vector-ref rule 2) (vector-ref rule 3)
                                (vector-ref rule 4) (vector-ref rule 5)
                                (vector-ref rule 6))
                        found))))))
          (literal-entries
           (let loop ((remaining rules) (ordinal 0) (entries '()))
             (if (null? remaining)
               entries
               (let ((rule (car remaining)))
                 (loop
                  (cdr remaining) (+ ordinal 1)
                  (if (vector-ref rule 2)
                    (fold
                     (lambda (literal found)
                       (cons
                        (list literal (vector-ref rule 3)
                              (vector-ref rule 4) ordinal)
                        found))
                     entries (vector-ref rule 2))
                    entries))))))
          (literal-ascii-starts
           (let (starts (make-vector 128 #f))
             (for-each
              (lambda (entry)
                (let* ((literal (car entry))
                       (code (and (positive? (string-length literal))
                                  (char->integer (string-ref literal 0)))))
                  (when (and code (< code 128))
                    (vector-set! starts code #t))))
              literal-entries)
             starts))
          (literal-scanner
           (and (pair? literal-entries)
                (make-ranked-literal-scanner literal-entries)))
          (prepare-scanners
           (lambda (terminals)
             (let ((admitted-literals (make-vector (length rules) #f)))
               (let loop ((remaining rules) (ordinal 0)
                          (has-literals? #f) (scanners '())
                          (regular-entries '())
                          (regular-first-predicates '()))
                 (if (null? remaining)
                   (let* ((regular-scanner
                           (and (pair? regular-entries)
                                (make-ranked-regular-scanner regular-entries)))
                          (regular-ascii-starts
                           (and regular-scanner
                                (vector-map/index
                                 (lambda (index _)
                                   (any (lambda (predicate)
                                          (predicate (integer->char index)))
                                        regular-first-predicates))
                                 (make-vector 128))))
                          (ascii-scanners
                           (vector-map/index
                            (lambda (index _)
                              (filter
                               (lambda (entry)
                                 ((cdr entry) (integer->char index)))
                               scanners))
                            (make-vector 128))))
                     (lambda (source offset)
                       (let* ((ch (string-ref source offset))
                              (code (char->integer ch))
                              (candidates
                               (if (< code 128)
                                 (vector-ref ascii-scanners code)
                                 scanners))
                              (selected
                               (fold
                                (lambda (entry selected)
                                  (prefer-generated-match
                                   selected ((car entry) source offset)))
                                #f candidates)))
                         (let (selected
                               (if (and regular-scanner
                                        (or (>= code 128)
                                            (vector-ref regular-ascii-starts
                                                        code)))
                                 (prefer-generated-match
                                  selected (regular-scanner source offset))
                                 selected))
                           (if (and literal-scanner has-literals?
                                    (or (>= code 128)
                                        (vector-ref literal-ascii-starts
                                                    code)))
                             (prefer-generated-match
                              selected
                              (literal-scanner source offset admitted-literals))
                             selected)))))
                   (let* ((rule (car remaining))
                          (admitted? ((vector-ref rule 0) terminals))
                          (literals (and admitted? (vector-ref rule 2)))
                          (regular-kind (and admitted? (vector-ref rule 6))))
                     (cond
                      ((pair? literals)
                       (vector-set! admitted-literals ordinal #t)
                       (loop
                        (cdr remaining) (+ ordinal 1) #t scanners
                        regular-entries regular-first-predicates))
                      (literals
                       (loop (cdr remaining) (+ ordinal 1)
                             has-literals? scanners regular-entries
                             regular-first-predicates))
                      (regular-kind
                       (loop (cdr remaining) (+ ordinal 1)
                             has-literals? scanners
                             (cons (list regular-kind (vector-ref rule 3)
                                         (vector-ref rule 4) ordinal)
                                   regular-entries)
                             (cons (vector-ref rule 5)
                                   regular-first-predicates)))
                      (admitted?
                       (loop
                        (cdr remaining) (+ ordinal 1) has-literals?
                        (cons (cons (vector-ref rule 1)
                                    (vector-ref rule 5))
                              scanners)
                        regular-entries regular-first-predicates))
                      (else
                       (loop (cdr remaining) (+ ordinal 1)
                             has-literals? scanners regular-entries
                             regular-first-predicates)))))))))
          (all-scanners (prepare-scanners #f))
          (mode-scanners
           (vector-map/index
            (lambda (_index mode)
              (prepare-scanners (lr-lexical-mode-terminals mode)))
            mode-catalog)))
     (letrec
       ((scan-one
         (lambda (source offset byte-offset mode)
           (let (match
                 (or (if mode
                       ((vector-ref mode-scanners
                                    (lr-lexical-mode-id mode))
                        source offset)
                       (all-scanners source offset))
                     ;; A mode miss must still materialize the offending token
                     ;; for the LR failure frontier and lossless diagnostics.
                     ;; Successful directed scans never enter this cold path.
                     (and mode
                          (all-scanners source offset))))
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
  (grammar-digest lexical-rules rules extras parser-entrypoints)
  ((_ binding parser-ir
      (grammar-digest parser-artifact-digest)
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
        parser-artifact-digest
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
             (car maybe-observability))))
        #f
        #f)))))

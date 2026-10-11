;;; -*- Gerbil -*-
;;; Closed lexical-expression expansion and ordered candidate selection.

(import (only-in ../runtime/lexical-source
                 scan-module-text scan-header-delimiter scan-header-data)
        (only-in ../grammar/lexical-algebra
                 text-profile-data)
        (only-in ../runtime/scan
                 scan-block-comment scan-decimal-digits scan-heredoc scan-horizontal-whitespace
                 scan-identifier scan-line scan-line-comment scan-character-run
                 make-text-profile-scanner scan-until-delimiters make-literal-end-scanner
                 scan-longest-literal scan-nested-block-comment scan-newline
                 scan-number-literal scan-number-literal/profile scan-escaped-quoted-strings
                 scan-quoted-strings scan-quoted-string/profile scan-whitespace))
(export lexical-end lexical-choice lexical-dispatch lexical-dispatch/ranked
        generated-lexical-rule lexical-rule-certificate-template prefer-generated-match)

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
(defrules lexical-end/primitive
  (whitespace+ horizontal-whitespace+ newline+ line decimal-digit+ number identifier
   heredoc number-literal
   quoted-string escaped-quoted-string quoted-string-profile until-delimiters header-delimiter header-data module-text
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
  ((_ source offset (quoted-string-profile delimiter escapes unicode-width))
   (scan-quoted-string/profile source offset delimiter escapes unicode-width))
  ((_ source offset (module-text border word end-border open close comment extra))
   (scan-module-text source offset '(module-text border word end-border open close comment extra)))
  ((_ source offset (header-delimiter prefix count index))
   (scan-header-delimiter source offset prefix count index))
  ((_ source offset (header-data prefix count stops))
   (scan-header-data source offset prefix count stops))
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

(defrules lexical-end (text-profile)
  ((_ source offset (text-profile expression))
   ((make-text-profile-scanner (text-profile-data expression)) source offset))
  ((_ source offset expression)
   (lexical-end/primitive source offset expression)))

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
(defrules lexical-scanner (literals precedence text-profile choice)
  ((_ (text-profile expression))
   (make-text-profile-scanner (text-profile-data expression)))
  ((_ (choice expression ...))
   (let (scanners (list (lexical-scanner expression) ...))
     (lambda (source offset)
       (foldl (lambda (scanner end)
                (prefer-longest-end end (scanner source offset)))
              #f scanners))))
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
   identifier number-literal quoted-string escaped-quoted-string quoted-string-profile heredoc
   line-comment block-comment
   nested-block-comment precedence choice character-run literals)
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
  ((_ ch (quoted-string-profile delimiter _escapes _unicode-width))
   (char=? ch (string-ref delimiter 0)))
  ((_ ch (literals value ...))
   (or (or (zero? (string-length value))
           (char-ci=? ch (string-ref value 0))) ...))
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

(defrules lexical-pure-fallback?
  (fallback precedence)
  ((_ (fallback)) #t)
  ((_ (precedence _rank expression)) (lexical-pure-fallback? expression))
  ((_ _expression) #f))

;;; This predicate is a lower bound on scanner acceptance; the first-character
;;; filter above is an upper bound. Do not conflate the two for incomplete
;;; strings, comments, numeric prefixes, or unknown/external expressions.
(defrules lexical-guaranteed-first-character?
  (whitespace+ horizontal-whitespace+ newline+ decimal-digit+ number identifier
   literals choice precedence)
  ((_ ch (whitespace+)) (lexical-first-character? ch (whitespace+)))
  ((_ ch (horizontal-whitespace+)) (lexical-first-character? ch (horizontal-whitespace+)))
  ((_ ch (newline+)) (lexical-first-character? ch (newline+)))
  ((_ ch (decimal-digit+)) (lexical-first-character? ch (decimal-digit+)))
  ((_ ch (number)) (lexical-first-character? ch (number)))
  ((_ ch (identifier)) (lexical-first-character? ch (identifier)))
  ((_ ch (literals value ...))
   (or (and (= (string-length value) 1) (char=? ch (string-ref value 0))) ...))
  ((_ ch (choice expression ...))
   (or (lexical-guaranteed-first-character? ch expression) ...))
  ((_ ch (precedence _rank expression)) (lexical-guaranteed-first-character? ch expression))
  ((_ _ch _expression) #f))

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
;;; Literal rules share construction code; their trie scanning remains unchanged.
;;; These predicates run during mode preparation, outside token traversal.
(def (make-generated-literal-rule name literals rank extras case-insensitive?)
  (vector
   (lambda (terminals)
     (or (not terminals) (memq name extras)
         (any (lambda (terminal)
                (and (pair? terminal) (eq? (car terminal) 'terminal)
                     (case (cadr terminal)
                       ((token) (eq? (caddr terminal) name))
                       ((literal layout-start layout-next)
                        (lexical-literal-admitted? (caddr terminal) literals case-insensitive?))
                       (else #f)))) terminals)))
   (lambda (_source _offset) #f)
   literals name rank
   (lambda (ch)
     (any (lambda (literal)
            (or (zero? (string-length literal))
                (char-ci=? ch (string-ref literal 0)))) literals))
   #f))

(defrules generated-lexical-rule
  (literals precedence)
  ((_ (name (literals value ...)) extras case-insensitive?)
   (make-generated-literal-rule 'name '(value ...) 0 extras case-insensitive?))
  ((_ (name (precedence rank (literals value ...))) extras case-insensitive?)
   (make-generated-literal-rule 'name '(value ...) rank extras case-insensitive?))
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

(defrules lexical-rule-certificate-template
  ()
  ((_ (name expression))
   (vector (lexical-pure-fallback? expression)
           (lambda (ch) (lexical-guaranteed-first-character? ch expression)))))

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


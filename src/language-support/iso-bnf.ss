;;; -*- Gerbil -*-
;;; ISO WG3 BNF language-source adapter and validation boundary.

(import :gerbil-parser/src/runtime/identity
        (only-in :std/srfi/13 string-trim-both)
        (only-in :gerbil-parser/src/utilities/strings
                 ascii-whitespace? string-index-from text-lines))
(export +iso-bnf-source-schema+
        +iso-bnf-rule-overlay-schema+
        iso-bnf-production?
        iso-bnf-production-name
        iso-bnf-production-expression
        iso-bnf-production-ast
        iso-bnf-production-line
        iso-bnf-production-references
        iso-bnf-source?
        iso-bnf-source-language
        iso-bnf-source-version
        iso-bnf-source-commit
        iso-bnf-source-digest
        iso-bnf-source-productions
        iso-bnf-source-production
        iso-bnf-source-grammar-rules
        iso-bnf-source-grammar-rules/overrides
        iso-bnf-apply-rule-precedences
        iso-bnf-source-syntax-kinds
        iso-bnf-source-literals
        iso-bnf-source-declaration-sources
        iso-bnf-source-declaration-sources/overrides
        iso-bnf-source-declaration-sources/precedences
        iso-bnf-choice-normalize-overlap
        parse-iso-bnf-source
        parse-iso-bnf-source/expected)

;; : String
(def +iso-bnf-source-schema+ "gerbil-parser.iso-wg3-bnf-source.v1")

;; : String
(def +iso-bnf-rule-overlay-schema+
  "gerbil-parser.iso-bnf-rule-overlay.v1")

;; : (-> String String BnfAst Nat (List String) IsoBnfProduction)
(defstruct iso-bnf-production (name expression ast line references)
  transparent: #t)
;; : (-> String String String String String (List IsoBnfProduction) IsoBnfSource)
(defstruct iso-bnf-source
  (schema language version commit digest productions)
  transparent: #t)

;; : (-> Char Boolean)
;; : (-> String (Maybe (Pair String String)))
(def (production-header line)
  (let* ((text (string-trim-both line))
         (separator (string-index-from text "::=")))
    (and separator
         (> separator 2)
         (char=? (string-ref text 0) #\<)
         (let (close (string-index-from text ">" 1))
           (and close
                (< close separator)
                (cons (substring text 1 close)
                      (string-trim-both
                       (substring text (+ separator 3)
                                  (string-length text)))))))))

;; : (-> String String String)
(def (append-expression current line)
  (let (next (string-trim-both line))
    (cond
     ((zero? (string-length next)) current)
     ((zero? (string-length current)) next)
     (else (string-append current " " next)))))

;; : (-> String Nat (List String) (List String))
(def (collect-reference-names source offset found)
  (let* ((open (string-index-from source "<" offset))
         (close (and open (string-index-from source ">" (+ open 1)))))
    (cond
     ((not close) (reverse found))
     (else
      (let (name (substring source (+ open 1) close))
        (collect-reference-names
         source (+ close 1)
         (if (or (zero? (string-length name)) (member name found))
           found
           (cons name found))))))))

;; : (-> String (List String))
(def (reference-names expression)
  (let* ((annotation (string-index-from expression "!!"))
         (source (if annotation (substring expression 0 annotation) expression)))
    (collect-reference-names source 0 '())))

;; : (forall (a) (-> (U Symbol (Pair Symbol a)) Symbol))
;; : (-> BnfToken Symbol)
(def (bnf-token-kind token)
  (if (pair? token) (car token) token))

;; : (-> Char Boolean)
(def (bnf-delimiter? ch)
  (or (ascii-whitespace? ch)
      (memv ch '(#\[ #\] #\{ #\} #\|))))

;; : (-> String (List BnfToken))
(def (tokenize-bnf-expression expression)
  (let (length (string-length expression))
    (let loop ((offset 0) (tokens '()))
      (cond
       ((= offset length) (reverse tokens))
       ((ascii-whitespace? (string-ref expression offset))
        (loop (+ offset 1) tokens))
       ((and (<= (+ offset 3) length)
             (string=? (substring expression offset (+ offset 3)) "..."))
        (loop (+ offset 3) (cons 'ellipsis tokens)))
       ((char=? (string-ref expression offset) #\<)
        (let (close (string-index-from expression ">" (+ offset 1)))
          (if (and close (> close (+ offset 1)))
            (loop (+ close 1)
                  (cons (cons 'reference
                              (substring expression (+ offset 1) close))
                        tokens))
            (loop (+ offset 1) (cons (cons 'terminal "<") tokens)))))
       ((char=? (string-ref expression offset) #\[)
        (loop (+ offset 1) (cons 'open-square tokens)))
       ((char=? (string-ref expression offset) #\])
        (loop (+ offset 1) (cons 'close-square tokens)))
       ((char=? (string-ref expression offset) #\{)
        (loop (+ offset 1) (cons 'open-brace tokens)))
       ((char=? (string-ref expression offset) #\})
        (loop (+ offset 1) (cons 'close-brace tokens)))
       ((char=? (string-ref expression offset) #\|)
        (loop (+ offset 1) (cons 'bar tokens)))
       (else
        (let end ((next (+ offset 1)))
          (if (or (= next length)
                  (bnf-delimiter? (string-ref expression next))
                  (and (<= (+ next 3) length)
                       (string=? (substring expression next (+ next 3)) "...")))
            (loop next
                  (cons (cons 'terminal (substring expression offset next))
                        tokens))
            (end (+ next 1)))))))))

;; : (-> (List BnfAst) BnfAst)
(def (finish-sequence reversed)
  (let (items (reverse reversed))
    (cond
     ((null? items) '(empty))
     ((null? (cdr items)) (car items))
     (else (cons 'sequence items)))))

;; : (-> (List BnfAst) BnfAst)
(def (finish-choice reversed)
  (let (items (reverse reversed))
    (if (null? (cdr items)) (car items) (cons 'choice items))))

;; : (-> (List BnfToken) (Values BnfAst (List BnfToken)))
(def (parse-bnf-atom tokens)
  (unless (pair? tokens)
    (error "ISO BNF expression expected an atom"))
  (let (token (car tokens))
    (case (bnf-token-kind token)
      ((reference terminal)
       (values (list (bnf-token-kind token) (cdr token)) (cdr tokens)))
      ((open-square)
       (let-values (((expression rest)
                     (parse-bnf-choice (cdr tokens) 'close-square)))
         (unless (and (pair? rest) (eq? (car rest) 'close-square))
           (error "unclosed ISO BNF optional expression"))
         (values (list 'optional expression) (cdr rest))))
      ((open-brace)
       (let-values (((expression rest)
                     (parse-bnf-choice (cdr tokens) 'close-brace)))
         (unless (and (pair? rest) (eq? (car rest) 'close-brace))
           (error "unclosed ISO BNF grouped expression"))
         (values (list 'group expression) (cdr rest))))
      (else (error "unexpected ISO BNF expression token" token)))))

;; : (-> (List BnfToken) (Maybe Symbol) (List BnfAst) (Values BnfAst (List BnfToken)))
(def (parse-bnf-sequence-from rest stop items)
  (if (or (null? rest)
          (eq? (car rest) 'bar)
          (and stop (eq? (car rest) stop)))
    (values (finish-sequence items) rest)
    (let-values (((atom next) (parse-bnf-atom rest)))
      (if (and (pair? next) (eq? (car next) 'ellipsis))
        (parse-bnf-sequence-from
         (cdr next) stop (cons (list 'repeat1 atom) items))
        (parse-bnf-sequence-from next stop (cons atom items))))))

;; : (-> (List BnfToken) (Maybe Symbol) (Values BnfAst (List BnfToken)))
(def (parse-bnf-sequence tokens stop)
  (parse-bnf-sequence-from tokens stop '()))

;; : (-> (List BnfToken) (Maybe Symbol) (List BnfAst) (Values BnfAst (List BnfToken)))
(def (parse-bnf-choice-from tokens stop alternatives)
  (let-values (((sequence next) (parse-bnf-sequence tokens stop)))
    (if (and (pair? next) (eq? (car next) 'bar))
      (parse-bnf-choice-from (cdr next) stop (cons sequence alternatives))
      (values (finish-choice (cons sequence alternatives)) next))))

;; : (-> (List BnfToken) (Maybe Symbol) (Values BnfAst (List BnfToken)))
(def (parse-bnf-choice tokens stop)
  (parse-bnf-choice-from tokens stop '()))

;; : (-> String BnfAst)
(def (parse-bnf-expression expression)
  (let (annotation (string-index-from expression "!!"))
    (if annotation
      (let ((terminal (string-trim-both (substring expression 0 annotation)))
            (codepoints
             (string-trim-both
              (substring expression (+ annotation 2)
                         (string-length expression)))))
        (when (zero? (string-length terminal))
          (error "ISO BNF annotated terminal has no spelling" expression))
        (list 'annotated-terminal terminal codepoints))
      (let-values (((ast rest)
                    (parse-bnf-choice (tokenize-bnf-expression expression) #f)))
        (unless (null? rest)
          (error "unexpected trailing ISO BNF expression tokens" rest))
        ast))))

;; : (-> String String Nat IsoBnfProduction)
(def (make-production-row name expression start-line)
  (make-iso-bnf-production
   name expression (parse-bnf-expression expression)
   start-line (reference-names expression)))

;; : (-> (Maybe String) String (Maybe Nat) (List IsoBnfProduction) (List IsoBnfProduction))
(def (adjoin-production name expression start-line productions)
  (if name
    (cons (make-production-row name expression start-line) productions)
    productions))

;; : (-> (Maybe String) String Boolean)
(def (production-continuation? name line)
  (let (text (string-trim-both line))
    (and name
         (or (zero? (string-length text))
             (not (char=? (string-ref text 0) #\#))))))

;; : (-> (List String) Nat (Maybe String) (Maybe Nat) String (List IsoBnfProduction) (List IsoBnfProduction))
(def (parse-production-lines
      lines line-number name start-line expression productions)
  (cond
   ((null? lines)
    (reverse (adjoin-production name expression start-line productions)))
   (else
    (let* ((line (car lines))
           (header (production-header line)))
      (cond
       (header
        (parse-production-lines
         (cdr lines) (+ line-number 1)
         (car header) line-number (cdr header)
         (adjoin-production name expression start-line productions)))
       (else
        (parse-production-lines
         (cdr lines) (+ line-number 1) name start-line
         (if (production-continuation? name line)
           (append-expression expression line)
           expression)
         productions)))))))

;; : (-> String (List IsoBnfProduction))
(def (parse-production-rows source)
  (parse-production-lines (text-lines source) 1 #f #f "" '()))

;; : (-> (List IsoBnfProduction) (List IsoBnfProduction))
(def (validate-productions productions)
  (let (names (map iso-bnf-production-name productions))
    (for-each
     (lambda (production)
       (let (name (iso-bnf-production-name production))
         (when (> (length (filter (cut string=? name <>) names)) 1)
           (error "duplicate ISO BNF production" name))
         (for-each
          (lambda (reference)
            (unless (member reference names)
              (error "unresolved ISO BNF production reference"
                     name reference)))
          (iso-bnf-production-references production))))
     productions)
    productions))

;; : (-> String String String String IsoBnfSource)
(def (parse-iso-bnf-source language version commit source)
  (unless (and (string? language) (string? version) (string? commit)
               (string? source))
    (error "ISO BNF source identity and content must be strings"))
  (let (productions (validate-productions (parse-production-rows source)))
    (when (null? productions)
      (error "ISO BNF source contains no productions"))
    (make-iso-bnf-source
     +iso-bnf-source-schema+ language version commit
     (sha256-text source) productions)))

;; : (-> String String String String String IsoBnfSource)
(def (parse-iso-bnf-source/expected language version commit expected-digest source)
  (let (catalog (parse-iso-bnf-source language version commit source))
    (unless (string=? (iso-bnf-source-digest catalog) expected-digest)
      (error "ISO BNF source digest mismatch"
             expected-digest (iso-bnf-source-digest catalog)))
    catalog))

;; : (-> IsoBnfSource String (Maybe IsoBnfProduction))
(def (iso-bnf-source-production source name)
  (find (lambda (production)
          (string=? (iso-bnf-production-name production) name))
        (iso-bnf-source-productions source)))

;;; BNF production names are already the grammar's native namespace.  Scheme
;;; symbols can preserve their spelling exactly, including embedded spaces.
;; : (-> String Symbol)
(def (iso-bnf-name-symbol name)
  (string->symbol name))

;; : (-> String Symbol)
(def (iso-bnf-node-kind name)
  (string->symbol name))

;; : (-> String Boolean)
(def (iso-bnf-string-member? value values)
  (and (pair? values)
       (or (string=? value (car values))
           (iso-bnf-string-member? value (cdr values)))))

;;; These source productions describe lexical classes rather than parser
;;; nonterminals.  They lower to the shared scanner token contract so Unicode
;;; prose such as "XID_START" never becomes a fabricated literal grammar.
(def +iso-bnf-identifier-productions+
  '("non-delimited identifier" "regular identifier" "extended identifier"
    "identifier start" "identifier extend"))
(def +iso-bnf-string-productions+
  '("character string literal" "single quoted character sequence"
    "double quoted character sequence"
    "unbroken single quoted character sequence"
    "unbroken double quoted character sequence"))
(def +iso-bnf-delimited-identifier-productions+
  '("accent quoted character sequence"
    "unbroken accent quoted character sequence"))
(def +iso-bnf-number-productions+
  '("signed numeric literal" "unsigned numeric literal" "exact numeric literal"
    "approximate numeric literal" "unsigned integer"
    "signed decimal integer" "unsigned decimal integer"
    "unsigned hexadecimal integer" "unsigned octal integer"
    "unsigned decimal in scientific notation"
    "unsigned decimal in common notation"))

;; ISO grammars sometimes spell the non-empty union of A and B as
;;   A [B] | [A] B
;; which admits A B twice. Preserve the source AST verbatim, but compile that
;; set union as A [B] | B so LR construction does not manufacture duplicate
;; successful paths. This is language-preserving algebra, not an overlay.
;; : (-> (List GrammarExpr) (List GrammarExpr))
(def (iso-bnf-choice-normalize-overlap alternatives)
  (if (and (= (length alternatives) 2)
           (eq? (caar alternatives) 'sequence)
           (eq? (caadr alternatives) 'sequence))
    (let* ((left (car alternatives))
           (right (cadr alternatives))
           (left-items (cdr left))
           (right-items (cdr right)))
      (cond
       ((and (= (length left-items) 2)
             (= (length right-items) 2)
             (eq? (car (cadr left-items)) 'optional)
             (eq? (car (car right-items)) 'optional)
             (equal? (car left-items) (cadr (car right-items)))
             (equal? (cadr (cadr left-items)) (cadr right-items)))
        (list left (cadr right-items)))
       ((and (= (length left-items) 2)
             (= (length right-items) 2)
             (eq? (car (car left-items)) 'optional)
             (eq? (car (cadr right-items)) 'optional)
             (equal? (cadr (car left-items)) (car right-items))
             (equal? (cadr left-items) (cadr (cadr right-items))))
        (list right (cadr left-items)))
       (else alternatives)))
    alternatives))

;; : (-> String (Maybe GrammarExpr))
(def (iso-bnf-lexical-override name)
  (cond
   ((iso-bnf-string-member? name +iso-bnf-identifier-productions+)
    '(token identifier))
   ((iso-bnf-string-member? name +iso-bnf-string-productions+)
    '(token string))
   ((iso-bnf-string-member? name +iso-bnf-delimited-identifier-productions+)
    '(token delimited-identifier))
   ((string=? name "signed numeric literal")
    '(choice (token number) (sequence (literal "-") (token number))))
   ((iso-bnf-string-member? name +iso-bnf-number-productions+)
    '(token number))
   (else #f)))

;; : (-> BnfAst GrammarExpr)
(def (iso-bnf-ast->grammar-expression ast)
  (case (car ast)
    ((reference)
     (list 'reference (iso-bnf-name-symbol (cadr ast))))
    ((terminal) (list 'literal (cadr ast)))
    ((annotated-terminal) (list 'literal (cadr ast)))
    ((group) (iso-bnf-ast->grammar-expression (cadr ast)))
    ((optional repeat repeat1)
     (list (car ast) (iso-bnf-ast->grammar-expression (cadr ast))))
    ((sequence)
     (cons 'sequence
           (map iso-bnf-ast->grammar-expression (cdr ast))))
    ((choice)
     (cons 'choice
           (iso-bnf-choice-normalize-overlap
            (map iso-bnf-ast->grammar-expression (cdr ast)))))
    ((empty) '(empty))
    (else (error "unsupported ISO BNF AST node" ast))))

;; : (-> IsoBnfSource [GrammarRule])
(def (iso-bnf-source-grammar-rules source)
  (map
   (lambda (production)
     (let ((name (iso-bnf-production-name production)))
       (list
        (iso-bnf-name-symbol name)
        (list 'alias
              (iso-bnf-node-kind name)
              (or (iso-bnf-lexical-override name)
                  (iso-bnf-ast->grammar-expression
                   (iso-bnf-production-ast production)))))))
   (iso-bnf-source-productions source)))

;; : (-> Datum String)
(def (iso-bnf-canonical value)
  (call-with-output-string (lambda (port) (write value port))))

;; : (-> List Symbol (Maybe List))
(def (iso-bnf-rule-row rules name)
  (find (lambda (row) (eq? (car row) name)) rules))

;;; A compatibility overlay is separate from the immutable source catalog.
;;; Every row binds structured overlay metadata and the exact source-derived
;;; expression it expects, so a source update cannot silently inherit a stale
;;; replacement.  Row shape: (rule-name metadata expected replacement).
;; : (-> [GrammarRule] [IsoBnfRuleOverride] [GrammarRule])
(def (iso-bnf-apply-rule-overrides rules overrides)
  (let loop ((rest overrides) (seen '()) (current rules))
    (if (null? rest)
      current
      (let (override (car rest))
        (unless (and (list? override)
                     (= (length override) 4)
                     (symbol? (car override))
                     (list? (cadr override))
                     (equal? (alet (entry (assq 'schema (cadr override)))
                               (cdr entry))
                             +iso-bnf-rule-overlay-schema+)
                     (symbol? (alet (entry (assq 'namespace (cadr override)))
                                (cdr entry)))
                     (symbol? (alet (entry (assq 'name (cadr override)))
                                (cdr entry)))
                     (string? (alet (entry (assq 'sourceVersion
                                                 (cadr override)))
                                (cdr entry)))
                     (string? (alet (entry (assq 'upstreamCommit
                                                 (cadr override)))
                                (cdr entry)))
                     (pair? (caddr override))
                     (pair? (cadddr override)))
          (error "invalid ISO BNF rule override" override))
        (let* ((name (car override))
               (expected (caddr override))
               (replacement (cadddr override))
               (row (iso-bnf-rule-row current name)))
          (when (memq name seen)
            (error "duplicate ISO BNF rule override" name))
          (unless row
            (error "ISO BNF rule override target does not exist" name))
          (unless (equal? (cadr row) expected)
            (error "ISO BNF rule override source expression mismatch"
                   name expected (cadr row)))
          (loop
           (cdr rest) (cons name seen)
           (map (lambda (candidate)
                  (if (eq? (car candidate) name)
                    (list name replacement)
                    candidate))
                current)))))))

;; : (-> IsoBnfSource [IsoBnfRuleOverride] [GrammarRule])
(def (iso-bnf-source-grammar-rules/overrides source overrides)
  (iso-bnf-apply-rule-overrides
   (iso-bnf-source-grammar-rules source) overrides))

;;; A precedence overlay resolves an ambiguity already present in the pinned
;;; source without copying its complete expression into the language pack.
;;; The source catalog digest remains the authority, while Bound IR records
;;; the exact source and replacement expression digests. Row shape:
;;;   (rule-name metadata direction rank)
;; : (-> [GrammarRule] [IsoBnfRulePrecedence] [GrammarRule])
(def (iso-bnf-apply-rule-precedences rules precedences)
  (let loop ((rest precedences) (seen '()) (current rules))
    (if (null? rest)
      current
      (let (precedence (car rest))
        (unless (and (list? precedence)
                     (= (length precedence) 4)
                     (symbol? (car precedence))
                     (list? (cadr precedence))
                     (equal? (alet (entry (assq 'schema (cadr precedence)))
                               (cdr entry))
                             +iso-bnf-rule-overlay-schema+)
                     (symbol? (alet (entry (assq 'namespace (cadr precedence)))
                                (cdr entry)))
                     (symbol? (alet (entry (assq 'name (cadr precedence)))
                                (cdr entry)))
                     (equal? (alet (entry (assq 'kind (cadr precedence)))
                               (cdr entry))
                             'disambiguation)
                     (string? (alet (entry (assq 'sourceVersion
                                                 (cadr precedence)))
                                (cdr entry)))
                     (string? (alet (entry (assq 'upstreamCommit
                                                 (cadr precedence)))
                                (cdr entry)))
                     (memq (caddr precedence) '(none left right dynamic))
                     (integer? (cadddr precedence)))
          (error "invalid ISO BNF rule precedence" precedence))
        (let* ((name (car precedence))
               (direction (caddr precedence))
               (rank (cadddr precedence))
               (row (iso-bnf-rule-row current name)))
          (when (memq name seen)
            (error "duplicate ISO BNF rule precedence" name))
          (unless row
            (error "ISO BNF rule precedence target does not exist" name))
          (loop
           (cdr rest) (cons name seen)
           (map (lambda (candidate)
                  (if (eq? (car candidate) name)
                    (list name
                          (list 'precedence direction rank (cadr candidate)))
                    candidate))
                current)))))))

;; : (-> IsoBnfSource [SyntaxKind])
(def (iso-bnf-source-syntax-kinds source)
  (map (lambda (production)
         (list (iso-bnf-node-kind (iso-bnf-production-name production))
               'node '()))
       (iso-bnf-source-productions source)))

;; : (-> GrammarExpr [String])
(def (iso-bnf-grammar-literals expression)
  (case (car expression)
    ((literal) (list (cadr expression)))
    ((sequence choice)
     (apply append (map iso-bnf-grammar-literals (cdr expression))))
    ((optional repeat repeat1)
     (iso-bnf-grammar-literals (cadr expression)))
    ((field alias)
     (iso-bnf-grammar-literals (caddr expression)))
    ((precedence)
     (iso-bnf-grammar-literals (cadddr expression)))
    (else '())))

;; : (-> [String] [String] [String])
(def (iso-bnf-unique-strings rest found)
  (if (null? rest)
    (reverse found)
    (iso-bnf-unique-strings
     (cdr rest)
     (if (iso-bnf-string-member? (car rest) found)
       found
       (cons (car rest) found)))))

;; : (-> IsoBnfSource [[GrammarRule]] [String])
(def (iso-bnf-source-literals source (rules #f))
  (iso-bnf-unique-strings
   (apply append
          (map (lambda (row) (iso-bnf-grammar-literals (cadr row)))
               (or rules (iso-bnf-source-grammar-rules source))))
   '()))

;;; Source line ownership is serializable Bound Grammar IR evidence.  The
;;; source catalog remains the authority; no runtime Syntax object is retained.
;; : (-> IsoBnfSource String DeclarationSourceMap)
(def (iso-bnf-source-declaration-sources source path)
  (list
   (cons
    'rule
    (map (lambda (production)
           (cons
            (iso-bnf-name-symbol (iso-bnf-production-name production))
            (list (cons 'path path)
                  (cons 'location
                        (string-append path ":"
                                       (number->string
                                        (iso-bnf-production-line production))))
                  (cons 'line (iso-bnf-production-line production))
                  (cons 'generated? #t))))
         (iso-bnf-source-productions source)))))

;; : (-> Alist Symbol Alist Alist)
(def (iso-bnf-annotate-rule-source source-map name annotation)
  (map
   (lambda (section)
     (if (eq? (car section) 'rule)
       (cons
        'rule
        (map (lambda (row)
               (if (eq? (car row) name)
                 (cons name (append (cdr row) annotation))
                 row))
             (cdr section)))
       section))
   source-map))

;;; Bound IR keeps the upstream path/line while making every applied overlay
;;; and both sides of its fail-closed comparison directly inspectable.
;; : (-> IsoBnfSource String [IsoBnfRuleOverride] DeclarationSourceMap)
(def (iso-bnf-source-declaration-sources/overrides source path overrides)
  ;; Validate the complete set before publishing any annotated source map.
  (iso-bnf-source-grammar-rules/overrides source overrides)
  (foldl
   (lambda (override source-map)
     (iso-bnf-annotate-rule-source
      source-map (car override)
      (list
       (cons 'compatibilityOverlay (cadr override))
       (cons 'sourceExpressionDigest
             (sha256-text (iso-bnf-canonical (caddr override))))
       (cons 'replacementExpressionDigest
             (sha256-text (iso-bnf-canonical (cadddr override)))))))
   (iso-bnf-source-declaration-sources source path)
   overrides))

;; : (-> DeclarationSourceMap [GrammarRule] [IsoBnfRulePrecedence]
;;        DeclarationSourceMap)
(def (iso-bnf-source-declaration-sources/precedences source-map rules
                                                     precedences)
  (foldl
   (lambda (precedence current-map)
     (let* ((name (car precedence))
            (row (iso-bnf-rule-row rules name))
            (source-expression (and row (cadr row)))
            (replacement
             (and source-expression
                  (list 'precedence (caddr precedence) (cadddr precedence)
                        source-expression))))
       (unless row
         (error "ISO BNF rule precedence target does not exist" name))
       (iso-bnf-annotate-rule-source
        current-map name
        (list
         (cons 'precedenceOverlay (cadr precedence))
         (cons 'sourceExpressionDigest
               (sha256-text (iso-bnf-canonical source-expression)))
         (cons 'replacementExpressionDigest
               (sha256-text (iso-bnf-canonical replacement)))))))
   source-map precedences))

;;; -*- Gerbil -*-
;;; Lossless-enough ANTLR4 grammar catalog used only at language compilation.

(import :gerbil-parser/src/runtime/identity)
(export +antlr4-source-schema-v1+
        antlr4-rule?
        antlr4-rule-name
        antlr4-rule-kind
        antlr4-rule-fragment?
        antlr4-rule-expression
        antlr4-rule-references
        antlr4-source?
        antlr4-source-language
        antlr4-source-version
        antlr4-source-commit
        antlr4-source-digest
        antlr4-source-name
        antlr4-source-options
        antlr4-source-rules
        antlr4-source-parser-rules
        antlr4-source-lexer-rules
        antlr4-source-rule
        antlr4-source-parser-grammar-rules
        antlr4-source-parser-syntax-kinds
        antlr4-source-parser-literals
        antlr4-source->datum
        antlr4-source-from-datum
        parse-antlr4-source
        parse-antlr4-source/expected)

(def +antlr4-source-schema-v1+ "gerbil-parser.antlr4-source.v1")

(defstruct antlr4-rule (name kind fragment? expression references)
  transparent: #t)
(defstruct antlr4-source
  (schema language version commit digest name options rules)
  transparent: #t)

(def (token kind value)
  (cons kind value))

(def (token-kind value)
  (car value))

(def (token-value value)
  (cdr value))

(def (antlr4-space? ch)
  (char-whitespace? ch))

(def (antlr4-identifier-start? ch)
  (or (char-alphabetic? ch) (char=? ch #\_)))

(def (antlr4-identifier-rest? ch)
  (or (antlr4-identifier-start? ch) (char-numeric? ch)))

(def (literal-at? source offset literal)
  (let ((source-length (string-length source))
        (literal-length (string-length literal)))
    (and (<= (+ offset literal-length) source-length)
         (string=? (substring source offset (+ offset literal-length))
                   literal))))

(def (scan-until-line-end source offset)
  (let (length (string-length source))
    (let loop ((cursor offset))
      (if (or (= cursor length)
              (char=? (string-ref source cursor) #\newline))
        cursor
        (loop (+ cursor 1))))))

(def (scan-block-comment-end source offset)
  (let (length (string-length source))
    (let loop ((cursor (+ offset 2)))
      (cond
       ((>= cursor length)
        (error "unterminated ANTLR4 block comment" offset))
       ((literal-at? source cursor "*/") (+ cursor 2))
       (else (loop (+ cursor 1)))))))

(def (scan-delimited-end source offset delimiter owner)
  (let (length (string-length source))
    (let loop ((cursor (+ offset 1)) (escaped? #f))
      (cond
       ((>= cursor length)
        (error "unterminated ANTLR4 delimited token" owner offset))
       (escaped? (loop (+ cursor 1) #f))
       ((char=? (string-ref source cursor) #\\)
        (loop (+ cursor 1) #t))
       ((char=? (string-ref source cursor) delimiter) (+ cursor 1))
       (else (loop (+ cursor 1) #f))))))

(def (scan-antlr4-tokens source)
  (let (length (string-length source))
    (let loop ((offset 0) (tokens '()))
      (cond
       ((= offset length) (reverse tokens))
       ((antlr4-space? (string-ref source offset))
        (loop (+ offset 1) tokens))
       ((literal-at? source offset "//")
        (loop (scan-until-line-end source (+ offset 2)) tokens))
       ((literal-at? source offset "/*")
        (loop (scan-block-comment-end source offset) tokens))
       ((char=? (string-ref source offset) #\')
        (let (end (scan-delimited-end source offset #\' 'literal))
          (loop end
                (cons (token 'literal
                             (substring source (+ offset 1) (- end 1)))
                      tokens))))
       ((char=? (string-ref source offset) #\[)
        (let (end (scan-delimited-end source offset #\] 'character-class))
          (loop end
                (cons (token 'character-class
                             (substring source offset end))
                      tokens))))
       ((antlr4-identifier-start? (string-ref source offset))
        (let next ((end (+ offset 1)))
          (if (and (< end length)
                   (antlr4-identifier-rest? (string-ref source end)))
            (next (+ end 1))
            (loop end
                  (cons (token 'identifier (substring source offset end))
                        tokens)))))
       ((literal-at? source offset "->")
        (loop (+ offset 2) (cons (token 'punctuation "->") tokens)))
       ((literal-at? source offset "+=")
        (loop (+ offset 2) (cons (token 'punctuation "+=") tokens)))
       (else
        (loop (+ offset 1)
              (cons (token 'punctuation
                           (string (string-ref source offset)))
                    tokens)))))))

(def (identifier-token? value wanted)
  (and (pair? value)
       (eq? (token-kind value) 'identifier)
       (string=? (token-value value) wanted)))

(def (punctuation-token? value wanted)
  (and (pair? value)
       (eq? (token-kind value) 'punctuation)
       (string=? (token-value value) wanted)))

(def (skip-balanced tokens opening closing)
  (unless (and (pair? tokens) (punctuation-token? (car tokens) opening))
    (error "ANTLR4 balanced block expected" opening))
  (let loop ((rest (cdr tokens)) (depth 1))
    (unless (pair? rest)
      (error "unterminated ANTLR4 balanced block" opening closing))
    (cond
     ((punctuation-token? (car rest) opening)
      (loop (cdr rest) (+ depth 1)))
     ((punctuation-token? (car rest) closing)
      (if (= depth 1) (cdr rest) (loop (cdr rest) (- depth 1))))
     (else (loop (cdr rest) depth)))))

(def (collect-rule-expression tokens)
  (let loop ((rest tokens) (depth 0) (found '()))
    (unless (pair? rest)
      (error "unterminated ANTLR4 rule"))
    (let (current (car rest))
      (cond
       ((and (zero? depth) (punctuation-token? current ";"))
        (values (reverse found) (cdr rest)))
       ((or (punctuation-token? current "(")
            (punctuation-token? current "[")
            (punctuation-token? current "{"))
        (loop (cdr rest) (+ depth 1) (cons current found)))
       ((or (punctuation-token? current ")")
            (punctuation-token? current "]")
            (punctuation-token? current "}"))
        (when (zero? depth)
          (error "unbalanced ANTLR4 rule expression" (token-value current)))
        (loop (cdr rest) (- depth 1) (cons current found)))
       (else (loop (cdr rest) depth (cons current found)))))))

(def (uppercase-rule-name? name)
  (and (positive? (string-length name))
       (char-upper-case? (string-ref name 0))))

(def (string-member? value values)
  (and (pair? values)
       (or (string=? value (car values))
           (string-member? value (cdr values)))))

(def (unique-strings values)
  (let loop ((rest values) (found '()))
    (if (null? rest)
      (reverse found)
      (loop (cdr rest)
            (if (string-member? (car rest) found)
              found
              (cons (car rest) found))))))

(def (expression-before-command expression)
  (let loop ((rest expression) (found '()))
    (if (or (null? rest) (punctuation-token? (car rest) "->"))
      (reverse found)
      (loop (cdr rest) (cons (car rest) found)))))

(def (expression-references expression)
  (let loop ((rest (expression-before-command expression))
             (previous #f)
             (found '()))
    (if (null? rest)
      (unique-strings (reverse found))
      (let* ((current (car rest))
             (next (and (pair? (cdr rest)) (cadr rest)))
             (reference?
              (and (eq? (token-kind current) 'identifier)
                   (not (and previous (punctuation-token? previous "#")))
                   (not (and next
                             (or (punctuation-token? next "=")
                                 (punctuation-token? next "+="))))
                   (not (string=? (token-value current) "options")))))
        (loop (cdr rest) current
              (if reference? (cons (token-value current) found) found))))))

(def (parse-rule tokens fragment?)
  (unless (and (pair? tokens) (eq? (token-kind (car tokens)) 'identifier))
    (error "ANTLR4 rule name expected"))
  (let* ((name (token-value (car tokens)))
         (after-name (cdr tokens))
         (after-options
          (if (and (pair? after-name)
                   (identifier-token? (car after-name) "options"))
            (skip-balanced (cdr after-name) "{" "}")
            after-name)))
    (unless (and (pair? after-options)
                 (punctuation-token? (car after-options) ":"))
      (error "ANTLR4 rule separator expected" name))
    (let-values (((expression rest)
                  (collect-rule-expression (cdr after-options))))
      (values
       (make-antlr4-rule
        name
        (if (or fragment? (uppercase-rule-name? name)) 'lexer 'parser)
        fragment?
        expression
        (expression-references expression))
       rest))))

(def (parse-top-level tokens)
  (unless (and (pair? tokens) (identifier-token? (car tokens) "grammar")
               (pair? (cdr tokens))
               (eq? (token-kind (cadr tokens)) 'identifier)
               (pair? (cddr tokens))
               (punctuation-token? (caddr tokens) ";"))
    (error "ANTLR4 grammar header expected"))
  (let ((name (token-value (cadr tokens))))
    (let loop ((rest (cdddr tokens)) (options '()) (rules '()))
      (cond
       ((null? rest) (values name (reverse options) (reverse rules)))
       ((identifier-token? (car rest) "options")
        (unless (pair? (cdr rest))
          (error "ANTLR4 options block expected"))
        (loop (skip-balanced (cdr rest) "{" "}")
              (cons 'options options) rules))
       ((identifier-token? (car rest) "fragment")
        (let-values (((rule next) (parse-rule (cdr rest) #t)))
          (loop next options (cons rule rules))))
       ((eq? (token-kind (car rest)) 'identifier)
        (let-values (((rule next) (parse-rule rest #f)))
          (loop next options (cons rule rules))))
       (else
        (error "unexpected ANTLR4 top-level token" (car rest)))))))

(def (rule-names rules)
  (map antlr4-rule-name rules))

(def (validate-rules rules)
  ;; Build the catalog once.  The former implementation filtered the complete
  ;; rule-name list once per rule and then linearly scanned it for every
  ;; reference, making admission quadratic for full language grammars.
  (let (catalog (make-table test: equal?))
    (for-each
     (lambda (rule)
       (let (name (antlr4-rule-name rule))
         (when (table-ref catalog name #f)
           (error "duplicate ANTLR4 rule" name))
         (table-set! catalog name rule)))
     rules)
    (for-each
     (lambda (rule)
       (let (name (antlr4-rule-name rule))
         (for-each
          (lambda (reference)
            (unless (or (string=? reference "EOF")
                        (table-ref catalog reference #f))
              (error "unresolved ANTLR4 rule reference" name reference)))
          (antlr4-rule-references rule))))
     rules)
    rules))

(def (parse-antlr4-source language version commit source)
  (unless (and (string? language) (string? version) (string? commit)
               (string? source))
    (error "ANTLR4 source identity and content must be strings"))
  (let-values (((name options rules)
                (parse-top-level (scan-antlr4-tokens source))))
    (make-antlr4-source
     +antlr4-source-schema-v1+ language version commit (sha256-text source)
     name options (validate-rules rules))))

(def (parse-antlr4-source/expected language version commit expected-digest source)
  (let (catalog (parse-antlr4-source language version commit source))
    (unless (string=? (antlr4-source-digest catalog) expected-digest)
      (error "ANTLR4 source digest mismatch"
             expected-digest (antlr4-source-digest catalog)))
    catalog))

(def (antlr4-source-parser-rules source)
  (filter (lambda (rule) (eq? (antlr4-rule-kind rule) 'parser))
          (antlr4-source-rules source)))

(def (antlr4-source-lexer-rules source)
  (filter (lambda (rule) (eq? (antlr4-rule-kind rule) 'lexer))
          (antlr4-source-rules source)))

(def (antlr4-source-rule source name)
  (find (lambda (rule) (string=? (antlr4-rule-name rule) name))
        (antlr4-source-rules source)))

(def (antlr4-source-rule-index source)
  (let (index (make-table test: equal?))
    (for-each
     (lambda (rule)
       (table-set! index (antlr4-rule-name rule) rule))
     (antlr4-source-rules source))
    index))

(def (antlr4-rule->datum rule)
  (list (antlr4-rule-name rule)
        (antlr4-rule-kind rule)
        (antlr4-rule-fragment? rule)
        (antlr4-rule-expression rule)
        (antlr4-rule-references rule)))

(def (antlr4-source->datum source)
  (list +antlr4-source-schema-v1+
        (antlr4-source-language source)
        (antlr4-source-version source)
        (antlr4-source-commit source)
        (antlr4-source-digest source)
        (antlr4-source-name source)
        (antlr4-source-options source)
        (map antlr4-rule->datum (antlr4-source-rules source))))

(def (antlr4-source-from-datum value)
  (unless (and (list? value) (= (length value) 8)
               (string=? (car value) +antlr4-source-schema-v1+))
    (error "invalid materialized ANTLR4 source v1" value))
  (make-antlr4-source
   (car value) (cadr value) (caddr value) (cadddr value)
   (list-ref value 4) (list-ref value 5) (list-ref value 6)
   (map (lambda (row)
          (apply make-antlr4-rule row))
        (list-ref value 7))))

(def (antlr4-empty)
  '(empty))

(def (antlr4-sequence expressions)
  (cond
   ((null? expressions) (antlr4-empty))
   ((null? (cdr expressions)) (car expressions))
   (else (cons 'sequence expressions))))

(def (antlr4-choice expressions)
  (cond
   ((null? expressions) (antlr4-empty))
   ((null? (cdr expressions)) (car expressions))
   (else (cons 'choice expressions))))

(def (parse-expression-choice tokens resolve)
  (let loop ((rest tokens) (alternatives '()))
    (let-values (((sequence next) (parse-expression-sequence rest resolve)))
      (if (and (pair? next) (punctuation-token? (car next) "|"))
        (loop (cdr next) (cons sequence alternatives))
        (values (antlr4-choice (reverse (cons sequence alternatives))) next)))))

(def (parse-expression-sequence tokens resolve)
  (let loop ((rest tokens) (expressions '()))
    (cond
     ((or (null? rest)
          (punctuation-token? (car rest) "|")
          (punctuation-token? (car rest) ")"))
      (values (antlr4-sequence (reverse expressions)) rest))
     ((punctuation-token? (car rest) "#")
      (unless (and (pair? (cdr rest))
                   (eq? (token-kind (cadr rest)) 'identifier))
        (error "ANTLR4 alternative label expected"))
      (values (antlr4-sequence (reverse expressions)) (cddr rest)))
     (else
      (let-values (((expression next) (parse-expression-postfix rest resolve)))
        (loop next (cons expression expressions)))))))

(def (parse-expression-postfix tokens resolve)
  (let-values (((atom rest) (parse-expression-atom tokens resolve)))
    (if (and (pair? rest) (eq? (token-kind (car rest)) 'punctuation))
      (let ((operator (token-value (car rest))))
        (cond
         ((string=? operator "?")
          (values (list 'optional atom) (cdr rest)))
         ((string=? operator "*")
          (values (list 'repeat atom)
                  (if (and (pair? (cdr rest))
                           (punctuation-token? (cadr rest) "?"))
                    (cddr rest)
                    (cdr rest))))
         ((string=? operator "+")
          (values (list 'repeat1 atom)
                  (if (and (pair? (cdr rest))
                           (punctuation-token? (cadr rest) "?"))
                    (cddr rest)
                    (cdr rest))))
         (else (values atom rest))))
      (values atom rest))))

(def (parse-expression-atom tokens resolve)
  (unless (pair? tokens)
    (error "ANTLR4 expression atom expected"))
  (let ((current (car tokens))
        (next (and (pair? (cdr tokens)) (cadr tokens))))
    (cond
     ((eq? (token-kind current) 'literal)
      (values (list 'literal (token-value current)) (cdr tokens)))
     ((eq? (token-kind current) 'identifier)
      (if (and next
               (or (punctuation-token? next "=")
                   (punctuation-token? next "+=")))
        (let-values (((value rest)
                      (parse-expression-postfix (cddr tokens) resolve)))
          (values (list 'field (string->symbol (token-value current)) value)
                  rest))
        (values (resolve (token-value current)) (cdr tokens))))
     ((punctuation-token? current "(")
      (let-values (((expression rest)
                    (parse-expression-choice (cdr tokens) resolve)))
        (unless (and (pair? rest) (punctuation-token? (car rest) ")"))
          (error "unterminated ANTLR4 grouped expression"))
        (values expression (cdr rest))))
     (else
      (error "unsupported ANTLR4 expression atom" current)))))

(def (parse-rule-grammar-expression rule resolve)
  (let-values (((expression rest)
                (parse-expression-choice
                 (expression-before-command (antlr4-rule-expression rule))
                 resolve)))
    (unless (null? rest)
      (error "unconsumed ANTLR4 rule expression"
             (antlr4-rule-name rule) rest))
    expression))

(def (string-contains? text fragment)
  (let ((text-length (string-length text))
        (fragment-length (string-length fragment)))
    (let loop ((offset 0))
      (and (<= (+ offset fragment-length) text-length)
           (or (string=? (substring text offset (+ offset fragment-length))
                         fragment)
               (loop (+ offset 1)))))))

(def (identifier-token-name? name)
  (or (string-contains? name "IDENTIFIER")
      (string=? name "PARAMETER_NAME")))

(def (string-token-name? name)
  (or (string-contains? name "CHARACTER_SEQUENCE")
      (string-contains? name "STRING_LITERAL")))

(def (number-token-name? name)
  (or (string-contains? name "UNSIGNED_DECIMAL")
      (string-contains? name "UNSIGNED_HEXADECIMAL")
      (string-contains? name "UNSIGNED_OCTAL")
      (string-contains? name "UNSIGNED_BINARY")))

(def (constant-lexer-expression rule-index name seen)
  (and (not (string-member? name seen))
       (let (rule (table-ref rule-index name #f))
         (and rule
              (eq? (antlr4-rule-kind rule) 'lexer)
              (with-catch
               (lambda (_) #f)
               (lambda ()
                 (parse-rule-grammar-expression
                  rule
                  (lambda (reference)
                    (or (constant-lexer-expression
                         rule-index reference (cons name seen))
                        (error "non-constant lexer rule" reference))))))))))

(def (parser-terminal-expression rule-index name)
  (def (lower name seen)
    (cond
     ((string=? name "EOF") (antlr4-empty))
     ((constant-lexer-expression rule-index name '()) => values)
     ((identifier-token-name? name) '(token identifier))
     ((string-token-name? name) '(token string))
     ((number-token-name? name) '(token number))
     ((string-member? name seen)
      (error "cyclic ANTLR4 lexer rule" name))
     (else
      (let (rule (table-ref rule-index name #f))
        (unless (and rule (eq? (antlr4-rule-kind rule) 'lexer))
          (error "ANTLR4 lexer rule expected" name))
        (with-catch
         (lambda (_)
           (error "OpenGQL lexer token requires an admitted scanner primitive"
                  name))
         (lambda ()
           (parse-rule-grammar-expression
            rule
            (lambda (reference)
              (lower reference (cons name seen))))))))))
  (lower name '()))

(def (direct-left-recursive? expression rule-name)
  (let (body
        (if (and (pair? expression) (eq? (car expression) 'sequence))
          (cdr expression)
          (list expression)))
    (and (pair? body)
         (equal? (car body) (list 'reference rule-name)))))

(def (apply-antlr-precedence expression rule-name)
  (if (and (pair? expression) (eq? (car expression) 'choice))
    (let* ((alternatives (cdr expression))
           (count (length alternatives)))
      (cons
       'choice
       (let loop ((rest alternatives) (rank count) (found '()))
         (if (null? rest)
           (reverse found)
           (let (alternative (car rest))
             (loop
              (cdr rest) (- rank 1)
              (cons
               (if (direct-left-recursive? alternative rule-name)
                 (list 'precedence 'left rank alternative)
                 alternative)
               found)))))))
    expression))

(def (antlr4-source-parser-grammar-rules source)
  (let ((rule-index (antlr4-source-rule-index source))
        (terminal-cache (make-table test: equal?)))
    (def (resolve reference)
      (if (uppercase-rule-name? reference)
        (or (table-ref terminal-cache reference #f)
            (let (expression
                  (parser-terminal-expression rule-index reference))
              (table-set! terminal-cache reference expression)
              expression))
        (list 'reference (string->symbol reference))))
    (map
     (lambda (rule)
       (let* ((name (string->symbol (antlr4-rule-name rule)))
              (expression (parse-rule-grammar-expression rule resolve)))
         (list name
               (list 'alias
                     (upper-initial-symbol (antlr4-rule-name rule))
                     (apply-antlr-precedence expression name)))))
     (antlr4-source-parser-rules source))))

(def (upper-initial-symbol name)
  (let (copy (string-copy name))
    (string-set! copy 0 (char-upcase (string-ref copy 0)))
    (string->symbol copy)))

(def (antlr4-source-parser-syntax-kinds source)
  (map (lambda (rule)
         (list (upper-initial-symbol (antlr4-rule-name rule)) 'node '()))
       (antlr4-source-parser-rules source)))

(def (grammar-literals expression)
  (case (car expression)
    ((literal) (list (cadr expression)))
    ((sequence choice)
     (apply append (map grammar-literals (cdr expression))))
    ((optional repeat repeat1)
     (grammar-literals (cadr expression)))
    ((field alias)
     (grammar-literals (caddr expression)))
    ((precedence)
     (grammar-literals (cadddr expression)))
    (else '())))

(def (antlr4-source-parser-literals source)
  (unique-strings
   (apply append
          (map (lambda (row) (grammar-literals (cadr row)))
               (antlr4-source-parser-grammar-rules source)))))

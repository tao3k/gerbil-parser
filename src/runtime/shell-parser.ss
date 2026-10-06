;;; -*- Gerbil -*-
;;; Lossless Bash command/word/here-document syntax entry.
;;; Unsupported command forms reject explicitly until their grammar is owned.

(import (only-in :gerbil-parser/src/runtime/artifact
                 +diagnostic-schema+ make-failure-parse-artifact
                 make-success-parse-artifact)
        (only-in :gerbil-parser/src/runtime/recognition
                 make-recognition-child
                 recognition-child-field recognition-node-start)
        (only-in :gerbil-parser/src/runtime/token
                 make-token token-end token-kind token-lexeme token-start)
        (only-in ./contextual-scanner delimiter-obligation-quoted? decode-marker)
        (only-in ../language/result-profile result-plan-node result-plan-token)
        (only-in ./shell-word make-shell-word-parser))
(export shell-here-document-link?
        shell-here-document-link-marker-start
        shell-here-document-link-body-start
        make-shell-parser)

(defstruct shell-here-document-link (marker-start body-start) transparent: #t)

(def (make-shell-parser regions results parts-plan)
  (let-values (((shell-word-components shell-assignment-components shell-here-content-components)
                (make-shell-word-parser regions results parts-plan)))


(def (trivia? token)
  (memq (token-kind token)
        '(horizontal-whitespace comment line-continuation)))

(def (same-operator? token value)
  (and token (eq? (token-kind token) 'operator)
       (string=? (token-lexeme token) value)))

(def (separator? token)
  (or (eq? (token-kind token) 'newline)
      (and (eq? (token-kind token) 'operator)
           (member (token-lexeme token) '(";" "&")))))

(def (redirect? token)
  (and token (eq? (token-kind token) 'operator)
       (member (token-lexeme token)
               '("<" ">" ">>" "<>" "<<" "<<-" "<<<"
                 "<&" ">&" ">|" "&>" "&>>"))))

(def (here-redirect? token)
  (or (same-operator? token "<<") (same-operator? token "<<-")))

(def (digits? text)
  (and (> (string-length text) 0)
       (let loop ((offset 0))
         (or (= offset (string-length text))
             (and (char-numeric? (string-ref text offset))
                  (loop (fx+ offset 1)))))))

(def (brace-descriptor? text)
  (let (length (string-length text))
    (and (> length 2)
         (char=? (string-ref text 0) #\{)
         (char=? (string-ref text (fx- length 1)) #\})
         (let (first (string-ref text 1))
           (or (char-alphabetic? first) (char=? first #\_)))
         (let loop ((offset 2))
           (or (= offset (fx- length 1))
               (and (let (character (string-ref text offset))
                      (or (char-alphabetic? character)
                          (char-numeric? character)
                          (char=? character #\_)))
                    (loop (fx+ offset 1))))))))

(def (child field value)
  (make-recognition-child field value))

(def (syntax-node kind start end children)
  (result-plan-node results kind start end children))

(def (failure-diagnostic condition)
  (list (cons 'schema +diagnostic-schema+)
        (cons 'code "BASH-SYNTAX-REJECTED")
        (cons 'reasonKind 'parse-rejected)
        (cons 'message (error-message condition))))

(def (fallback-token source)
  (if (zero? (string-length source)) '()
    (list (result-plan-token results (make-token 'unparsed-source source 0
                      (u8vector-length (string->utf8 source)))))))

;;; Returns the artifact plus source-span links from redirection markers to
;;; their deferred here-document bodies. The links do not change CST order.
(def (parse-shell-core/receipt source scan grammar-digest)
  (unless (string? source) (error "Bash source must be a string" source))
  (with-catch
   (lambda (condition)
     (values
      (make-failure-parse-artifact
       grammar-digest source (fallback-token source)
       (failure-diagnostic condition))
      '()))
   (lambda ()
     (let ((remaining (scan source))
           (emitted-reversed '())
           (pending-markers '())
           (links '()))
       (def (emit! token)
         (set! emitted-reversed (cons (result-plan-token results token) emitted-reversed)))
       (def (take-raw!)
         (unless (pair? remaining) (error "unexpected Bash EOF"))
         (let (token (car remaining))
           (set! remaining (cdr remaining))
           (emit! token)
           token))
       (def (skip-trivia!)
         (let loop ()
           (when (and (pair? remaining) (trivia? (car remaining)))
             (take-raw!)
             (loop))))
       (def (peek)
         (skip-trivia!)
         (and (pair? remaining) (car remaining)))
       (def (word-is? token text)
         (and token (eq? (token-kind token) 'word)
              (string=? (token-lexeme token) text)))
       (def (take-keyword! text)
         (let (token (peek))
           (unless (word-is? token text)
             (error "expected Bash reserved word" text token))
           (take-raw!)))
       (def (last-byte)
         (if (pair? emitted-reversed)
           (token-end (car emitted-reversed)) 0))
       (def (take-word!)
         (let (raw (peek))
           (unless (and raw (eq? (token-kind raw) 'word))
             (error "expected Bash word" raw))
           (set! remaining (cdr remaining))
           (let-values (((word pieces) (shell-word-components raw)))
             (for-each emit! pieces)
             word)))
       (def (parse-redirection! descriptor)
         (let* ((operator (peek))
                (start (if descriptor
                         (token-start descriptor)
                         (token-start operator))))
           (unless (redirect? operator)
             (error "expected Bash redirection" operator))
           (take-raw!)
           (let (target (peek))
             (unless (and target
                          (if (here-redirect? operator)
                            (eq? (token-kind target) 'heredoc-marker)
                            (eq? (token-kind target) 'word)))
               (error "missing Bash redirection target" operator))
             (let* ((target-value
                     (if (eq? (token-kind target) 'heredoc-marker)
                       (begin
                         (set! pending-markers
                               (append pending-markers
                                       (list
                                        (cons
                                         (token-start target)
                                         (delimiter-obligation-quoted?
                                          (decode-marker 'shell-quote-removal
                                           (token-lexeme target)
                                           (same-operator?
                                            operator "<<-")))))))
                         (take-raw!))
                       (take-word!)))
                    (children
                     (append
                      (if descriptor
                        (list (child 'descriptor descriptor)) '())
                      (list (child 'operator operator)
                            (child 'target target-value)))))
               (syntax-node 'Redirection start (last-byte) children)))))
       (def (descriptor-before-redirection?)
         (and (pair? remaining)
              (eq? (token-kind (car remaining)) 'word)
              (or (digits? (token-lexeme (car remaining)))
                  (brace-descriptor?
                   (token-lexeme (car remaining))))
              (pair? (cdr remaining))
              (redirect? (cadr remaining))
              (= (token-end (car remaining))
                 (token-start (cadr remaining)))))
       (def (array-assignment-open? raw)
         (and (pair? remaining)
              (same-operator? (car remaining) "(")
              (= (token-end raw) (token-start (car remaining)))))
       (def (parse-array-assignment! assignment)
         (let* ((open (take-raw!))
                (start (recognition-node-start assignment)))
           (let loop ((children (list (child 'assignment assignment)
                                       (child 'open open))))
             (let (next (peek))
               (cond
                ((not next) (error "unterminated Bash array assignment"))
                ((same-operator? next ")")
                 (let (close (take-raw!))
                   (syntax-node
                    'ArrayAssignment start (last-byte)
                    (append children (list (child 'close close))))))
                ((eq? (token-kind next) 'newline)
                 (loop (append children
                               (list (child 'separator (take-raw!))))))
                ((eq? (token-kind next) 'word)
                 (loop (append children
                               (list (child 'element (take-word!))))))
                (else (error "invalid Bash array assignment element"
                             next)))))))
       (def (parse-simple-command!)
         (let* ((first (peek))
                (start (and first (token-start first)))
                (named? #f)
                (children '()))
           (unless first (error "expected Bash command"))
           (when (and (eq? (token-kind first) 'word)
                      (member (token-lexeme first)
                              '("if" "then" "elif" "else" "fi"
                                "while" "until" "do" "done" "for" "select"
                                "case" "in" "esac" "function"
                                "{" "}")))
             (error "Bash compound command is not admitted yet"
                    (token-lexeme first)))
           (let loop ()
             (let (token (peek))
               (cond
                ((descriptor-before-redirection?)
                 (let (descriptor (take-raw!))
                   (set! children
                         (cons (child 'redirect
                                      (parse-redirection! descriptor))
                               children))
                   (loop)))
                ((redirect? token)
                 (set! children
                       (cons (child 'redirect (parse-redirection! #f))
                             children))
                 (loop))
                ((and token (eq? (token-kind token) 'word))
                 (let-values (((assignment pieces)
                               (if named?
                                 (values #f #f)
                                 (shell-assignment-components token))))
                   (if assignment
                     (begin
                       (set! remaining (cdr remaining))
                       (for-each emit! pieces)
                       (set! children
                             (cons (child 'assignment
                                          (if (array-assignment-open? token)
                                            (parse-array-assignment!
                                             assignment)
                                            assignment))
                                   children)))
                     (let (word (take-word!))
                       (set! children
                             (cons (child (if named? 'argument 'name) word)
                                   children))
                       (set! named? #t)))
                   (loop)))
                ((null? children)
                 (error "expected Bash command or redirection" token))
                (else
                 (syntax-node 'SimpleCommand start (last-byte)
                              (reverse children))))))))
       (def (has-command? children)
         (ormap (lambda (entry)
                  (eq? (recognition-child-field entry) 'command))
                children))
       (def (parse-compound-list! start terminators
                                  (allow-empty? #f))
         (let (children (parse-list! terminators))
           (unless (or allow-empty? (has-command? children))
             (error "Bash command list is empty"))
           (syntax-node 'CommandList start (last-byte) children)))
       (def (parse-if!)
         (let* ((start (token-start (peek)))
                (children (list (child 'keyword (take-keyword! "if")))))
           (let loop ((condition-start (last-byte)))
             (set! children
                   (append children
                           (list (child 'condition
                                        (parse-compound-list!
                                         condition-start '("then")))
                                 (child 'keyword (take-keyword! "then"))
                                 (child 'body
                                        (parse-compound-list!
                                         (last-byte) '("elif" "else" "fi"))))))
             (cond
              ((word-is? (peek) "elif")
               (set! children
                     (append children
                             (list (child 'keyword (take-keyword! "elif")))))
               (loop (last-byte)))
              ((word-is? (peek) "else")
               (set! children
                     (append children
                             (list (child 'keyword (take-keyword! "else"))
                                   (child 'else-body
                                          (parse-compound-list!
                                           (last-byte) '("fi"))))))
               (set! children
                     (append children
                             (list (child 'keyword (take-keyword! "fi")))))
               (syntax-node 'IfCommand start (last-byte) children))
              (else
               (set! children
                     (append children
                             (list (child 'keyword (take-keyword! "fi")))))
               (syntax-node 'IfCommand start (last-byte) children))))))
       (def (parse-loop!)
         (let* ((keyword (peek))
                (name (token-lexeme keyword))
                (start (token-start keyword)))
           (take-raw!)
           (let* ((condition
                   (parse-compound-list! (last-byte) '("do")))
                  (do-token (take-keyword! "do"))
                  (body (parse-compound-list! (last-byte) '("done")))
                  (done-token (take-keyword! "done")))
             (syntax-node
              (if (string=? name "while") 'WhileCommand 'UntilCommand)
              start (last-byte)
              (list (child 'keyword keyword)
                    (child 'condition condition)
                    (child 'keyword do-token)
                    (child 'body body)
                    (child 'keyword done-token))))))
       (def (parse-iteration! word)
         (let* ((keyword (take-keyword! word))
                (start (token-start keyword))
                (arithmetic? (and (string=? word "for")
                                  (arithmetic-command-start?)))
                (children (list (child 'keyword keyword))))
           (if arithmetic?
             (set! children
                   (append children
                           (list (child 'header
                                        (parse-arithmetic-command!)))))
             (begin
               (set! children
                     (append children
                             (list (child 'variable (take-word!)))))
               (when (word-is? (peek) "in")
                 (set! children
                       (append children
                               (list (child 'keyword
                                            (take-keyword! "in")))))
                 (let words ()
                   (let (next (peek))
                     (when (and next (eq? (token-kind next) 'word))
                       (set! children
                             (append children
                                     (list (child 'item (take-word!)))))
                       (words)))))))
           (let (separator (peek))
             (unless (and separator (separator? separator))
               (error "iteration header requires a separator" separator))
             (take-raw!)
             (set! children
                   (append children (list (child 'separator separator)))))
           (let* ((do-token (take-keyword! "do"))
                  (body (parse-compound-list! (last-byte) '("done")))
                  (done-token (take-keyword! "done")))
             (syntax-node
              (cond
               (arithmetic? 'ArithmeticForCommand)
               ((string=? word "select") 'SelectCommand)
               (else 'ForCommand))
              start (last-byte)
                          (append children
                                  (list (child 'keyword do-token)
                                        (child 'body body)
                                        (child 'keyword done-token)))))))
       (def (parse-case!)
         (let* ((keyword (take-keyword! "case"))
                (start (token-start keyword))
                (subject (take-word!))
                (in-token (take-keyword! "in"))
                (children (list (child 'keyword keyword)
                                (child 'subject subject)
                                (child 'keyword in-token))))
           (let clauses ()
             (let (next (peek))
               (cond
                ((not next) (error "missing esac"))
                ((separator? next)
                 (take-raw!)
                 (set! children
                       (append children (list (child 'separator next))))
                 (clauses))
                ((word-is? next "esac")
                 (set! children
                       (append children
                               (list (child 'keyword (take-keyword! "esac")))))
                 (syntax-node 'CaseCommand start (last-byte) children))
                (else
                 (let* ((clause-start (token-start next))
                        (patterns '()))
                   (when (same-operator? (peek) "(")
                     (set! patterns
                           (list (child 'open (take-raw!)))))
                   (let pattern-loop ()
                     (let (pattern (peek))
                       (unless (and pattern
                                    (eq? (token-kind pattern) 'word))
                         (error "expected case pattern" pattern))
                       (set! patterns
                             (append patterns
                                     (list (child 'pattern (take-word!)))))
                       (when (same-operator? (peek) "|")
                         (set! patterns
                               (append patterns
                                       (list (child 'alternate
                                                    (take-raw!)))))
                         (pattern-loop))))
                   (unless (same-operator? (peek) ")")
                     (error "missing case pattern close"))
                   (set! patterns
                         (append patterns
                                 (list (child 'close (take-raw!)))))
                   (let* ((body
                           (parse-compound-list!
                            (last-byte) '(";;" ";&" ";;&" "esac") #t))
                          (ending (peek)))
                     (set! patterns
                           (append patterns (list (child 'body body))))
                     (when (and ending
                                (member (token-lexeme ending)
                                        '(";;" ";&" ";;&")))
                       (set! patterns
                             (append patterns
                                     (list (child 'terminator
                                                  (take-raw!))))))
                     (set! children
                           (append children
                                   (list (child
                                          'clause
                                          (syntax-node 'CaseClause
                                                       clause-start
                                                       (last-byte)
                                                       patterns)))))
                     (clauses)))))))))
       (def (skip-trivia-list tokens)
         (if (and (pair? tokens) (trivia? (car tokens)))
           (skip-trivia-list (cdr tokens)) tokens))
       (def (function-header?)
         (let* ((first (skip-trivia-list remaining))
                (second (and (pair? first)
                             (skip-trivia-list (cdr first))))
                (third (and (pair? second)
                            (skip-trivia-list (cdr second)))))
           (and (pair? first) (eq? (token-kind (car first)) 'word)
                (pair? second) (same-operator? (car second) "(")
                (pair? third) (same-operator? (car third) ")"))))
       (def (parse-function! explicit-keyword?)
         (let* ((first (peek))
                (start (token-start first))
                (children '()))
           (when explicit-keyword?
             (set! children
                   (list (child 'keyword (take-keyword! "function")))))
           (set! children
                 (append children (list (child 'name (take-word!)))))
           (when (same-operator? (peek) "(")
             (set! children
                   (append children (list (child 'open (take-raw!)))))
             (unless (same-operator? (peek) ")")
               (error "missing function parameter close"))
             (set! children
                   (append children (list (child 'close (take-raw!))))))
           (set! children
                 (append children
                         (list (child 'body (parse-command!)))))
           (syntax-node 'FunctionDefinition start (last-byte) children)))
       (def (parse-conditional!)
         (let* ((open (take-keyword! "[["))
                (start (token-start open)))
           (let loop ((children (list (child 'open open))))
             (let (next (peek))
               (cond
                ((not next) (error "missing ]]"))
                ((word-is? next "]]")
                 (let (close (take-keyword! "]]"))
                   (syntax-node
                    'ConditionalCommand start (last-byte)
                    (append children (list (child 'close close))))))
                ((and (eq? (token-kind next) 'word)
                      (member (token-lexeme next)
                              '("==" "=" "!=" "=~" "-eq" "-ne"
                                "-lt" "-le" "-gt" "-ge" "-z" "-n" "!")))
                 (loop (append children
                               (list (child 'operator (take-raw!))))))
                ((eq? (token-kind next) 'word)
                 (loop (append children
                               (list (child 'operand (take-word!))))))
                ((eq? (token-kind next) 'operator)
                 (loop (append children
                               (list (child 'operator (take-raw!))))))
                (else (error "invalid conditional token" next)))))))
       (def (arithmetic-command-start?)
         (let* ((first (skip-trivia-list remaining))
                (second (and (pair? first) (cdr first))))
           (and (pair? first) (same-operator? (car first) "(")
                (pair? second) (same-operator? (car second) "(")
                (= (token-end (car first))
                   (token-start (car second))))))
       (def (parse-arithmetic-command!)
         (peek)
         (let* ((open-first (take-raw!))
                (start (token-start open-first))
                (open-second (take-raw!)))
           (let loop ((depth 2)
                      (children (list (child 'open open-first)
                                      (child 'open open-second))))
             (let (next (peek))
               (unless next (error "missing arithmetic command close"))
               (cond
                ((same-operator? next "(")
                 (loop (fx+ depth 1)
                       (append children (list (child 'open (take-raw!))))))
                ((same-operator? next ")")
                 (let* ((close (take-raw!))
                        (updated (append children
                                         (list (child 'close close)))))
                   (if (= depth 1)
                     (syntax-node 'ArithmeticCommand start (last-byte)
                                  updated)
                     (loop (fx- depth 1) updated))))
                ((eq? (token-kind next) 'word)
                 (loop depth
                       (append children
                               (list (child 'expression (take-word!))))))
                ((eq? (token-kind next) 'operator)
                 (loop depth
                       (append children
                               (list (child 'operator (take-raw!))))))
                (else (error "invalid arithmetic command token" next)))))))
       (def (parse-compound-redirections! command)
         (let loop ((children (list (child 'command command))))
           (let (next (peek))
             (cond
              ((descriptor-before-redirection?)
               (let (descriptor (take-raw!))
                 (loop (append children
                               (list (child 'redirect
                                            (parse-redirection!
                                             descriptor)))))))
              ((redirect? next)
               (loop (append children
                             (list (child 'redirect
                                          (parse-redirection! #f))))))
              ((null? (cdr children)) command)
              (else
               (syntax-node 'RedirectedCommand
                            (recognition-node-start command)
                            (last-byte) children))))))
       (def (parse-group! opening closing kind)
         (let* ((open (peek))
                (start (token-start open)))
           (take-raw!)
           (let* ((body (parse-compound-list! (last-byte) (list closing)))
                  (close (peek)))
             (unless (if (string=? closing ")")
                       (same-operator? close closing)
                       (word-is? close closing))
               (error "missing Bash group terminator" closing))
             (take-raw!)
             (syntax-node kind start (last-byte)
                          (list (child 'open open)
                                (child 'body body)
                                (child 'close close))))))
       (def (parse-command!)
         (let (first (peek))
           (let-values
               (((command compound?)
                 (cond
                  ((word-is? first "if") (values (parse-if!) #t))
                  ((or (word-is? first "while")
                       (word-is? first "until"))
                   (values (parse-loop!) #t))
                  ((or (word-is? first "for")
                       (word-is? first "select"))
                   (values (parse-iteration! (token-lexeme first)) #t))
                  ((word-is? first "case") (values (parse-case!) #t))
                  ((word-is? first "function")
                   (values (parse-function! #t) #t))
                  ((function-header?)
                   (values (parse-function! #f) #t))
                  ((word-is? first "[[")
                   (values (parse-conditional!) #t))
                  ((arithmetic-command-start?)
                   (values (parse-arithmetic-command!) #t))
                  ((word-is? first "{")
                   (values (parse-group! "{" "}" 'BraceGroup) #t))
                  ((same-operator? first "(")
                   (values (parse-group! "(" ")" 'Subshell) #t))
                  (else (values (parse-simple-command!) #f)))))
             (if compound?
               (parse-compound-redirections! command) command))))
       (def (parse-pipeline!)
         (let ((prefixes '())
               (start-token (peek)))
           (when (word-is? (peek) "time")
             (set! prefixes (list (child 'keyword (take-raw!))))
             (when (word-is? (peek) "-p")
               (set! prefixes
                     (append prefixes
                             (list (child 'option (take-raw!)))))))
           (when (word-is? (peek) "!")
             (set! prefixes
                   (append prefixes (list (child 'negate (take-raw!))))))
           (let* ((first (parse-command!))
                  (children
                   (append prefixes (list (child 'command first)))))
             (let loop ()
             (let (operator (peek))
               (if (or (same-operator? operator "|")
                       (same-operator? operator "|&"))
                 (begin
                   (take-raw!)
                   (set! children
                         (append children
                                 (list (child 'operator operator)
                                       (child 'command
                                              (parse-command!)))))
                   (loop))
                 (if (and (null? prefixes) (null? (cdr children))) first
                   (syntax-node 'Pipeline
                                (if (null? prefixes)
                                  (recognition-node-start first)
                                  (token-start start-token))
                                (last-byte) children))))))))
       (def (parse-and-or!)
         (let* ((first (parse-pipeline!))
                (children (list (child 'command first))))
           (let loop ()
             (let (operator (peek))
               (if (or (same-operator? operator "&&")
                       (same-operator? operator "||"))
                 (begin
                   (take-raw!)
                   (set! children
                         (append children
                                 (list (child 'operator operator)
                                       (child 'command (parse-pipeline!)))))
                   (loop))
                 (if (null? (cdr children)) first
                   (syntax-node 'AndOrList
                                (recognition-node-start first)
                                (last-byte) children)))))))
       (def (parse-here-documents!)
         (let loop ((nodes '()))
           (let (next (peek))
             (if (and next
                      (memq (token-kind next)
                            '(heredoc-content heredoc-end)))
               (let ((start (token-start next)) (children '()))
                 (unless (pair? pending-markers)
                   (error "here-document body has no redirection"))
                 (let body-loop ()
                   (let (line (peek))
                     (unless (and line
                                  (memq (token-kind line)
                                        '(heredoc-content heredoc-end)))
                       (error "unterminated Bash here-document body"))
                     (let (value
                           (if (or (eq? (token-kind line) 'heredoc-end)
                                   (cdr (car pending-markers)))
                             (take-raw!)
                             (begin
                               (set! remaining (cdr remaining))
                               (let-values
                                   (((node pieces)
                                     (shell-here-content-components line)))
                                 (for-each emit! pieces)
                                 node))))
                       (set! children
                             (cons (child
                                    (if (eq? (token-kind line) 'heredoc-end)
                                      'delimiter 'content)
                                    value)
                                   children)))
                     (unless (eq? (token-kind line) 'heredoc-end)
                       (body-loop))))
                 (set! links
                       (cons (make-shell-here-document-link
                              (car (car pending-markers)) start)
                             links))
                 (set! pending-markers (cdr pending-markers))
                 (loop (cons
                        (child 'here-document
                               (syntax-node 'HereDocument start (last-byte)
                                            (reverse children)))
                        nodes)))
               (reverse nodes)))))
       (def (terminator? token names)
         (and token
              (or (and (eq? (token-kind token) 'word)
                       (member (token-lexeme token) names))
                  (and (eq? (token-kind token) 'operator)
                       (member (token-lexeme token) names)))))
       (def (parse-list! terminators)
         (let loop ((children '()))
           (let (next (peek))
             (cond
              ((or (not next) (terminator? next terminators))
               (reverse children))
              ((separator? next)
               (when (and (eq? (token-kind next) 'operator)
                          (not (has-command? children)))
                 (error "Bash list separator has no command" next))
               (take-raw!)
               (let (with-separator (cons (child 'separator next) children))
                 (if (eq? (token-kind next) 'newline)
                   (loop (append (reverse (parse-here-documents!))
                                 with-separator))
                   (loop with-separator))))
              (else
               (loop (cons (child 'command (parse-and-or!)) children)))))))
       (let* ((children (parse-list! '()))
              (source-end (u8vector-length (string->utf8 source))))
         (when (pair? pending-markers)
           (error "missing Bash here-document body"))
         (let* ((root (syntax-node 'BashFile 0 source-end children))
                (artifact
                 (make-success-parse-artifact
                  grammar-digest source
                  (reverse emitted-reversed) root trivia?)))
           (values artifact (reverse links))))))))

(def (parse-shell-core source scan grammar-digest)
  (let-values (((artifact _links)
                (parse-shell-core/receipt source scan grammar-digest)))
    artifact))

    (values parse-shell-core parse-shell-core/receipt)))

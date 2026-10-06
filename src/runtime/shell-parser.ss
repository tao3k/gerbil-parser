;;; -*- Gerbil -*-
;;; Lossless Bash command/word/here-document syntax entry.
;;; Unsupported command forms reject explicitly until their grammar is owned.

(import (only-in ../language/command-profile command-plan-role-matcher command-plan-text? command-plan-kind command-plan-forms command-plan-match-form command-plan-trigger?)
        (only-in ./funcs recognition-sequence-append recognition-sequence->list recognition-sequence-arity)
        (only-in :gerbil-parser/src/runtime/artifact
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

(def (make-shell-parser regions results parts-plan commands)
  (let-values (((shell-word-components shell-assignment-components shell-here-content-components)
                (make-shell-word-parser regions results parts-plan)))


(def forms-index (make-hash-table-eq))
(for-each (lambda (row) (hash-put! forms-index (car row) row)) (command-plan-forms commands))
(def (form id) (or (hash-get forms-index id) (error "undeclared command form" id)))
(def header-first (cons 'or (map cadr (list-tail (form 'pipeline-prefix) 4))))
(def trivia? (command-plan-role-matcher commands 'trivia))
(def separator? (command-plan-role-matcher commands 'separator))
(def redirect? (command-plan-role-matcher commands 'redirect))
(def here-redirect? (command-plan-role-matcher commands 'here-redirect))
(def strip-redirect? (command-plan-role-matcher commands 'strip-redirect))
(def reserved? (command-plan-role-matcher commands 'reserved))
(def case-end? (command-plan-role-matcher commands 'case-end))
(def conditional-operator? (command-plan-role-matcher commands 'conditional-operator))
(def pipeline? (command-plan-role-matcher commands 'pipeline))
(def and-or? (command-plan-role-matcher commands 'and-or))
(def (child field value)
  (make-recognition-child field value))

(def (syntax-node kind start end children)
  (result-plan-node results (command-plan-kind commands kind) start end (recognition-sequence->list children)))

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
           (pending-markers '()) (pending-back '())
           (links '()))
       (def (marker-head)
         (when (null? pending-markers)
           (set! pending-markers (reverse pending-back)) (set! pending-back '()))
         (and (pair? pending-markers) (car pending-markers)))
       (def (enqueue-marker! marker) (set! pending-back (cons marker pending-back)))
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
                         (enqueue-marker!
                           (cons (token-start target)
                                 (delimiter-obligation-quoted?
                                  (decode-marker 'shell-quote-removal (token-lexeme target)
                                                 (strip-redirect? operator)))))
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
              (command-plan-text? commands 'descriptor (token-lexeme (car remaining)))
              (pair? (cdr remaining))
              (redirect? (cadr remaining))
              (= (token-end (car remaining))
                 (token-start (cadr remaining)))))
       (def (array-assignment-open? raw)
         (and (pair? remaining)
              (command-plan-trigger? commands (cadr (form 'array-tail)) remaining)
              (= (token-end raw) (token-start (car remaining)))))
       (def (parse-simple-command!)
         (let* ((first (peek))
                (start (and first (token-start first)))
                (named? #f)
                (children '()))
           (unless first (error "expected Bash command"))
           (when (reserved? first)
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
                                            (parse-form! 'array-tail (recognition-node-start assignment)
                                                         (list (child 'assignment assignment)))
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
       (def (matches? trigger)
         (peek)
         (command-plan-trigger? commands trigger remaining))
       (def (execute-program! program kind (initial '()))
         (let ((children initial) (projection kind))
           (def (publish! field value)
             (set! children (recognition-sequence-append children (list (child field value)))))
           (def (execute! body)
             (for-each (lambda (step)
               (case (car step)
                 ((as) (set! projection (cadr step)))
                 ((raw) (peek) (publish! (cadr step) (take-raw!)))
                 ((take)
                  (unless (matches? (caddr step)) (error "expected declared command token" (caddr step) (peek)))
                  (publish! (cadr step) (take-raw!)))
                 ((word) (publish! (cadr step) (take-word!)))
                 ((command) (publish! (cadr step) (parse-command!)))
                 ((call) (publish! (cadr step) (parse-form! (caddr step))))
                 ((node) (publish! (cadr step) (parse-program-node! (caddr step) (cdddr step))))
                 ((list)
                  (publish! (cadr step) (parse-compound-list! (last-byte) (caddr step) (cadddr step))))
                 ((optional) (when (matches? (cadr step)) (execute! (cddr step))))
                 ((branch) (execute! (if (matches? (cadr step)) (caddr step) (cadddr step))))
                 ((choose)
                  (let choices ((rows (cdr step)))
                    (cond ((null? rows) (error "no declared command branch accepts token" (peek)))
                          ((matches? (caar rows)) (execute! (cdar rows)))
                          (else (choices (cdr rows))))))
                 ((many until)
                  (let loop ()
                    (let (matched? (matches? (cadr step)))
                      (when (if (eq? (car step) 'many) matched? (not matched?))
                        (unless (pair? remaining) (error "unterminated declared command repetition" (cadr step)))
                        (let (before remaining)
                          (execute! (cddr step))
                          (when (eq? before remaining) (error "command program did not advance input")))
                        (loop)))))
                 ((balance)
                  (let loop ((depth (list-ref step 3)))
                    (unless (peek) (error "unterminated declared balanced command body"))
                    (cond
                     ((matches? (cadr step)) (publish! (list-ref step 4) (take-raw!)) (loop (+ depth 1)))
                     ((matches? (caddr step))
                      (publish! (list-ref step 5) (take-raw!)) (unless (= depth 1) (loop (- depth 1))))
                     (else
                      (let (before remaining)
                        (execute! (list-tail step 6))
                        (when (eq? before remaining) (error "balanced command program did not advance input")))
                      (loop depth))))))) body))
           (execute! program)
           (values projection children)))
       (def (parse-program-node! kind program (start #f) (initial '()))
         (let (beginning (or start (and (peek) (token-start (peek)))))
           (unless beginning (error "expected declared compound command" kind))
           (let-values (((projection children) (execute-program! program kind initial)))
             (syntax-node projection beginning (last-byte) children))))
       (def (parse-form! id (start #f) (initial '()))
         (let (row (form id))
           (parse-program-node! (cadddr row) (list-tail row 4) start initial)))
       (def (parse-compound-redirections! command)
         (let loop ((children (list (child 'command command))))
           (let (next (peek))
             (cond
              ((descriptor-before-redirection?)
               (let (descriptor (take-raw!))
                 (loop (recognition-sequence-append children
                               (list (child 'redirect
                                            (parse-redirection!
                                             descriptor)))))))
              ((redirect? next)
               (loop (recognition-sequence-append children
                             (list (child 'redirect
                                          (parse-redirection! #f))))))
              ((= (recognition-sequence-arity children) 1) command)
              (else
               (syntax-node 'RedirectedCommand
                            (recognition-node-start command)
                            (last-byte) children))))))
       (def (parse-command!)
         (peek)
         (let (id (command-plan-match-form commands remaining))
           (if id (parse-compound-redirections! (parse-form! id)) (parse-simple-command!))))
       (def (parse-pipeline!)
         (let ((prefixes '())
               (start-token (peek)))
           (when (matches? header-first)
             (let-values (((_projection children)
                           (execute-program! (list-tail (form 'pipeline-prefix) 4) 'Pipeline)))
               (set! prefixes (recognition-sequence->list children))))
           (let* ((first (parse-command!))
                  (children
                   (append prefixes (list (child 'command first)))))
             (let loop ()
             (let (operator (peek))
               (if (pipeline? operator)
                 (begin
                   (take-raw!)
                   (set! children
                         (recognition-sequence-append children
                                 (list (child 'operator operator)
                                       (child 'command
                                              (parse-command!)))))
                   (loop))
                 (if (and (null? prefixes) (= (recognition-sequence-arity children) 1)) first
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
               (if (and-or? operator)
                 (begin
                   (take-raw!)
                   (set! children
                         (recognition-sequence-append children
                                 (list (child 'operator operator)
                                       (child 'command (parse-pipeline!)))))
                   (loop))
                 (if (= (recognition-sequence-arity children) 1) first
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
                 (unless (marker-head)
                   (error "here-document body has no redirection"))
                 (let body-loop ()
                   (let (line (peek))
                     (unless (and line
                                  (memq (token-kind line)
                                        '(heredoc-content heredoc-end)))
                       (error "unterminated Bash here-document body"))
                     (let (value
                           (if (or (eq? (token-kind line) 'heredoc-end)
                                   (cdr (marker-head)))
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
                              (car (marker-head)) start)
                             links))
                 (set! pending-markers (cdr pending-markers))
                 (loop (cons
                        (child 'here-document
                               (syntax-node 'HereDocument start (last-byte)
                                            (reverse children)))
                        nodes)))
               (reverse nodes)))))
       (def (parse-list! terminators)
         (let loop ((children '()))
           (let (next (peek))
             (cond
              ((or (not next) (command-plan-trigger? commands terminators remaining))
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
       (let* ((children (parse-list! '(none)))
              (source-end (u8vector-length (string->utf8 source))))
         (when (marker-head)
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

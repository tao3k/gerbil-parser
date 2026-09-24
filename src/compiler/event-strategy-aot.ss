;;; -*- Gerbil -*-
;;; One pure Scheme line-event algorithm, executable here and lowered to Rust.

(import (only-in ../language/descriptor language-grammar-ir language-grammar-machine)
        (only-in ../runtime/identity sha256-text)
        (only-in ./machine parser-machine-grammar-digest)
        (only-in ./rust-syntax
                 rust-line-event-function rust-event-node rust-event-token
                 rust-event-if rust-method rust-identifier rust-string
                 rust-module write-rust-module))
(export define-line-event-parser event-node event-token line-starts-with?
        compile-line-event-parser generate-line-event-module)

(defrules event-node ()
  ((_ kind child ...)
   (append (list (list 'start 'kind)) child ... (list '(finish)))))

(defrules event-token ()
  ((_ kind start end)
   (list (list 'token 'kind start end))))

(def (line-starts-with? line prefix)
  (and (<= (string-length prefix) (string-length line))
       (string=? (substring line 0 (string-length prefix)) prefix)))

(def (source-line-events source root visit)
  (let (size (string-length source))
    (let loop ((start 0) (cursor 0) (byte-start 0)
               (reversed (list (list 'start root))))
      (cond
       ((= cursor size)
        (if (= start size)
          (reverse (cons '(finish) reversed))
          (let* ((line (substring source start size))
                 (byte-end (+ byte-start
                              (u8vector-length (string->utf8 line)))))
            (reverse (cons '(finish)
                           (append (reverse (visit line byte-start byte-end))
                                   reversed))))))
       ((or (char=? (string-ref source cursor) #\newline)
            (char=? (string-ref source cursor) #\return))
        (let* ((after (+ cursor
                         (if (and (char=? (string-ref source cursor) #\return)
                                  (< (+ cursor 1) size)
                                  (char=? (string-ref source (+ cursor 1))
                                          #\newline))
                           2 1)))
               (line (substring source start after))
               (byte-end (+ byte-start
                            (u8vector-length (string->utf8 line)))))
          (loop after after byte-end
                (append (reverse (visit line byte-start byte-end))
                        reversed))))
       (else (loop start (+ cursor 1) byte-start reversed))))))

(def (kind-index grammar name category)
  (let (kinds (cdr (assq 'syntax-kinds (language-grammar-ir grammar))))
    (let loop ((rest kinds) (index 0))
      (cond
       ((null? rest) (error "unknown event strategy syntax kind" name))
       ((eq? (caar rest) name)
        (unless (eq? (cadar rest) category)
          (error "wrong event strategy kind category" name category))
        index)
       (else (loop (cdr rest) (+ index 1)))))))

(def (compile-event-expression grammar expression bindings)
  (unless (pair? expression)
    (error "event strategy requires a structured expression" expression))
  (case (car expression)
    ((event-node)
     (unless (and (>= (length expression) 3) (symbol? (cadr expression)))
       (error "event node requires a kind and child" expression))
     (rust-event-node
      (kind-index grammar (cadr expression) 'node)
      (map (lambda (child) (compile-event-expression grammar child bindings))
           (cddr expression))))
    ((event-token)
     (unless (and (= (length expression) 4) (symbol? (cadr expression))
                  (memq (caddr expression) bindings)
                  (memq (cadddr expression) bindings))
       (error "event token requires bound byte offsets" expression))
     (rust-event-token
      (kind-index grammar (cadr expression) 'token)
      (rust-identifier (caddr expression))
      (rust-identifier (cadddr expression))))
    ((if)
     (unless (= (length expression) 4)
       (error "event strategy if requires two branches" expression))
     (rust-event-if
      (compile-event-condition (cadr expression) bindings)
      (compile-event-expression grammar (caddr expression) bindings)
      (compile-event-expression grammar (cadddr expression) bindings)))
    (else (error "unsupported event strategy expression" expression))))

(def (compile-event-condition condition bindings)
  (unless (and (list? condition) (= (length condition) 3)
               (eq? (car condition) 'line-starts-with?)
               (memq (cadr condition) bindings)
               (string? (caddr condition)))
    (error "unsupported event strategy condition" condition))
  (rust-method (rust-identifier (cadr condition))
               'starts_with (list (rust-string (caddr condition)))))

(def (compile-line-event-parser name grammar root bindings body)
  (unless (and (list? bindings) (= (length bindings) 3)
               (equal? bindings '(line start end)))
    (error "line event parser requires line/start/end bindings" bindings))
  (rust-line-event-function
   name (kind-index grammar root 'node)
   (compile-event-expression grammar body bindings)
   (sha256-text
    (call-with-output-string
     (lambda (port)
       (write (list 'line-event-parser.v1
                    (parser-machine-grammar-digest
                     (language-grammar-machine grammar))
                    root bindings body)
              port))))))

(defrules define-line-event-parser ()
  ((_ scheme-name rust-name grammar root source (line start end) body)
   (begin
     (def (scheme-name source)
       (source-line-events source 'root
                           (lambda (line start end) body)))
     (def rust-name
       (compile-line-event-parser 'rust-name grammar 'root
                                  '(line start end) 'body)))))

(def (generate-line-event-module output-path syntax)
  (write-rust-module
   output-path
   (rust-module '("TreeEvent") syntax "event-strategy-aot.ss")))

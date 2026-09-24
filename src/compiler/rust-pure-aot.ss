;;; -*- Gerbil -*-
;;; A deliberately bounded pure Scheme subset lowered to Rust function syntax.

(import (only-in :std/string/misc string-trim)
        (only-in ./rust-syntax
                 rust-function-value rust-block rust-let rust-method
                 rust-call
                 rust-string rust-identifier rust-before rust-first-word
                 rust-after rust-words rust-any rust-empty
                 rust-string-in rust-if rust-binary))
(export define-rust-pure scheme-pure->rust string-before ascii-ci=?
        string-after string-first-word string-words string-in?)

(def (string-before value delimiter)
  (let (index (string-contains value delimiter))
    (if (fixnum? index) (substring value 0 index) value)))

(def (string-after value delimiter)
  (let (index (string-contains value delimiter))
    (if (fixnum? index)
      (substring value (+ index (string-length delimiter))
                 (string-length value))
      "")))

(def (ascii-fold char)
  (let (code (char->integer char))
    (if (and (<= 65 code) (<= code 90))
      (integer->char (+ code 32))
      char)))

(def (ascii-ci=? left right)
  (and (= (string-length left) (string-length right))
       (let loop ((index 0))
         (or (= index (string-length left))
             (and (char=? (ascii-fold (string-ref left index))
                          (ascii-fold (string-ref right index)))
                  (loop (+ index 1)))))))

(def (string-first-word value)
  (let* ((text (string-trim value)) (size (string-length text)))
    (let loop ((index 0))
      (if (or (= index size)
              (char-whitespace? (string-ref text index)))
        (substring text 0 index)
        (loop (+ index 1))))))

(def (string-words value)
  (let (size (string-length value))
    (let loop ((index 0) (start #f) (words '()))
      (if (= index size)
        (reverse (if start (cons (substring value start size) words) words))
        (if (char-whitespace? (string-ref value index))
          (loop (+ index 1) #f
                (if start (cons (substring value start index) words) words))
          (loop (+ index 1) (or start index) words))))))

(def (string-in? value collection)
  (if (member value collection) #t #f))

(def (rust-name-text name)
  (apply string-append
         (map (lambda (char)
                (case char
                  ((#\-) "_")
                  ((#\?) "_p")
                  (else (string char))))
              (string->list (symbol->string name)))))

(def current-pure-calls (make-parameter '()))

(def (pure-call-signature expression)
  (and (pair? expression) (symbol? (car expression))
       (assq (car expression) (current-pure-calls))))

(def (compile-pure-expression expression variables result-type)
  (cond
   ((symbol? expression)
    (unless (assq expression variables)
      (error "unbound pure AOT variable" expression))
    (rust-identifier (rust-name-text expression)))
   ((string? expression) (rust-string expression))
   ((pure-call-signature expression)
    (let* ((signature (pure-call-signature expression))
           (argument-types (cdr signature))
           (arguments (cdr expression)))
      (unless (= (length arguments) (length argument-types))
        (error "pure AOT call has wrong arity" expression))
      (rust-call
       (rust-identifier (rust-name-text (car expression)))
       (map (lambda (argument type)
              (compile-pure-expression argument variables type))
            arguments argument-types))))
   ((and (pair? expression) (eq? (car expression) 'string-trim)
         (= (length expression) 2))
    (let (trimmed
          (rust-method
           (compile-pure-expression (cadr expression) variables "&str")
           'trim '()))
      (if (equal? result-type "&str")
        trimmed
        (rust-method trimmed 'to_owned '()))))
   ((and (pair? expression) (eq? (car expression) 'string-before)
         (= (length expression) 3))
    (rust-before
     (compile-pure-expression (cadr expression) variables "&str")
     (compile-pure-expression (caddr expression) variables "&str")
     (not (equal? result-type "&str"))))
   ((and (pair? expression) (eq? (car expression) 'string-after)
         (= (length expression) 3))
    (rust-after
     (compile-pure-expression (cadr expression) variables "&str")
     (compile-pure-expression (caddr expression) variables "&str")))
   ((and (pair? expression) (eq? (car expression) 'ascii-ci=?)
         (= (length expression) 3))
    (rust-method
     (compile-pure-expression (cadr expression) variables "&str")
     'eq_ignore_ascii_case
     (list (compile-pure-expression (caddr expression) variables "&str"))))
   ((and (pair? expression) (eq? (car expression) 'string-first-word)
         (= (length expression) 2))
    (rust-first-word
     (compile-pure-expression (cadr expression) variables "&str")))
   ((and (pair? expression) (eq? (car expression) 'string-words)
         (= (length expression) 2))
    (rust-words
     (compile-pure-expression (cadr expression) variables "&str")))
   ((and (pair? expression) (eq? (car expression) 'null?)
         (= (length expression) 2))
    (rust-empty
     (compile-pure-expression (cadr expression) variables "&[&str]")))
   ((and (pair? expression) (eq? (car expression) 'equal?)
         (= (length expression) 3))
    (cond
     ((equal? (caddr expression) "")
      (rust-empty
       (compile-pure-expression (cadr expression) variables "&str")))
     ((equal? (cadr expression) "")
      (rust-empty
       (compile-pure-expression (caddr expression) variables "&str")))
     (else
      (rust-binary "=="
       (compile-pure-expression (cadr expression) variables "&str")
       (compile-pure-expression (caddr expression) variables "&str")))))
   ((and (pair? expression) (eq? (car expression) 'ormap)
         (= (length expression) 3))
    (let* ((abstraction (cadr expression))
           (collection (caddr expression))
           (slice? (symbol? collection)))
      (unless (and (pair? abstraction) (eq? (car abstraction) 'lambda)
                   (= (length abstraction) 3)
                   (list? (cadr abstraction))
                   (= (length (cadr abstraction)) 1)
                   (symbol? (caadr abstraction)))
        (error "pure AOT ormap requires one-argument lambda" expression))
      (when (and slice?
                 (not (equal? (cdr (assq collection variables))
                              "&[String]")))
        (error "pure AOT ormap requires a String slice" collection))
      (rust-any
       (compile-pure-expression collection variables "&[&str]")
       (rust-name-text (caadr abstraction))
       (compile-pure-expression (caddr abstraction)
                                (cons (cons (caadr abstraction) "&str")
                                      variables) "bool")
       slice?)))
   ((and (pair? expression) (eq? (car expression) 'let*))
    (compile-pure-body expression variables result-type))
   ((and (pair? expression) (eq? (car expression) 'string-in?)
         (= (length expression) 3))
    (rust-string-in
     (compile-pure-expression (cadr expression) variables "&str")
     (compile-pure-expression (caddr expression) variables "&[&str]")))
   ((and (pair? expression) (eq? (car expression) 'if)
         (= (length expression) 4))
    (rust-if
     (compile-pure-expression (cadr expression) variables "bool")
     (compile-pure-expression (caddr expression) variables result-type)
     (compile-pure-expression (cadddr expression) variables result-type)))
   ((and (pair? expression) (memq (car expression) '(or and))
         (>= (length expression) 3))
    (let ((operator (if (eq? (car expression) 'or) "||" "&&"))
          (parts (map (lambda (part)
                        (compile-pure-expression part variables "bool"))
                      (cdr expression))))
      (let loop ((result (car parts)) (remaining (cdr parts)))
        (if (null? remaining) result
          (loop (rust-binary operator result (car remaining))
                (cdr remaining))))))
   (else (error "unsupported pure AOT expression" expression))))

(def (compile-pure-body expression variables result-type)
  (if (and (pair? expression) (eq? (car expression) 'let*))
    (let loop ((bindings (cadr expression)) (known variables)
               (statements '()))
      (if (null? bindings)
        (rust-block (reverse statements)
                    (compile-pure-expression (caddr expression) known
                                             result-type))
        (let* ((binding (car bindings))
               (name (car binding))
               (value (cadr binding)))
          (unless (and (symbol? name) (not (assq name known)))
            (error "invalid pure AOT binding" binding))
          (loop (cdr bindings) (cons (cons name "&str") known)
                (cons (rust-let (rust-name-text name)
                                (compile-pure-expression value known "&str"))
                      statements)))))
    (rust-block '()
                (compile-pure-expression expression variables result-type))))

(def (scheme-pure->rust name parameters result expression
                        (known-calls '()))
  (unless (and (list? known-calls)
               (let loop ((rest known-calls) (seen '()))
                 (or (null? rest)
                     (let (signature (car rest))
                       (and (pair? signature)
                            (symbol? (car signature))
                            (not (member (car signature) seen))
                            (list? (cdr signature))
                            (andmap string? (cdr signature))
                            (loop (cdr rest)
                                  (cons (car signature) seen)))))))
    (error "pure AOT requires distinct typed function calls" known-calls))
  (parameterize ((current-pure-calls known-calls))
    (rust-function-value
     (string->symbol (rust-name-text name))
     parameters result
     (compile-pure-body expression parameters result))))

;; One source body is both executable Scheme and the AOT input. The compiler
;; admits only expressions handled above; arbitrary Gerbil forms fail closed.
(defrules define-rust-pure (using)
  ((_ scheme-name rust-name ((argument type) ...) result
      (using ((callee call-type ...) ...) body))
   (begin
     (def (scheme-name argument ...) body)
     (def rust-name
       (scheme-pure->rust 'scheme-name
                          (list (cons 'argument type) ...)
                          result 'body
                          (list (cons 'callee (list call-type ...)) ...)))))
  ((_ scheme-name rust-name ((argument type) ...) result body)
   (begin
     (def (scheme-name argument ...) body)
     (def rust-name
       (scheme-pure->rust 'scheme-name
                          (list (cons 'argument type) ...)
                          result 'body)))))

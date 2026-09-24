;;; -*- Gerbil -*-
;;; A deliberately bounded pure Scheme subset lowered to Rust function syntax.

(import (only-in ./rust-syntax
                 rust-function-value rust-block rust-let rust-method
                 rust-string rust-identifier rust-before rust-binary))
(export define-rust-pure scheme-pure->rust string-before ascii-ci=?)

(def (string-before value delimiter)
  (let (index (string-contains value delimiter))
    (if index (substring value 0 index) value)))

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

(def (compile-pure-expression expression variables result-type)
  (cond
   ((symbol? expression)
    (unless (member expression variables)
      (error "unbound pure AOT variable" expression))
    (rust-identifier (symbol->string expression)))
   ((string? expression) (rust-string expression))
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
   ((and (pair? expression) (eq? (car expression) 'ascii-ci=?)
         (= (length expression) 3))
    (rust-method
     (compile-pure-expression (cadr expression) variables "&str")
     'eq_ignore_ascii_case
     (list (compile-pure-expression (caddr expression) variables "&str"))))
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
          (unless (and (symbol? name) (not (member name known)))
            (error "invalid pure AOT binding" binding))
          (loop (cdr bindings) (cons name known)
                (cons (rust-let name
                                (compile-pure-expression value known "String"))
                      statements)))))
    (rust-block '()
                (compile-pure-expression expression variables result-type))))

(def (scheme-pure->rust name parameters result expression)
  (rust-function-value
   (string->symbol
    (apply string-append
           (map (lambda (char)
                  (case char
                    ((#\-) "_")
                    ((#\?) "_p")
                    (else (string char))))
                (string->list (symbol->string name)))))
   parameters result
   (compile-pure-body expression (map car parameters) result)))

;; One source body is both executable Scheme and the AOT input. The compiler
;; admits only expressions handled above; arbitrary Gerbil forms fail closed.
(defrules define-rust-pure ()
  ((_ scheme-name rust-name ((argument type) ...) result body)
   (begin
     (def (scheme-name argument ...) body)
     (def rust-name
       (scheme-pure->rust 'scheme-name
                          (list (cons 'argument type) ...)
                          result 'body)))))

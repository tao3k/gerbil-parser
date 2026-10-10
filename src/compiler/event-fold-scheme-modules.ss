;;; -*- Gerbil -*-
;;; Bound native compilation units without changing the admitted strategy.
(import (only-in ./event-fold-scheme event-fold-scheme-source))
(export event-fold-scheme-module-sources)

(def (private-definition? form)
  (and (pair? form) (eq? (car form) 'def) (pair? (cadr form))
       (let (name (symbol->string (caadr form)))
         (and (>= (string-length name) 12)
              (string=? (substring name 0 12) "%event-fold-")))))

(def (references form directory)
  (let ((seen (make-table test: eq?)) (names '()))
    (def (walk value)
      (cond
       ((symbol? value)
        (when (and (table-ref directory value #f) (not (table-ref seen value #f)))
          (table-set! seen value #t)
          (set! names (cons value names))))
       ((and (pair? value) (not (eq? (car value) 'quote)))
        (walk (car value)) (walk (cdr value)))))
    (walk form)
    (reverse names)))

(def (ordered-definitions definitions directory)
  (let ((active (make-table test: eq?)) (done (make-table test: eq?)) (ordered '()))
    (def (visit name)
      (when (table-ref active name #f) (error "cyclic native fold procedure graph" name))
      (unless (table-ref done name #f)
        (let (form (table-ref directory name))
          (table-set! active name #t)
          ;; Only bodies contain references; the definition's own name is a binder.
          (for-each visit (references (cddr form) directory))
          (table-set! active name)
          (table-set! done name #t)
          (set! ordered (cons form ordered)))))
    (for-each (lambda (form) (visit (caadr form))) definitions)
    (reverse ordered)))

(def (chunks definitions width)
  (let loop ((rest definitions) (part '()) (count 0) (parts '()))
    (cond
     ((null? rest) (reverse (if (null? part) parts (cons (reverse part) parts))))
     ((= count width) (loop rest '() 0 (cons (reverse part) parts)))
     (else (loop (cdr rest) (cons (car rest) part) (+ count 1) parts)))))

(def (forms-source forms)
  (call-with-output-string
   (lambda (port)
     (display ";;; Engine-owned bounded native EventFold unit.\n" port)
     (for-each (lambda (form) (write form port) (newline port)) forms))))

(def (safe-component? value)
  (and (> (string-length value) 0)
       (every (lambda (char)
                (or (char<=? #\a char #\z) (char<=? #\A char #\Z)
                    (char<=? #\0 char #\9) (memv char '(#\- #\_))))
              (string->list value))))

(def (event-fold-scheme-module-sources name grammar root initial line finish
                                      (helpers '()) (parameters '()) (width 64))
  (unless (and (symbol? name) (safe-component? (symbol->string name))
               (exact-integer? width) (<= 1 width 64))
    (error "invalid native fold compilation namespace or unit width"))
  (let* ((source (event-fold-scheme-source name grammar root initial line finish helpers parameters))
         (forms (call-with-input-string source
                  (lambda (port)
                    (let read-forms ((forms '()))
                      (let (form (read port))
                        (if (eof-object? form) (reverse forms)
                          (read-forms (cons form forms))))))))
         (definitions (filter private-definition? forms))
         (directory (make-table test: eq?))
         (owners (make-table test: eq?))
         (base (symbol->string name)))
    (for-each (lambda (form) (table-set! directory (caadr form) form)) definitions)
    (let* ((parts (chunks (ordered-definitions definitions directory) width))
           (names (map (lambda (ordinal) (string-append base "-part-" (number->string ordinal)))
                       (iota (length parts)))))
      (for-each
       (lambda (part module)
         (for-each (lambda (form) (table-set! owners (caadr form) module)) part))
       parts names)
      (def (imports body current)
        (let ((groups (make-table test: equal?)) (order '()))
          (for-each
           (lambda (ref)
             (let (owner (table-ref owners ref))
               (unless (equal? current owner)
                 (unless (table-ref groups owner #f)
                   (set! order (cons owner order))
                   (table-set! groups owner '()))
                 (table-set! groups owner (cons ref (table-ref groups owner))))))
           (references body directory))
          (map (lambda (owner)
                 `(only-in ,(string-append "./" owner ".ss")
                           ,@(reverse (table-ref groups owner))))
               (reverse order))))
      (def (import-forms body current)
        (let (specifications (imports body current))
          (if (null? specifications) '() (list `(import ,@specifications)))))
      (append
       (map
        (lambda (part module)
          (cons module
                (forms-source
                 `(,(car forms)
                   ,@(import-forms (map cddr part) module)
                   (export ,@(map caadr part))
                   ,@part))))
        parts names)
       (let (public (filter (lambda (form) (not (private-definition? form))) (cdr forms)))
         (list (cons base
                     (forms-source
                      `(,(car forms) ,@(import-forms public #f) ,@public)))))))))

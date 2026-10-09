;;; -*- Gerbil -*-
;;; Build-local interning of code, never memoization of request values.
(export current-fold-scheme-counter current-fold-scheme-prefix
        current-fold-scheme-expressions current-fold-scheme-definitions
        fold-scheme-fresh-name fold-scheme-share-expression fold-scheme-share-statements)

(def current-fold-scheme-counter (make-parameter 0))
(def current-fold-scheme-prefix (make-parameter 'native))
(def current-fold-scheme-expressions (make-parameter #f))
(def current-fold-scheme-definitions (make-parameter '()))

(def (fold-scheme-fresh-name prefix)
  (let (ordinal (current-fold-scheme-counter))
    (current-fold-scheme-counter (+ ordinal 1))
    (string->symbol
     (string-append "%event-fold-" (symbol->string (current-fold-scheme-prefix))
                    "-" (symbol->string prefix) "-" (number->string ordinal)))))

(def (contains-variable? code name)
  (cond ((eq? code name) #t)
        ((not (pair? code)) #f)
        ((eq? (car code) 'quote) #f)
        (else (or (contains-variable? (car code) name)
                  (contains-variable? (cdr code) name)))))

(def (rename-variables code names)
  (cond
   ((symbol? code) (let (entry (assq code names)) (if entry (cdr entry) code)))
   ((not (pair? code)) code)
   ((eq? (car code) 'quote) code)
   (else (cons (rename-variables (car code) names)
               (rename-variables (cdr code) names)))))

(def (substantial-code? code)
  ;; A bounded walk avoids traversing large constants or counting entire trees.
  (let loop ((pending (list code)) (remaining 24))
    (cond ((<= remaining 0) #t)
          ((null? pending) #f)
          (else
           (let (node (car pending))
             (if (and (pair? node) (not (eq? (car node) 'quote)))
               (loop (cons (car node) (cons (cdr node) (cdr pending))) (- remaining 1))
               (loop (cdr pending) (- remaining 1))))))))

(def (writes-variable? code name)
  (and (pair? code) (not (eq? (car code) 'quote))
       (or (and (eq? (car code) 'set!) (eq? (cadr code) name))
           (writes-variable? (car code) name)
           (writes-variable? (cdr code) name))))

(def (share-code code bindings indices statements?)
  (let (cache (current-fold-scheme-expressions))
    (if (and cache (substantial-code? code))
      (let* ((free
              (filter (lambda (name) (contains-variable? code name))
                      (map cdr (filter (lambda (entry) (symbol? (cdr entry)))
                                      (append bindings indices)))))
             (formals
              (map (lambda (index)
                     (string->symbol (string-append "%fold-free-" (number->string index))))
                   (iota (length free))))
             (normalized (rename-variables code (map cons free formals)))
             (key (list statements? formals normalized)))
        ;; A mutable join temporary stays in its lexical owner. Base slots
        ;; are vector cells shared explicitly, not free-variable mutation.
        (if (and statements? (any (lambda (name) (writes-variable? code name)) free))
          code
          (let (name (or (table-ref cache key #f)
                       (let (name (fold-scheme-fresh-name 'expression))
                         (table-set! cache key name)
                         (current-fold-scheme-definitions
                          (cons `(def (,name source-bytes line start end slots
                                            ,@(if statements? '(events) '()) ,@formals)
                                   ,normalized)
                                (current-fold-scheme-definitions)))
                         name)))
            `(,name source-bytes line start end slots
                    ,@(if statements? '(events) '()) ,@free))))
      code)))

(def (fold-scheme-share-expression code bindings indices)
  (share-code code bindings indices #f))

(def (fold-scheme-share-statements statements bindings indices)
  (let* ((code `(begin ,@statements events))
         (shared (share-code code bindings indices #t)))
    (if (eq? code shared) statements
      (list `(set! events ,shared)))))

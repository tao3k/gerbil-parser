;;; -*- Gerbil -*-
;;; Build-local interning of code, never memoization of request values.
(export current-fold-scheme-counter current-fold-scheme-prefix
        current-fold-scheme-expressions current-fold-scheme-definitions
        current-fold-scheme-diagnostics current-fold-scheme-idle-plan fold-scheme-observed
        current-fold-scheme-tape-kinds current-fold-scheme-effect-only fold-scheme-event
        fold-scheme-fresh-name fold-scheme-share-expression fold-scheme-share-statements)

(def current-fold-scheme-counter (make-parameter 0))
(def current-fold-scheme-prefix (make-parameter 'native))
(def current-fold-scheme-expressions (make-parameter #f))
(def current-fold-scheme-definitions (make-parameter '()))
(def current-fold-scheme-diagnostics (make-parameter #f))
(def current-fold-scheme-idle-plan (make-parameter #f))
(def current-fold-scheme-tape-kinds (make-parameter #f))
(def current-fold-scheme-effect-only (make-parameter #f))
(def (fold-scheme-event tag (kind #f) (from 0) (until 0))
  (let (kinds (current-fold-scheme-tape-kinds))
    (if kinds
      (case tag
        ((finish) '(fold-tape-finish! events))
        (else
         (let (entry (assq kind kinds))
           (unless entry (error "missing compiled Fold kind" kind))
           (if (eq? tag 'start) `(fold-tape-start! events ,(cadr entry))
             `(fold-tape-token! events ,(cadr entry) ,from ,until)))))
      (case tag
        ((finish) '(set! events (cons '(finish) events)))
        ((start) `(set! events (cons '(start ,kind) events)))
        ((token) `(set! events (cons (list 'token ',kind ,from ,until) events)))))))
(def (fold-scheme-observed name code)
  (case (current-fold-scheme-diagnostics)
    ((counts) `(begin (fold-cost-count! ',name) ,code))
    ((#f) code)
    (else `(call-with-fold-cost ',name (lambda () ,code)))))

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
                          (cons `(def (,name source-bytes start end slots
                                            ,@(if statements? '(events) '()) ,@formals)
                                   ,(fold-scheme-observed name normalized))
                                (current-fold-scheme-definitions)))
                         name)))
            `(,name source-bytes start end slots
                    ,@(if statements? '(events) '()) ,@free))))
      code)))

(def (fold-scheme-share-expression code bindings indices)
  (share-code code bindings indices #f))

(def (fold-scheme-share-statements statements bindings indices)
  ;; A direct tape is a private mutable owner. Internal transitions need no
  ;; owner-valued result: publication and the outer source fold retain it.
  ;; Keep list-producing Fold's result threading unchanged.
  (let* ((effect-only? (and (current-fold-scheme-tape-kinds)
                           (current-fold-scheme-effect-only)))
         (code (if effect-only? `(begin ,@statements) `(begin ,@statements events)))
         (shared (share-code code bindings indices #t)))
    (if (eq? code shared) statements
      (list (if effect-only? shared `(set! events ,shared))))))

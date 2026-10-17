;;; -*- Gerbil -*-
;;; Compiler-side resolution of contextual declaration methods. The output is
;;; closed data; parsing never consults POO objects or method procedures.

(import (only-in ../modules/parser/contextual-objects
                 contextual-role? contextual-role-ref
                 contextual-method-name contextual-method-mode
                 contextual-method-position contextual-method-form
                 contextual-method-result)
        (only-in ../runtime/identity sha256-text))
(export compile-contextual-dispatch contextual-dispatch-ref)

(def +contextual-dispatch-schema+ "gerbil-parser.contextual-dispatch.v1")
(defstruct contextual-candidate (origin name mode position form result)
  transparent: #t)

(def (contextual-dispatch-ref ir key)
  (let (row (assq key ir))
    (and row (cdr row))))

(def (catalog? values)
  (and (pair? values)
       (list? values)
       (andmap symbol? values)
       (not (memq 'any values))
       (let loop ((rest values) (seen '()))
         (or (null? rest)
             (and (not (memq (car rest) seen))
                  (loop (cdr rest) (cons (car rest) seen)))))))

(def (role-candidate role method)
  (make-contextual-candidate
   (contextual-role-ref role 'name)
   (contextual-method-name method)
   (contextual-method-mode method)
   (contextual-method-position method)
   (contextual-method-form method)
   (contextual-method-result method)))

(def (candidate-row candidate)
  (list (contextual-candidate-origin candidate)
        (contextual-candidate-name candidate)
        (contextual-candidate-mode candidate)
        (contextual-candidate-position candidate)
        (contextual-candidate-form candidate)
        (contextual-candidate-result candidate)))

(def (candidate-origin candidate)
  (list (contextual-candidate-origin candidate)
        (contextual-candidate-name candidate)))

(def (known-selector? selector catalog)
  (or (eq? selector 'any) (memq selector catalog)))

(def (matches? selector actual)
  (or (eq? selector 'any) (eq? selector actual)))

(def (method-matches? candidate mode position form)
  (and (matches? (contextual-candidate-mode candidate) mode)
       (matches? (contextual-candidate-position candidate) position)
       (matches? (contextual-candidate-form candidate) form)))

(def (selector-at-least-as-specific? left right)
  (or (eq? left right) (eq? right 'any)))

(def (dominates? left right)
  (and
   (selector-at-least-as-specific?
    (contextual-candidate-mode left) (contextual-candidate-mode right))
   (selector-at-least-as-specific?
    (contextual-candidate-position left) (contextual-candidate-position right))
   (selector-at-least-as-specific?
    (contextual-candidate-form left) (contextual-candidate-form right))
   (or (not (eq? (contextual-candidate-mode left)
                 (contextual-candidate-mode right)))
       (not (eq? (contextual-candidate-position left)
                 (contextual-candidate-position right)))
       (not (eq? (contextual-candidate-form left)
                 (contextual-candidate-form right))))))

(def (maximal-methods matching)
  (filter
   (lambda (row)
     (not (any (lambda (other) (dominates? other row)) matching)))
   matching))

(def (resolve-cell rows mode position form)
  (let* ((matching
          (filter (lambda (row) (method-matches? row mode position form)) rows))
         (maximal (maximal-methods matching)))
    (if (null? maximal)
      #f
      (let (result (contextual-candidate-result (car maximal)))
        (unless (andmap (lambda (candidate)
                          (eq? (contextual-candidate-result candidate) result))
                        maximal)
          (error "ambiguous contextual dispatch"
                 (list mode position form) (map candidate-row maximal)))
        (list result (map candidate-origin maximal))))))

;;; All resolution happens here. A method tie with the same result retains
;;; both origins in the receipt; a tie with different results is an error.
(def (compile-contextual-dispatch roles modes positions forms)
  (unless (and (list? roles) (andmap contextual-role? roles)
               (catalog? modes) (catalog? positions) (catalog? forms))
    (error "invalid contextual dispatch declaration"))
  (let (candidates
        (apply append
               (map (lambda (role)
                      (map (lambda (method) (role-candidate role method))
                           (contextual-role-ref role 'methods)))
                    roles)))
    (for-each
     (lambda (candidate)
       (unless (and (symbol? (contextual-candidate-name candidate))
                    (symbol? (contextual-candidate-mode candidate))
                    (symbol? (contextual-candidate-position candidate))
                    (symbol? (contextual-candidate-form candidate))
                    (symbol? (contextual-candidate-result candidate))
                    (known-selector? (contextual-candidate-mode candidate) modes)
                    (known-selector? (contextual-candidate-position candidate)
                                     positions)
                    (known-selector? (contextual-candidate-form candidate) forms))
         (error "unknown contextual dispatch selector"
                (candidate-row candidate))))
     candidates)
    (let* ((cells
            (apply append
                   (map
                    (lambda (mode)
                      (apply append
                             (map
                              (lambda (position)
                                (map
                                 (lambda (form)
                                   (list mode position form
                                         (resolve-cell candidates mode position
                                                       form)))
                                 forms))
                              positions)))
                    modes)))
           (body
            (list (cons 'schema +contextual-dispatch-schema+)
                  (cons 'modes modes)
                  (cons 'positions positions)
                  (cons 'forms forms)
                  (cons 'methods (map candidate-row candidates))
                  (cons 'cells cells)))
           (digest
            (sha256-text
             (call-with-output-string
              (lambda (port) (write body port))))))
      (append body (list (cons 'digest digest))))))

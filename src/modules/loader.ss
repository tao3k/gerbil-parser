;;; Closed SDK loader programs and evaluator-safe replay for package products.
;;; SDK cache, errors and native implementation selection remain authoritative.
(import (only-in :gerbil/runtime/loader __find-library-module)
        (only-in :gerbil/runtime/init __scheme-source))
(export compiled-dependencies compiled-wrapper-forms read-compiled-dependencies
        sdk-module-ready? settled-source-wrapper? owned-source-wrapper?
        load-ready-source-wrapper load-parser-module)

(def (compiled-dependencies value (native? #f))
  ;; Recognize an executable loader language, never search arbitrary Scheme
  ;; data. Unsupported siblings reject the entire plan before any effect runs.
  (cond ((and (list? value) (= (length value) 2)
              (eq? (car value) 'load-module) (string? (cadr value)))
         (list (cadr value)))
        ((and (pair? value) (eq? (car value) 'begin) (list? (cdr value)))
         (compiled-wrapper-forms (cdr value) native?))
        ((and native? (equal? value '(declare (block) (standard-bindings) (extended-bindings)))) '())
        (else #f)))

(def (compiled-wrapper-forms forms native?)
  (let loop ((rest forms) (reversed '()))
    (if (null? rest) (reverse reversed)
      (let (dependencies (compiled-dependencies (car rest) native?))
        (and dependencies
             (loop (cdr rest) (foldl cons reversed dependencies)))))))

(def (read-compiled-dependencies source (native? #f))
  ;; Inspection failure means SDK delegation, not successful admission. Always
  ;; call load-module afterward so its native selection, errors and state win.
  (with-catch (lambda (_) #f)
    (lambda ()
      (call-with-input-file source
        (lambda (port)
          (let loop ((reversed '()))
            (let (value (read port))
              (if (eof-object? value) (reverse reversed)
                (let (dependencies (compiled-dependencies value native?))
                  (and dependencies
                       (loop (foldl cons reversed dependencies))))))))))))

(def (sdk-module-ready? module)
  (let (entry (assoc module (list-modules)))
    (and entry (or (eq? (cdr entry) 'builtin) (string? (cdr entry))))))

(def (owned-source-wrapper? module selected)
  ;; Only admitted package products with a compiler interface qualify. The
  ;; caller's publication reader owns byte identity for the entire operation.
  (and (string-prefix? "gerbil-parser/" module)
       (not (string-contains module "~"))
       (string-suffix? ".scm" selected)
       (equal? (path-normalize selected)
               (path-normalize (path-expand (string-append module ".scm")
                                            (path-expand "lib" (gerbil-path)))))
       (file-exists? (string-append (path-strip-extension selected) ".ssi"))))

(def (settled-source-wrapper? dependencies)
  ;; Only a completed success prefix or its first cached failure may replay
  ;; in another evaluator. A cold/loading request may run fresh code.
  (let loop ((rest dependencies))
    (or (null? rest)
        (let (entry (assoc (car rest) (list-modules)))
          (and entry
               (let (state (cdr entry))
                 (cond ((or (eq? state 'builtin) (string? state)) (loop (cdr rest)))
                       ((and (pair? state) (eq? (car state) 'error)) #t)
                       (else #f))))))))

(def (load-ready-source-wrapper module selected dependencies)
  ;; Match the SDK's raw Scheme source mode, retaining its module cache.
  ;; Never let a fresh initializer inherit this evaluator switch.
  (unless (settled-source-wrapper? dependencies)
    (error "source wrapper dependencies are not settled" module))
  (parameterize ((__scheme-source selected)) (load-module module)))

(def (load-parser-module module (visiting #f) (observe (lambda (_phase _module) (void))))
  ;; SDK states remain authoritative, including loading and failed states.
  (let ((visiting (or visiting (make-table test: equal?))) (source-plan #f))
    (unless (table-ref visiting module #f)
      (table-set! visiting module #t)
      (unless (or (assoc module (list-modules)) (string-contains module "~"))
        (let (selected (__find-library-module module))
          (when selected
            (let* ((native? (not (string-suffix? ".scm" selected)))
                   (owned? (and (not native?) (owned-source-wrapper? module selected)))
                   (source (if native? (string-append (path-strip-extension selected) ".scm") selected))
                   (dependencies (read-compiled-dependencies source (or native? owned?))))
              (when dependencies
                (with-catch void
                  (lambda ()
                    (for-each (lambda (dependency) (load-parser-module dependency visiting observe))
                              dependencies)))
                (when owned? (set! source-plan (cons selected dependencies))))))))
      (observe 'loading module)
      (if source-plan
        (load-ready-source-wrapper module (car source-plan) (cdr source-plan))
        (load-module module))
      (observe 'loaded module))))

;;; Build input selects one descriptor; multi-scope packs require an export name.
(import :gerbil/expander
        (only-in ./descriptor language-grammar?)
        (only-in :yuanxu/module-system/source/admission
                 module-source-read module-source-admit-current!
                 call-with-module-source-fresh-reads))
(export language-module-descriptor)
;; Last attempted bytes select official reload only. This table stores no
;; context, descriptor, loaded-state, exception or admission verdict.
(def source-observations (make-hash-table))
;; File inputs re-admit changed content, including same-size/same-time edits.
;; Library inputs use the SDK snapshot; explicit reload is not recursive.
(def (language-module-descriptor input reload?: (reload? #f))
 ;; Native executables initialize the runtime, while gxi additionally initializes
 ;; the expander. Use Gerbil's idempotent entry before any dynamic source import.
 (gerbil-load-expander!)
 (call-with-module-source-fresh-reads (lambda ()
 (unless (and (string? input) (positive? (string-length input)))
  (error "language build input requires a module path" input))
 (let* ((split (let loop ((at 0))
                (and (< at (string-length input))
                     (if (char=? (string-ref input at) #\#) at (loop (+ at 1))))))
        (path (if split (substring input 0 split) input))
        (name (and split (substring input (+ split 1) (string-length input)))))
  (when (or (zero? (string-length path)) (and name (zero? (string-length name))))
   (error "invalid language descriptor selector" input))
  (let* ((file? (not (char=? (string-ref path 0) #\:)))
         (canonical (and file? (path-normalize (path-expand path))))
         (observed (and file? (cons canonical (module-source-read canonical))))
         (changed? (and observed (not (equal? observed (hash-get source-observations canonical)))))
         (context
          (begin
            ;; Core rechecks bytes across effects. The native expander owns
            ;; source promises and their failure/retry behavior.
            (when observed
              (module-source-admit-current! (list observed))
              (hash-put! source-observations canonical observed))
            (import-module
             (if (char=? (string-ref path 0) #\:) (string->symbol path) path)
             (or reload? changed?) #t)))
         (descriptors
          (filter-map
           (lambda (exported)
             (and (= (module-export-phi exported) 0)
                  (or (not name) (eq? (module-export-name exported) (string->symbol name)))
                  (let (value (eval (binding-id (core-resolve-module-export exported))))
                    (and (language-grammar? value) value))))
           (module-context-export context))))
   (unless (= (length descriptors) 1)
    (error "language build input must select exactly one descriptor" input (length descriptors)))
   ;; An initializer cannot make its historical input pass current admission.
   (when observed (module-source-admit-current! (list observed)))
   (car descriptors))))))

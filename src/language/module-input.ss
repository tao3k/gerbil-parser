;;; Build input selects one descriptor; multi-scope packs require an export name.
(import :gerbil/expander
        (only-in ./descriptor language-grammar?)
        (only-in ../runtime/identity sha256-text))
(export language-module-descriptor)
;; Track source identity only; contexts and evaluation stay SDK-owned.
(def source-identities (make-hash-table))
;; File inputs re-admit changed content, including same-size/same-time edits.
;; Library inputs use the SDK snapshot; explicit reload is not recursive.
(def (language-module-descriptor input reload?: (reload? #f))
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
         (digest (and file? (sha256-text
                   (call-with-input-file canonical
                     (lambda (port) (read-line port #f))))))
         (changed? (and file? (not (equal? digest (hash-get source-identities canonical)))))
         (context (import-module
                   (if (char=? (string-ref path 0) #\:) (string->symbol path) path)
                   (or reload? changed?) #t))
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
   ;; Commit only after evaluation and descriptor selection succeed.
   (when file? (hash-put! source-identities canonical digest))
   (car descriptors))))

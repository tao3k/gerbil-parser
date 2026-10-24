(import :gerbil-parser/language-test-support
        (only-in :std/io/tempfile make-temporary-file-name)
        (only-in :gerbil-parser/src/build-support/preparation-snapshot preparation-files))

(def (with-inventory-directory procedure)
  (let (root (make-temporary-file-name "parser-inventory-"))
    (create-directory* (path-expand "owned/deep" root))
    (create-directory* (path-expand "foreign" root))
    (for-each
      (lambda (name)
        (call-with-output-file (path-expand name root) (lambda (port) (write name port))))
      '("owned/a.ss" "owned/deep/b.ss" "foreign/a.ss" "foreign/z.ss" "root.ss"))
    (try (procedure root)
         (finally (delete-file-or-directory root #t)))))

(deflanguage-parser-tests preparation-snapshot-test "ownership-pruned preparation inventory"
  (engine)
  (scenario "pruning a directory retains the independent flattened selection in order"
    (using (with-inventory-directory) (root)
      (scope ((owned (path-expand "owned" root))
              (prefix (path-expand "owned/" root)))
        (equal (preparation-files root (lambda (path) (not (equal? path owned))))
               (filter (lambda (path) (not (string-prefix? prefix path)))
                       (preparation-files root))))))
  (scenario "an opaque ownership boundary never visits its descendants"
    (using (with-inventory-directory) (root)
      (scope ((owned (path-expand "owned" root))
              (prefix (path-expand "owned/" root)) (visited '()))
        (equal (preparation-files root
                 (lambda (path)
                   (when (string-prefix? prefix path) (error "entered excluded owner" path))
                   (set! visited (cons path visited))
                   (not (equal? path owned))))
               (map (lambda (name) (path-expand name root))
                    '("foreign/a.ss" "foreign/z.ss" "root.ss")))
        (equal (and (member owned visited) #t) #t))))
  (scenario "file exclusion and a missing root retain their original semantics"
    (using (with-inventory-directory) (root)
      (equal (preparation-files (path-expand "root.ss" root) (lambda (_) #f)) '())
      (equal (preparation-files (path-expand "missing" root)) '())
      (equal (preparation-files (path-expand "root.ss" root))
             (list (path-expand "root.ss" root))))))

;;; Native Gerbil owns Suite discovery; this file only selects build inputs.
(def (conformance-source-only? name)
  (or (string-suffix? "-test" name)
      (member name '("t/fixtures/language-entries"
                     "t/generate-language-abi-alignment" "t/research-main"
                     "t/benchmarks/versioned-languages/all-languages"))))
(def (test-plan-definition file name)
  (call-with-input-file file
    (lambda (port)
      (let loop ()
        (let (form (read port))
          (cond ((eof-object? form) (error "missing build declaration" file name))
                ((and (pair? form) (eq? (car form) 'def) (equal? (cadr form) name))
                 (caddr form))
                (else (loop))))))))
(def (conformance-covered-test-files)
  (let (catalog (test-plan-definition "build-language-conformance.ss" 'conformance-modules))
    (unless (and (pair? catalog) (eq? (car catalog) 'quote))
      (error "expected literal conformance module catalog"))
    (map (lambda (module) (string-append module ".ss"))
         (filter (lambda (module) (string-suffix? "-test" module)) (cadr catalog)))))

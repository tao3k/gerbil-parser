;;; ASP evaluates the source targets published by Parser's own build entry.
(import :std/test
        (only-in :std/source this-source-file)
        (only-in :clan/poo/object .get)
        (only-in "../init.ss" gerbil-parser.init.exports)
        (only-in :asp-gerbil-scheme/src/build-api/policy-runner
                 asp-gerbil-scheme-run-selected-policy))
(export asp-policy-test)

(def root (path-expand ".." (path-directory (this-source-file))))

(def asp-policy-test
  (test-suite "ASP policy"
    (test-case "check production library sources"
      (parameterize ((current-directory root))
        (asp-gerbil-scheme-run-selected-policy
          (map (lambda (row)
                 (string-append (if (string? row) row (cadr row)) ".ss"))
               (.get gerbil-parser.init.exports std-make-spec)))))))

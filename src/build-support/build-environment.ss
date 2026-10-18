;;; Stable search identity for nested invocations of the SDK environment tool.
(export canonical-build-search-path)

(def (canonical-build-search-path value)
  (if (not value) #f
    ;; The SDK splitter omits a final empty entry. Restore it explicitly: an
    ;; empty PATH component denotes the current directory, not an absent path.
    (let* ((parts (string-split value #\:))
           (parts (if (or (zero? (string-length value)) (string-suffix? ":" value))
                    (append parts '("")) parts))
           (first (car parts)))
      (string-join
        (cons first
          (let drop-leading ((rest (cdr parts)))
            (if (and (pair? rest) (equal? first (car rest)))
              (drop-leading (cdr rest)) rest)))
        ":"))))

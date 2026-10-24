;;; Stable search identity for nested invocations of the SDK environment tool.
(export canonical-build-search-path canonical-build-runtime-options)

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

;; SDK command nesting repeats its exact leading home/bin/lib relocation group.
;; Applying that same group again has no effect. Preserve every other option,
;; including order, heap flags, changed mappings and a trailing separator.
(def (canonical-build-runtime-options value)
  (if (not value) #f
    (let (parts (string-split value #\,))
      (if (not (and (>= (length parts) 3)
                    (string-prefix? "~~=" (car parts))
                    (string-prefix? "~~bin=" (cadr parts))
                    (string-prefix? "~~lib=" (caddr parts)))) value
        (let* ((prefix (string-join (list (car parts) (cadr parts) (caddr parts)) ","))
               (repeat (string-append "," prefix))
               (width (string-length repeat)))
          (let drop ((rest (substring value (string-length prefix) (string-length value))))
            (if (and (string-prefix? repeat rest)
                     (or (= width (string-length rest))
                         (char=? (string-ref rest width) #\,)))
              (drop (substring rest width (string-length rest)))
              (string-append prefix rest))))))))

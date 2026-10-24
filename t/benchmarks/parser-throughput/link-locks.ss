;;; Official compile-exe waits indefinitely for object locks. Reject a dirty
;;; static closure before starting; never remove locks owned by another build.
(export main static-link-locks)

(def (static-link-locks directory)
  (if (file-exists? directory)
    (map (cut path-expand <> directory)
         (filter (cut string-suffix? ".o.lock" <>) (directory-files directory)))
    '()))

(def (main . directories)
  (let (locks (foldr append '() (map static-link-locks directories)))
    (unless (null? locks)
      (error "AOT object locks exist; verify their owners before recovery" locks))))

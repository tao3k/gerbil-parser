;;; Compiler products are liveness evidence only. They never satisfy receipts
;;; or replace successful compiler exit. Cached files establish the baseline.
(export compiler-output-observer)

(def (compiler-output-observer directory (interval 0.5))
  (let ((observed (make-hash-table)) (last-scan 0) (started (floor (time->seconds (current-time)))))
    (def (scan report?)
      (let (changed? #f)
        (def (walk path)
         (with-catch
          (lambda (ex) (unless (no-such-file-or-directory-exception? ex) (raise ex)))
          (lambda ()
          (when (file-exists? path)
            (if (eq? (file-type path) 'directory)
              (for-each (lambda (name) (walk (path-expand name path))) (directory-files path))
              (when (member (path-extension path) '(".rmeta" ".rlib" ".o" ".a"))
                (let* ((info (file-info path))
                       (identity (cons (file-info-device info) (file-info-inode info)))
                       (state (list (file-info-size info)
                                    (time->seconds (file-info-last-modification-time info)))))
                  ;; The SDK exposes second-resolution mtime. Identity keeps
                  ;; same-second renames/hard links of cached files idle.
                  (when (and report? (> (car state) 0) (>= (cadr state) started)
                             (not (equal? (hash-ref observed identity #f) state)))
                    (set! changed? #t)
                    (displayln "COMPILER-OUTPUT " path " bytes=" (car state)) (force-output))
                  (hash-put! observed identity state))))))))
        (walk directory)
        changed?))
    (scan #f)
    (lambda ()
      (let (now (##current-time-point))
        (if (< (- now last-scan) interval) #f
          (begin (set! last-scan now) (scan #t)))))))

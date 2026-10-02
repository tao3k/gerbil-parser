;;; -*- Gerbil -*-
;;; Report real compiled-module admission before the test entry imports it.
;;; Dependency boundaries keep cold imports subject to the same silence gate.
(def +preloaded-modules+ '())

(def (compiled-dependencies value)
  (cond ((and (pair? value) (eq? (car value) 'load-module)
              (pair? (cdr value)) (string? (cadr value)))
         (list (cadr value)))
        ((pair? value)
         (append (compiled-dependencies (car value))
                 (compiled-dependencies (cdr value))))
        (else '())))

(def (preload-module module)
  (unless (member module +preloaded-modules+)
    (set! +preloaded-modules+ (cons module +preloaded-modules+))
    ;; Generated wrapper modules list dependencies without evaluating their
    ;; initializers. Phase bodies (~0/~1) are admitted directly, never decoded.
    (unless (string-contains module "~")
      (let (source
            (find file-exists?
                  (map (lambda (root) (path-expand (string-append module ".scm") root))
                       (cons (path-expand "lib" (gerbil-home)) (load-path)))))
        (when source
          (for-each preload-module
                    (call-with-input-file source
                      (lambda (port)
                        (let loop ((dependencies '()))
                          (let (value (read port))
                            (if (eof-object? value) dependencies
                              (loop (append dependencies (compiled-dependencies value))))))))))))
    (displayln "MODULE-LOAD " module) (force-output)
    (load-module module)
    (displayln "MODULE-LOADED " module) (force-output)))

(def (preload-tests directory)
  (for-each
   (lambda (name)
     (unless (string-prefix? "." name)
       (let (path (path-expand name directory))
         (cond ((eq? (file-type path) 'directory) (preload-tests path))
               ((string-suffix? "-test.ss" name)
                (displayln "TEST-IMPORT " path) (force-output)
                (gx#import-module path #f #t)
                (displayln "TEST-IMPORTED " path) (force-output))))))
   (list-sort string<? (directory-files directory))))

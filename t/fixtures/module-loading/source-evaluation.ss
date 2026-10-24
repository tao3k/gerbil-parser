;;; Real source helpers exercise SDK __eval-import, not a synthetic import walk.
(import :gerbil-parser/language-test-support :gerbil/expander
        (only-in :std/io/tempfile make-temporary-file-name))
(export with-source-evaluation source-export-value)
(def write-source (parser-test-datum-writer values))

(def (with-source-evaluation mode procedure)
  (let* ((directory (make-temporary-file-name "parser-source-evaluation-"))
         (root (path-expand "root.ss" directory)))
    (create-directory directory)
    (def (put name form)
      (call-with-output-file (path-expand name directory)
        (lambda (port) (write-source form port))))
    (try
      (put "left.ss"
        `(begin (export left)
           (def left ,(if (or (eq? mode #t) (eq? mode 'dormant)) '(error "source dependency ran too early")
                          '(begin (thread-sleep! 3) (list 'left))))))
      (put "right.ss"
        '(begin (export right)
           (def right (begin (thread-sleep! 3) (list 'right)))))
      (put "root.ss"
        (cond
          ((eq? mode 'dormant)
           `(begin (import (for-template ,(path-expand "left.ss" directory)))
              (export result) (def result 'dormant)))
          ((eq? mode 'dynamic)
           `(begin (export result)
              (def result
                (begin (eval '(begin (import ,(path-expand "left.ss" directory)
                                            ,(path-expand "right.ss" directory))))
                       'dynamic))))
          (else
           `(begin ,@(if mode '((error "source parent failed first")) '())
              (import ,(path-expand "left.ss" directory) ,(path-expand "right.ss" directory))
              (export result) (def result (list left right))))))
      (procedure (import-module root #f #f))
      (finally (delete-file-or-directory directory #t)))))

(def (source-export-value context name)
  (let (exported (find (lambda (entry) (eq? (module-export-name entry) name))
                      (module-context-export context)))
    (unless exported (error "missing source evaluation export" name))
    (eval (binding-id (core-resolve-module-export exported)))))

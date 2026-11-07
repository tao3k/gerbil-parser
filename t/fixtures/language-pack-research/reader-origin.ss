;;; -*- Gerbil -*-
;;; Capture definition syntax while its reader source is still available.
(export with-reader-origin)
(defsyntax (with-reader-origin stx)
  (def (position value)
    (and value
         (let (filepos (##position->filepos value))
           (list (cons 'line (+ 1 (##filepos-line filepos)))
                 (cons 'column (+ 1 (##filepos-col filepos)))))))
  (syntax-case stx ()
    ((_ origin-binding definition)
     (identifier? #'origin-binding)
     (let* ((location (stx-source #'definition))
            (origin
             (and location
                  (list (cons 'path (##container->path (##locat-container location)))
                        (cons 'start (position (##locat-start-position location)))
                        (cons 'end (position (##locat-end-position location)))))))
       (with-syntax ((source-value (datum->syntax #'origin-binding (list 'quote origin))))
         #'(begin
             (begin-syntax (def origin-binding source-value))
             definition))))
    (_ (raise-syntax-error #f "expected an origin binding and a definition" stx))))

;;; Build-time generation; benchmark requests call real native Fold products.
(import (only-in :std/misc/ports read-all-as-string)
        (only-in :gerbil-parser/t/event-strategy-fixture event-lines-language-grammar)
        (only-in :gerbil-parser/src/compiler/event-fold-runtime run-event-fold)
        (only-in :gerbil-parser/src/compiler/event-fold-scheme event-fold-scheme-source))
(export main)

(def (family-condition names)
  (cons 'or (map (lambda (name) `(line-starts-with-ascii-ci ,name)) names)))

(def (linear-forms names)
  (if (null? names)
    '((start-node Text) (token Line start end) (finish-node))
    `((if (line-starts-with-ascii-ci ,(car names))
          ((start-node Heading) (token Line start end) (finish-node)) ,(linear-forms (cdr names))))))

(def (main output)
  (call-with-output-file output
    (lambda (port)
      (for-each
       (lambda (family)
         (let ((label (car family)) (names (cdr family)))
           (for-each
            (lambda (fused?)
              (let (name (string->symbol
                          (string-append label (if fused? "-fused" "-linear"))))
                (let (forms (if fused?
                             `((if ,(family-condition names)
                                   ((start-node Heading) (token Line start end) (finish-node))
                                   ((start-node Text) (token Line start end) (finish-node))))
                             (linear-forms names)))
                  (display (event-fold-scheme-source name event-lines-language-grammar 'Document '()
                             forms '()) port)
                  (write `(def (,(string->symbol (string-append (symbol->string name) "-oracle")))
                            ,@(map (lambda (text)
                                     `(unless (equal? (,name ,text)
                                                      ',(run-event-fold text 'Document '() forms '() '()))
                                        (error "native prefix interpreter mismatch" ',name ,text)))
                                   (append '("" "#+T" "#+TITLE:x" "#+DESCRIPTIO:" "é中🦀")
                                           (foldr append '() (map
                                            (lambda (prefix)
                                              (list prefix (string-downcase prefix)
                                                    (string-append prefix " λ中🦀\r\n\n")
                                                    (substring prefix 0 (- (string-length prefix) 1))))
                                            names))))) port)
                  (newline port))))
            '(#f #t))))
       (list (cons "rich" '("#+TITLE:" "#+SUBTITLE:" "#+AUTHOR:" "#+DATE:" "#+CAPTION:" "#+DESCRIPTION:"))
             (cons "stress" (map (lambda (index) (string-append "#+KEY" (number->string index) ":")) (iota 32)))))
      (display (call-with-input-file "t/benchmarks/prefix-families/body.ss" read-all-as-string) port))))

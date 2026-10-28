;;; Downstream language and runtime boundaries use reviewed package interfaces.
(import :std/test
        (only-in :std/source this-source-file)
        (only-in :std/misc/process run-process)
        (only-in :std/misc/ports read-all-as-string))
(export public-interface-test)
(def (output-of code)
  (run-process (list "gxi" "-:max-heap=1G,debug=q" "-e" (object->string code))
    stdout-redirection: #t coprocess: read-all-as-string))
(def (fixture file)
  (path-expand (string-append "fixtures/parser-prelude/" file)
               (path-directory (this-source-file))))
(def public-interface-test
  (test-suite "Parser public language and runtime boundaries"
    (test-case "product prelude is inert and inherits common macro phases"
      (check-equal?
        (and (string-contains (output-of `(import ,(fixture "ordinary.ss")))
                             "PARSER-PRELUDE-INERT-OK") #t) #t))
    (test-case "product prelude lowers real grammar declarations"
      (check-equal?
        (and (string-contains (output-of `(import ,(fixture "grammar.ss")))
                             "PARSER-PRELUDE-GRAMMAR-OK") #t) #t))
    (test-case "public text values do not initialize a compiler"
      (def output
        (output-of '(begin
                      (import (only-in :gerbil-parser/text-support string-before ascii-ci=?))
                      (unless (and (string=? (string-before "name=value" "=") "name")
                                   (ascii-ci=? "Name" "NAME"))
                        (error "public text behavior changed"))
                      (when (ormap (lambda (row)
                                     (string-prefix? "gerbil-parser/src/compiler/" (car row)))
                                   (list-modules))
                        (error "public text imported compiler"))
                      (displayln "PUBLIC-TEXT-OK"))))
      (check-equal? (and (string-contains output "PUBLIC-TEXT-OK") #t) #t))))

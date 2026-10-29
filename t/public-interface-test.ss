;;; Downstream language and runtime boundaries use reviewed package interfaces.
(import :std/test
        (only-in :std/source this-source-file)
        (only-in :std/misc/process run-process)
        (only-in :std/misc/ports read-all-as-string))
(export public-interface-test)
(def (output-of code)
  (run-process (list "gxi" "-:max-heap=2G,debug=q" "-e" (object->string code))
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
    (test-case "Runtime publishes native capabilities without the API umbrella"
      (def output
        (output-of '(begin
                      (import (only-in :gerbil-parser/source-line-support fold-source-byte-spans)
                              (only-in :gerbil-parser/artifact-support parse-artifact-valid?))
                      (unless (and (not (parse-artifact-valid? #f))
                                   (equal? (reverse
                                             (fold-source-byte-spans
                                               (string->utf8 "a\r\nb") '()
                                               (lambda (from until rows)
                                                 (cons (list from until) rows))))
                                           '((0 3) (3 4))))
                        (error "Runtime public capability changed"))
                      (when (assoc "gerbil-parser/src/api" (list-modules))
                        (error "Runtime loaded the retired API umbrella"))
                      (displayln "PUBLIC-RUNTIME-OK"))))
      (check-equal? (and (string-contains output "PUBLIC-RUNTIME-OK") #t) #t))
    (test-case "native compiler capabilities do not construct the strategy catalogue"
      (def output
        (output-of '(begin
                      (import (only-in :gerbil-parser/compiler-build-support
                                       initialize-compiler-build! run-build-tool)
                              (only-in :gerbil-parser/event-fold-build-support
                                       event-fold-scheme-module-sources compile-event-fold-scheme-units))
                      (unless (and (procedure? initialize-compiler-build!)
                                   (procedure? run-build-tool)
                                   (procedure? event-fold-scheme-module-sources)
                                   (procedure? compile-event-fold-scheme-units))
                        (error "native compiler capability changed"))
                      (when (ormap (lambda (owner) (assoc owner (list-modules)))
                                   '("gerbil-parser/src/compiler/build-strategy~0"
                                     "gerbil-parser/src/compiler/runtime-strategy~0"
                                     "gerbil-parser/language-support/development~0"))
                        (error "native compiler capability loaded unrelated strategies"))
                      (displayln "PUBLIC-NATIVE-COMPILER-OK"))))
      (check-equal? (and (string-contains output "PUBLIC-NATIVE-COMPILER-OK") #t) #t))
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

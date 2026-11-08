;;; Downstream language and runtime boundaries use reviewed package interfaces.
(import :gerbil-parser/language-test-support
        (only-in :std/source this-source-file)
        (only-in :std/misc/process run-process)
        (only-in :std/misc/ports read-all-as-string))
(def (output-of code)
  (run-process (list "gxi" "-:max-heap=2G,debug=q" "-e" (object->string code))
    stdout-redirection: #t stderr-redirection: #t
    coprocess: (lambda (process)
                 (let (output (read-all-as-string process))
                   (display output) (force-output)
                   output))))
(def (fixture file)
  (path-expand (string-append "fixtures/parser-prelude/" file)
               (path-directory (this-source-file))))

(deflanguage-parser-tests public-interface-tests "Parser public language and runtime boundaries"
  (engine)
    (scenario "product prelude is inert and inherits common macro phases"
      (equal
        (and (string-contains (output-of `(import ,(fixture "ordinary.ss")))
                             "PARSER-PRELUDE-INERT-OK") #t) #t))
    (scenario "product prelude lowers real grammar declarations"
      (equal
        (and (string-contains (output-of `(import ,(fixture "grammar.ss")))
                             "PARSER-PRELUDE-GRAMMAR-OK") #t) #t))
    (scenario "Runtime publishes native capabilities without the API umbrella"
      (scope ((output
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
                      (displayln "PUBLIC-RUNTIME-OK")))))
        (equal (and (string-contains output "PUBLIC-RUNTIME-OK") #t) #t)))
    (scenario "native compiler capabilities do not construct the strategy catalogue"
      (scope ((output
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
                      (displayln "PUBLIC-NATIVE-COMPILER-OK")))))
        (equal (and (string-contains output "PUBLIC-NATIVE-COMPILER-OK") #t) #t)))
    (scenario "installed Fold program admission owns data and retains DAG sharing"
      (scope ((entry (path-expand "lib/gerbil-parser/event-fold-program-support.ssi" (gerbil-path))))
        (equal (file-exists? entry) #t)
        (scope ((output
          (output-of `(begin
            (import (only-in ,entry snapshot-fold-data event-fold-program?))
            (let* ((text (string-copy "abc")) (node (list text))
                   (owned (snapshot-fold-data (list node node))))
              (unless (and (eq? (car owned) (cadr owned))
                           (not (event-fold-program? #f)))
                (error "Fold program data contract changed"))
              (string-set! text 0 #\z)
              (unless (equal? (caar owned) "abc")
                (error "Fold program retained mutable caller data")))
            (unless (with-catch (lambda (_) #t)
                      (lambda () (snapshot-fold-data (lambda () #t)) #f))
              (error "Fold program accepted executable data"))
            (displayln "PUBLIC-FOLD-PROGRAM-OK")))))
          (equal (and (string-contains output "PUBLIC-FOLD-PROGRAM-OK") #t) #t))))
    (scenario "public text values do not initialize a compiler"
      (scope ((output
        (output-of '(begin
                      (import (only-in :gerbil-parser/text-support string-before ascii-ci=?))
                      (unless (and (string=? (string-before "name=value" "=") "name")
                                   (ascii-ci=? "Name" "NAME"))
                        (error "public text behavior changed"))
                      (when (ormap (lambda (row)
                                     (string-prefix? "gerbil-parser/src/compiler/" (car row)))
                                   (list-modules))
                        (error "public text imported compiler"))
                      (displayln "PUBLIC-TEXT-OK")))))
        (equal (and (string-contains output "PUBLIC-TEXT-OK") #t) #t))))

(def public-interface-test public-interface-tests)
(export public-interface-test)

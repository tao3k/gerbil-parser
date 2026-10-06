;;; Seed streams are executable controls copied from the prior HCL suite.
;;; Independent port-writing references retain the entire original sequence.
(import :std/test
        (only-in :gerbil-parser/src/testing/language-strategy generate-language-test-sources check-language-strategies)
        (only-in :clan/poo/object .cc .o .ref)
        (only-in :clan/poo/mop validate)
        (only-in :gerbil-parser/language-support/development LanguageDevelopmentLoaderContract)
        (only-in :gerbil-parser/languages/hcl/parser hcl-language)
        (only-in :gerbil-parser/languages/arithmetic/parser arithmetic-language))
(export language-strategy-test)

(def (reference-sources seed count build)
  (def (next-random modulus)
    (set! seed (modulo (+ (* seed 1103515245) 12345) 2147483648))
    (modulo seed modulus))
  (map (lambda (index) (build index next-random)) (iota count)))

(def language-strategy-test
  (test-suite "closed language strategy source admission"
    (test-case "all 512 ASCII sources preserve the prior stream"
      (check (generate-language-test-sources
               '(seeded 1729 512 (repeat 0 32 (character "abcXYZ_eE0123= \t\r\n"))))
             => (reference-sources 1729 512
                  (lambda (_ draw)
                    (let (alphabet "abcXYZ_eE0123= \t\r\n")
                      (call-with-output-string
                       (lambda (port)
                         (let chars ((remaining (draw 32)))
                           (when (> remaining 0)
                             (display (string-ref alphabet (draw (string-length alphabet))) port)
                             (chars (- remaining 1)))))))))))
    (test-case "all 128 quoted sources preserve the prior stream"
      (check (generate-language-test-sources
               '(seeded 3857 128 (concat "key = \"" (repeat 0 12 (choice "a" "0" " " "\\\\" "\\\"" "//")) "\"\n")))
             => (reference-sources 3857 128
                  (lambda (_ draw)
                    (let (pieces '#("a" "0" " " "\\\\" "\\\"" "//"))
                      (call-with-output-string
                       (lambda (port)
                         (display "key = \"" port)
                         (let parts ((remaining (draw 12)))
                           (when (> remaining 0)
                             (display (vector-ref pieces (draw (vector-length pieces))) port)
                             (parts (- remaining 1))))
                         (display "\"\n" port))))))))
    (test-case "all 128 comment sources preserve draw order and terminators"
      (check (generate-language-test-sources
               '(seeded 7103 128 (bind ((start (choice "#" "//" "/*")))
                  (concat "key = 1 " start (repeat 0 16 (character "ab09 #/\t"))
                          (when-equal start "/*" "*/") "\r\n"))))
             => (reference-sources 7103 128
                  (lambda (_ draw)
                    (let ((start (vector-ref '#("#" "//" "/*") (draw 3))) (alphabet "ab09 #/\t"))
                      (call-with-output-string
                       (lambda (port)
                         (display "key = 1 " port) (display start port)
                         (let chars ((remaining (draw 16)))
                           (when (> remaining 0)
                             (display (string-ref alphabet (draw (string-length alphabet))) port)
                             (chars (- remaining 1))))
                         (when (equal? start "/*") (display "*/" port))
                         (display "\r\n" port))))))))
    (test-case "all 128 numeric sources preserve parity and draw bounds"
      (check (generate-language-test-sources
               '(seeded 7207 128 (concat "key = " (integer 1 999) "." (integer 0 1000)
                                        (if-even "e+" "E-") (integer 0 30) "\n")))
             => (reference-sources 7207 128
                  (lambda (index draw)
                    (string-append "key = " (number->string (+ 1 (draw 999))) "."
                                   (number->string (draw 1000)) (if (even? index) "e+" "E-")
                                   (number->string (draw 30)) "\n")))))
    (test-case "all 128 mixed corridors preserve row and value streams"
      (check (generate-language-test-sources
               '(seeded 4919 128 (repeat 1 8
                  (concat (choice "a" "foo" "é" "name_2") " = "
                          (choice "1" "23" "foo" "true" "\"x\"" "\"雪\"")
                          (choice "\n" "\r\n" " # comment\n")))))
             => (reference-sources 4919 128
                  (lambda (_ draw)
                    (let ((names '#("a" "foo" "é" "name_2"))
                          (values '#("1" "23" "foo" "true" "\"x\"" "\"雪\""))
                          (ends '#("\n" "\r\n" " # comment\n")))
                      (call-with-output-string
                       (lambda (port)
                         (let rows ((remaining (+ 1 (draw 8))))
                           (when (> remaining 0)
                             (display (vector-ref names (draw (vector-length names))) port)
                             (display " = " port)
                             (display (vector-ref values (draw (vector-length values))) port)
                             (display (vector-ref ends (draw (vector-length ends))) port)
                             (rows (- remaining 1)))))))))))
    (test-case "lexical-only coverage executes each candidate once"
      (let* ((profile (.ref hcl-language 'native-test-profile))
             (lex (.ref profile 'lexer)) (calls 0)
             (owner (.cc hcl-language 'native-test-profile
                         (.cc profile 'lexer (lambda (source)
                                               (set! calls (+ calls 1)) (lex source))))))
        (validate LanguageDevelopmentLoaderContract owner)
        (check-language-strategies owner '(sources "key = 1\n" "雪" "")
                                  '((routes) (lexical optional) (coverage 0 0)))
        (check calls => 3)))
    (test-case "profiles reject stale digest, foreign source and missing lexer"
      (let* ((profile (.ref hcl-language 'native-test-profile))
             (rejected? (lambda (owner)
                          (with-catch (lambda (_) #t)
                            (lambda () (validate LanguageDevelopmentLoaderContract owner) #f)))))
        (check (rejected? (.cc hcl-language 'native-test-profile (.cc profile 'digest "sha256:stale"))) => #t)
        (check (rejected? (.cc hcl-language 'native-test-profile (.cc profile 'source (lambda args #t)))) => #t)
        (check (rejected? (.cc hcl-language 'native-test-profile (.cc profile 'lexer #f))) => #t)
        (check (rejected? (.cc hcl-language 'native-test-profile (.cc profile 'profile 'unknown))) => #t)
        (check (rejected? (.cc arithmetic-language 'native-test-profile profile)) => #t)
        (check (rejected? (.cc hcl-language 'native-test-profile (.cc profile 'digest (.ref profile 'digest)))) => #f)))))

;;; Closed declared regions: shared matching, source isolation and admission.
(import :std/test
        (only-in :gerbil-parser/src/runtime/region-scanner
                 prepare-region-plan valid-region-specification? region-plan-end
                 region-plan-specification region-plan-pair-end region-plan-quote-end
                 region-plan-operator)
        (only-in :gerbil-parser/languages/bash/grammar bash-word-regions))
(export region-scanner-test)
(def (rejects? thunk)
  (with-catch (lambda (_) #t) (lambda () (thunk) #f)))
(def region-scanner-test
  (test-suite "closed region plans"
    (test-case "longest openers and initial depth retain word boundaries"
      (for-each
       (lambda (word)
         (check (region-plan-end bash-word-regions (string-append word " ;tail") 0)
                => (string-length word)))
       '("α$((1+(2*3)))" "<(printf 'α')" ">(cat)" "${x:-$(printf '}')}"))
      (check (region-plan-operator bash-word-regions "x;;&y" 1) => ";;&"))
    (test-case "quote scopes admit declared nesting and shield literal openers"
      (let (source "\"${x:-$(printf '%s' '}')}\" tail")
        (check (region-plan-quote-end bash-word-regions source 0 #\")
               => (region-plan-end bash-word-regions source 0)))
      (check (region-plan-end bash-word-regions "'${opaque' rest" 0) => 10)
      (check (region-plan-end bash-word-regions "`$(opaque` rest" 0) => 10)
      (check (region-plan-end bash-word-regions "a\\ b rest" 0) => 4))
    (test-case "deep nesting uses explicit frames with exact endpoints"
      (let* ((depth 5000)
             (word (string-append
                    (apply string-append (make-list depth "$(")) "α"
                    (make-string depth #\))))
             (source (string-append "xx" word " rest")))
        (check (region-plan-end bash-word-regions source 2) => (+ 2 (string-length word)))
        (check (region-plan-pair-end bash-word-regions source 2) => (+ 2 (string-length word)))))
    (test-case "unterminated scopes fail rather than publish a partial word"
      (for-each
       (lambda (source)
         (check (rejects? (lambda () (region-plan-end bash-word-regions source 0))) => #t))
       '("${x" "$((1)" "<(cat" "\"${x}" "`x")))
    (test-case "prepared plans own their declaration data across sources"
      (let* ((spec (region-plan-specification bash-word-regions))
             (plan (prepare-region-plan spec))
             (view (region-plan-specification plan)))
        (string-set! (caar spec) 0 #\x)
        (string-set! (caar view) 0 #\y)
        (check (region-plan-operator plan ";;&" 0) => ";;&")
        (check (region-plan-end plan "${a} " 0) => 4)
        (check (region-plan-end plan "β " 0) => 1)))
    (test-case "malformed rows and foreign quote prefixes reject at admission"
      (for-each
       (lambda (spec)
         (check (valid-region-specification? spec) => #f)
         (check (rejects? (lambda () (prepare-region-plan spec))) => #t))
       '(((";") (bad-row) () #f)
         ((";") () (bad-row) #f)
         ((";" ";") () () #f)
         ((";") ((#\" #t ("missing"))) () #f)
         ((";") () (("$(" #\( #\) 0)) #f)
         ((";") () (("$(" #\( #\) 3)) #f)
         ((";") () (("α(" #\( #\) 3)) #f))))))

;;; Closed declared regions: shared matching, source isolation and admission.
(import :std/test
        (only-in :gerbil-parser/src/runtime/region-scanner
                 prepare-region-plan valid-region-specification? region-plan-end
                 region-plan-specification region-plan-pair-end region-plan-quote-end
                 region-plan-operator prepare-region-source prepare-scoped-region-source region-source-pair-end region-source-quote-end)
        (only-in :gerbil-parser/t/fixtures/bash-products bash-word-regions))
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
    (test-case "source-local boundaries retain all nested frame endpoints"
      (let* ((depth 5000)
             (text (string-append (apply string-append (make-list depth "${"))
                                  "α" (make-string depth #\})))
             (source (prepare-region-source bash-word-regions text)))
        (check (region-source-pair-end source 0) => (string-length text))
        (check (let loop ((at 0))
                 (or (= at depth)
                     (and (= (region-source-pair-end source (* 2 at))
                             (- (string-length text) at))
                          (loop (+ at 1))))) => #t)))
    (test-case "independent declared regions own text and preserve quote boundaries"
      (let* ((plan (prepare-region-plan '((";") ((#\" #t ("@{"))) (("@{" #\{ #\} 1)) #f)))
             (text (string-copy "@{\"@{α}\"}"))
             (source (prepare-region-source plan text)))
        (string-set! text 0 #\x)
        (check (region-source-pair-end source 0) => 9)
        (check (region-source-quote-end source 2 #\") => 8)
        (check (region-source-pair-end source 3) => 7)
        (check (rejects? (lambda () (region-source-quote-end source 2 #\'))) => #t))
      (let (source (prepare-region-source bash-word-regions "'literal ${x}"))
        ;; Here-document literals do not require matching the leading quote.
        (check (region-source-pair-end source 9) => 13)
        (check (rejects? (lambda () (region-source-quote-end source 0 #\'))) => #t)))
    (test-case "overlapping quote and pair entries keep separate match identities"
      (let* ((plan (prepare-region-plan '((";") ((#\" #t ())) (("\"(" #\( #\) 1)) #f)))
             (first (prepare-region-source plan "\"(α)\""))
             (second (prepare-region-source plan "\"(α)\"")))
        (check (region-source-quote-end first 0 #\") => 5)
        (check (region-source-pair-end first 0) => 4)
        (check (region-source-pair-end second 0) => 4)
        (check (region-source-quote-end second 0 #\") => 5)))
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
    (test-case "scoped overlapping pair openers retain independent cached endpoints"
      (let (plan (prepare-region-plan '((";") () (("#{" #\{ #\} 1) ("@" #\@ #\! 1) ("@(" #\( #\) 1)) #f)))
        (for-each (lambda (first)
                    (let (source (prepare-scoped-region-source plan "#{@(x)!}" '(("#{" "@"))))
                      (check (region-source-pair-end source first) => (if (= first 0) 8 6))
                      (check (region-source-pair-end source 0) => 8)
                      (check (region-source-pair-end source 2) => 6))) '(0 2))
        (check (rejects? (lambda () (prepare-scoped-region-source plan "x" '(("unknown" "@"))))) => #t)))
    (test-case "candidate indexes retain Unicode and declaration ownership"
      (let* ((spec (list '(";") '()
                         (list (list (string-copy "α{") #\{ #\} 1)
                               (list (string-copy "😀[") #\[ #\] 1)) #f))
             (plan (prepare-region-plan spec))
             (view (region-plan-specification plan))
             (source (prepare-region-source plan "α{😀[x]}")))
        (string-set! (car (car (caddr spec))) 0 #\x)
        (string-set! (car (car (caddr view))) 0 #\y)
        (check (region-source-pair-end source 0) => 7)
        (check (region-source-pair-end source 2) => 6)
        (check (region-plan-end plan "βx rest" 0) => 2)
        (for-each
         (lambda (at) (check (rejects? (lambda () (region-source-pair-end source at))) => #t))
         '(-1 1 7 8 0.5))))
    (test-case "overlapping buckets preserve longest admitted opener in either order"
      (for-each
       (lambda (pairs)
         (let* ((plan (prepare-region-plan (list '(";") '() pairs #f)))
                (outer (prepare-scoped-region-source plan "#{@(x)!}" '(("#{" "@"))))
                (unrestricted (prepare-region-source plan "@(x)!")))
           (check (region-source-pair-end unrestricted 0) => 4)
           (check (region-source-pair-end outer 0) => 8)
           (check (region-source-pair-end outer 2) => 6)
           (let (opaque (prepare-scoped-region-source plan "#{@(x}" '(("#{"))))
             (check (region-source-pair-end opaque 0) => 6))))
       '((("#{" #\{ #\} 1) ("@" #\@ #\! 1) ("@(" #\( #\) 1))
         (("@(" #\( #\) 1) ("@" #\@ #\! 1) ("#{" #\{ #\} 1)))))
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

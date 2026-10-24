;;; PATH identity preserves first-match lookup and every empty component.
(import :gerbil-parser/language-test-support
        (only-in :gerbil-parser/src/build-support/build-environment canonical-build-search-path canonical-build-runtime-options))

(deflanguage-parser-tests build-environment-test "SDK build environment identity"
  (engine)
  (matrix "only repeated leading entries collapse, retaining search order"
    (setup)
    ((row '((#f #f) ("" "") ("::" "") ("a:a:b" "a:b")
            ("a:a:a:b:" "a:b:") ("a:b:a" "a:b:a")
            ("a:b:b" "a:b:b") ("a:ab:b" "a:ab:b")
            ("a::b" "a::b") (":a:" ":a:")
            ("./bin:./bin:/bin" "./bin:/bin")
            ("/tools λ:/tools λ:/bin" "/tools λ:/bin"))))
    (bindings (input (car row)) (expected (cadr row)))
    (equal (canonical-build-search-path input) expected)
    (equal (canonical-build-search-path expected) expected))
  (matrix "only exact repeated SDK relocation groups collapse"
    (setup)
    ((row '((#f #f) ("" "") ("max-heap=1G,max-heap=1G" "max-heap=1G,max-heap=1G")
            ("~~=a,~~bin=b,~~lib=c,~~=a,~~bin=b,~~lib=c" "~~=a,~~bin=b,~~lib=c")
            ("~~=a,~~bin=b,~~lib=c,~~=a,~~bin=b,~~lib=c,max-heap=1G"
             "~~=a,~~bin=b,~~lib=c,max-heap=1G")
            ("~~=a,~~bin=b,~~lib=c,~~=a,~~bin=b,~~lib=c," "~~=a,~~bin=b,~~lib=c,")
            ("~~=a,~~bin=b,~~lib=c,~~=a,~~bin=b,~~lib=changed"
             "~~=a,~~bin=b,~~lib=c,~~=a,~~bin=b,~~lib=changed")
            ("~~=a,~~bin=b,~~lib=c,~~=a,~~bin=b,~~lib=cd"
             "~~=a,~~bin=b,~~lib=c,~~=a,~~bin=b,~~lib=cd")
            ("~~=a,~~lib=c,~~bin=b,~~=a,~~lib=c,~~bin=b"
             "~~=a,~~lib=c,~~bin=b,~~=a,~~lib=c,~~bin=b"))))
    (bindings (input (car row)) (expected (cadr row)))
    (equal (canonical-build-runtime-options input) expected)
    (equal (canonical-build-runtime-options expected) expected))
  (property "search order and unset PATH remain distinct identities"
    (bindings)
    (equal (equal? (canonical-build-search-path "a:b")
                   (canonical-build-search-path "b:a")) #f)
    (equal (equal? (canonical-build-search-path #f)
                   (canonical-build-search-path "")) #f)))

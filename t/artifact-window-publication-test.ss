(import :gerbil-parser/language-test-support)
(import :std/test
        (only-in ./fixtures/artifact-window window-tokens window-artifact)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-certified-window-artifact make-failure-parse-artifact
                 parse-artifact-events parse-artifact-valid-for-source? sha256-text))


(deflanguage-parser-tests artifact-window-publication-test "shared window publication"
  (engine)
    (scenario "expanded contracted and Unicode windows equal fresh products"
      (each (entry) ('(("abc" "axyc") ("abc" "ac") ("abc" "aλ🙂c") ("abc" "axc")))
         (sequential ((old-source (car entry)) (source (cadr entry))
                (old (window-tokens old-source)) (new (window-tokens source))
                (old-window (list (cadr old)))
                (new-window (take (cdr new) (- (length new) 2)))
                (base (window-artifact old-source old))
                (delta (- (u8vector-length (string->utf8 source))
                          (u8vector-length (string->utf8 old-source)))))
           (scope-values (((product shared)
                         (make-certified-window-artifact base source 1 old-window new-window delta)))
             (equal product  (window-artifact source new))
             (equal (parse-artifact-valid-for-source? product source)  #t)
             (equal (> shared 0)  #t)
             (equal (eq? (car (parse-artifact-events base))
                         (car (parse-artifact-events product)))  #t)
             (equal (parse-artifact-events base)
                     (parse-artifact-events (window-artifact old-source old)))))))
    (scenario "empty groups preserve explicit certified assignment order"
      (sequential ((old (window-tokens "abcd")) (new (window-tokens "aλ🙂d"))
             (base (window-artifact "abcd" old))
             (old-window (take (cdr old) 2)) (new-window (take (cdr new) 2)))
        (scope-values (((product shared)
                      (make-certified-window-artifact base "aλ🙂d" 1 old-window new-window 4
                        (list '() new-window) '((1 . 1) (3 . 7)))))
          (equal product  (window-artifact "aλ🙂d" new))
          (equal (parse-artifact-valid-for-source? product "aλ🙂d")  #t))
        (raises
         (make-certified-window-artifact base "aλ🙂d" 1 old-window new-window 4
           (list new-window new-window)) true)))
    (scenario "large complete event streams retain IDs and independent list ownership"
      (sequential ((source (make-string 20000 #\a)) (tokens (window-tokens source))
             (base (window-artifact source tokens)))
        (scope-values (((product shared)
                      (make-certified-window-artifact base source 0 tokens tokens 0
                        (map list tokens))))
          (equal product  base)
          (equal (parse-artifact-valid-for-source? product source)  #t)
          (equal shared  2)
          (equal (eq? (parse-artifact-events base) (parse-artifact-events product))  #f)))
      (sequential ((source (make-string 20000 #\a)) (tokens (window-tokens source))
             (product (make-failure-parse-artifact (sha256-text "window-failure") source tokens
                        '((message . "expected another token")))))
        (equal (parse-artifact-valid-for-source? product source)  #t)
        (equal (vector-ref (car (parse-artifact-events product)) 1)  0)
        (equal (vector-ref (list-ref (parse-artifact-events product) 19999) 1)  19999))))

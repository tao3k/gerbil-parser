(import :std/test
        (only-in ./fixtures/artifact-window window-tokens window-artifact)
        (only-in :gerbil-parser/src/runtime/artifact
                 make-certified-window-artifact make-failure-parse-artifact
                 parse-artifact-events parse-artifact-valid-for-source? sha256-text))
(export artifact-window-publication-test)

(def artifact-window-publication-test
  (test-suite "shared window publication"
    (test-case "expanded contracted and Unicode windows equal fresh products"
      (for-each
       (lambda (entry)
         (let* ((old-source (car entry)) (source (cadr entry))
                (old (window-tokens old-source)) (new (window-tokens source))
                (old-window (list (cadr old)))
                (new-window (take (cdr new) (- (length new) 2)))
                (base (window-artifact old-source old))
                (delta (- (u8vector-length (string->utf8 source))
                          (u8vector-length (string->utf8 old-source)))))
           (let-values (((product shared)
                         (make-certified-window-artifact base source 1 old-window new-window delta)))
             (check product => (window-artifact source new))
             (check (parse-artifact-valid-for-source? product source) => #t)
             (check (> shared 0) => #t)
             (check (eq? (car (parse-artifact-events base))
                         (car (parse-artifact-events product))) => #t)
             (check (parse-artifact-events base)
                    => (parse-artifact-events (window-artifact old-source old))))))
       '(("abc" "axyc") ("abc" "ac") ("abc" "aλ🙂c") ("abc" "axc"))))
    (test-case "empty groups preserve explicit certified assignment order"
      (let* ((old (window-tokens "abcd")) (new (window-tokens "aλ🙂d"))
             (base (window-artifact "abcd" old))
             (old-window (take (cdr old) 2)) (new-window (take (cdr new) 2)))
        (let-values (((product shared)
                      (make-certified-window-artifact base "aλ🙂d" 1 old-window new-window 4
                        (list '() new-window) '((1 . 1) (3 . 7)))))
          (check product => (window-artifact "aλ🙂d" new))
          (check (parse-artifact-valid-for-source? product "aλ🙂d") => #t))
        (check-exception
         (make-certified-window-artifact base "aλ🙂d" 1 old-window new-window 4
           (list new-window new-window)) true)))
    (test-case "large complete event streams retain IDs and independent list ownership"
      (let* ((source (make-string 20000 #\a)) (tokens (window-tokens source))
             (base (window-artifact source tokens)))
        (let-values (((product shared)
                      (make-certified-window-artifact base source 0 tokens tokens 0
                        (map list tokens))))
          (check product => base)
          (check (parse-artifact-valid-for-source? product source) => #t)
          (check shared => 2)
          (check (eq? (parse-artifact-events base) (parse-artifact-events product)) => #f)))
      (let* ((source (make-string 20000 #\a)) (tokens (window-tokens source))
             (product (make-failure-parse-artifact (sha256-text "window-failure") source tokens
                        '((message . "expected another token")))))
        (check (parse-artifact-valid-for-source? product source) => #t)
        (check (vector-ref (car (parse-artifact-events product)) 1) => 0)
        (check (vector-ref (list-ref (parse-artifact-events product) 19999) 1) => 19999)))))

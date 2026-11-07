;;; Progress observes actual compiler products; it never admits a result.
(import :gerbil-parser/language-test-support
        (only-in :std/io/tempfile make-temporary-file-name)
        (only-in ./support/qualification/compiler-output compiler-output-observer))

(def (with-compiler-products procedure)
  (let (root (make-temporary-file-name "parser-compiler-output-"))
    (create-directory root)
    (try (procedure root) (finally (delete-file-or-directory root #t)))))
(def (put-compiler-product path value)
  (call-with-output-file path (lambda (port) (write-string value port))))

(deflanguage-parser-tests compiler-output-test "owned compiler product progress"
  (engine)
  (scenario "dynamic SDK objects report growth once and unchanged files stay idle"
    (using (with-compiler-products) (root)
      (scope ((path (path-expand "module.o1" root)))
        (act (put-compiler-product path "cached"))
        (scope ((observe (compiler-output-observer root 0)))
          (equal (observe) #f)
          (act (put-compiler-product path "new-object-body"))
          (equal (observe) #t)
          (equal (observe) #f)
          (act (put-compiler-product (path-expand "heartbeat.log" root) "unrelated"))
          (equal (observe) #f)))))
  (scenario "an explicitly selected executable reports actual output growth"
    (using (with-compiler-products) (root)
      (scope ((path (path-expand "compiler-product" root)))
        (scope ((observe (compiler-output-observer path 0)))
          (equal (observe) #f)
          (act (put-compiler-product path "native-image"))
          (equal (observe) #t)
          (equal (observe) #f)))))
)

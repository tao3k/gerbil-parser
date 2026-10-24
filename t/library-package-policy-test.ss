;;; SDK-backed package direction, runtime generations and pre-expansion refusal.
(import :gerbil-parser/language-test-support
        (only-in :std/io/tempfile make-temporary-file-name)
        (only-in :std/error Error?)
        (only-in :gerbil/tools/gxpkg pkg-root-dir)
        (only-in :gerbil/expander current-expander-module-import)
        "../src/build-support/package-inputs.ss"
        (only-in "../src/build-support/compiler-profile.ss" call-with-compiler-import-progress initialize-compiler-build!)
        (only-in "../src/build-support/library-publication.ss" library-publication-inputs
                 call-with-library-publication call-with-library-reader))
(export library-package-policy-test)
(def (put path value)
  (create-directory* (path-directory path))
  (call-with-output-file (list path: path truncate: #t)
    (lambda (port) (display value port))))
(def (with-packages procedure)
  (def token (path-strip-directory (make-temporary-file-name "input-policy-")))
  (def parent (path-expand (string-append "qualification.local/" token) (pkg-root-dir)))
  (def checkout (make-temporary-file-name "input-policy-checkout-"))
  (def output (path-expand "output" checkout))
  (def (key name) (string-append "qualification.local/" token "/" name))
  (def (ns name) (string-append token "-" name))
  (def (source name) (path-expand (key name) (pkg-root-dir)))
  (def (declare name dependencies (prefix (ns name)))
    (def target (if (string=? name "enclosing") checkout (path-expand name checkout)))
    (create-directory* target)
    (create-directory* parent)
    (unless (file-exists? (source name)) (create-symbolic-link target (source name)))
    (put (path-expand "gerbil.pkg" (source name))
      (object->string (list package: (string->symbol prefix) depend: dependencies))))
  (try
    (declare "dependency" '())
    (declare "base" (list (key "dependency")))
    (declare "middle" (list (key "base")))
    (declare "consumer" (list (key "middle")))
    (declare "unrelated" '())
    ;; Like a linked application checkout containing its .gerbil/pkg tree,
    ;; this registered consumer encloses independently registered packages.
    (declare "enclosing" (list (key "base")))
    (create-directory* (path-expand "lib" output))
    (procedure key ns source output declare)
    (finally
      (for-each (lambda (name)
                  (when (file-exists? (source name)) (delete-file (source name))))
                '("dependency" "base" "middle" "consumer" "unrelated" "enclosing"))
      (when (file-exists? parent) (delete-file-or-directory parent #t))
      (when (file-exists? checkout) (delete-file-or-directory checkout #t))
      ;; Only this fixture's unique native namespace is ever removed.
      (for-each
        (lambda (name)
          (let (path (path-expand (string-append "lib/" (ns name)) (gerbil-path)))
            (when (file-exists? path) (delete-file-or-directory path #t))))
        '("consumer" "unrelated")))))
(deflanguage-parser-tests library-package-policy-test "Native package publication direction"
  (engine)
  (scenario "SDK transitive consumers are excluded; dependencies and unknown inputs remain"
    (using
      (with-packages)
      (key ns source output declare)
      (each
        (path)
        ((append
           (list
             (string-append "lib/" (ns "consumer") "/x.o2")
             (string-append "lib/" (ns "middle") "/x.ssi")
             "lib/unknown/input.ssi")
           (map
             (lambda (suffix) (string-append "lib/" (ns "dependency") "/x" suffix))
             '(".ssi" ".o1" ".o2" ".o3" ".scm"))))
        (act (put (path-expand path output) "fixture")))
      (act (put (path-expand "x.ss" (source "consumer")) "fixture"))
      (sequential
        ((policy (package-input-policy (key "base") output)))
        (equal (package-input-path? policy (path-expand "x.ss" (source "base"))) #t)
        (equal (package-input-path? policy (path-expand "x.ss" (source "dependency"))) #t)
        (equal (package-input-path? policy (path-expand "../x.ss" (source "base"))) #f)
        (equal
          (package-input-path? policy
            (path-expand (string-append "lib/" (ns "consumer") "/x.o2") output))
          #f)
        (equal
          (package-input-path? policy
            (path-expand (string-append "lib/" (ns "middle") "/x.ssi") output))
          #f)
        (each
          (suffix)
          ('(".ssi" ".o1" ".o2" ".o3" ".scm"))
          (equal
            (package-input-path? policy
              (path-expand (string-append "lib/" (ns "dependency") "/x" suffix) output))
            #t))
        (equal (package-input-path? policy (path-expand "lib/unknown/input.ssi" output)) #t)
        (equal (package-input-path? policy (path-expand "x.ss" (source "consumer"))) #f))))
  (scenario "compiler import admission refuses consumers before macro code executes"
    (using
      (with-packages)
      (key ns source output declare)
      (sequential
        ((policy (package-input-policy (key "base") (gerbil-path)))
         (marker (path-expand "forbidden" output))
         (allowed (path-expand "allowed" output))
         (module
           (lambda
             (name file)
             (def path (path-expand (string-append "lib/" (ns name) "/probe.ss") (gerbil-path)))
             (put path
               (string-append "package: "
                 (ns name)
                 "\n(begin-syntax (call-with-output-file "
                 (object->string file)
                 " (lambda (p) (display \"executed\" p))))\n"))
             path))
         (blocked (module "consumer" marker)))
        (act (module "unrelated" allowed))
        (sequential
          ((import
             (lambda
               (path)
               (call-with-compiler-import-progress
                 (lambda () ((current-expander-module-import) path #f))
                 (lambda (path) (package-input-admit-import! policy path))))))
          (raises (import (string->symbol (string-append ":" (ns "consumer") "/probe"))) Error?)
          (raises (import blocked) Error?)
          (equal (file-exists? marker) #f)
          (act (import (string->symbol (string-append ":" (ns "unrelated") "/probe"))))
          (equal (file-exists? allowed) #t)))))
  (scenario "base and whole-environment views retain different input obligations"
    (using
      (with-packages)
      (key ns source output declare)
      (parameters
        ((current-directory (source "base")))
        (equal (current-package-input-owner) (key "base"))
        (act (initialize-compiler-build!))
        (sequential
          ((before (library-publication-inputs output (key "base")))
           (whole (library-publication-inputs output)))
          (act
            (call-with-library-publication output
              (lambda
                (root)
                (put (path-expand "lib/gerbil-parser/module.ssi" root) "interface")
                (put (path-expand "lib/gerbil-parser/module.o1" root) "object"))
              (lambda (root) (list '("compile") (library-publication-inputs root (key "base"))))))
          (equal (call-with-library-reader output (lambda () 'readable)) 'readable)
          (act
            (put
              (path-expand (string-append "lib/" (ns "consumer") "/changed.o1") output)
              "consumer output"))
          (equal (library-publication-inputs output (key "base")) before)
          (equal (call-with-library-reader output (lambda () 'readable)) 'readable)
          (equal (equal? (library-publication-inputs output) whole) #f)
          (act
            (put
              (path-expand (string-append "lib/" (ns "dependency") "/changed.o2") output)
              "effective native input"))
          (equal (equal? (library-publication-inputs output (key "base")) before) #f)
          (raises (call-with-library-reader output void) Error?)
          (raises (library-publication-inputs output (key "unrelated")) Error?)
          (sequential
            ((policy (package-input-policy (key "base") output)))
            (act (declare "consumer" '()))
            (raises (library-publication-inputs output (key "base")) Error?)
            (raises (package-input-admit-import! policy ':gerbil/core) Error?))))))
  (scenario "extension handoff preserves current base and environment obligations"
    (using
      (with-packages)
      (key ns source output declare)
      (parameters
        ((current-directory (source "base")))
        (act (initialize-compiler-build!))
        (sequential
          ((calls 0)
           (build
             (lambda
               (root)
               (set! calls (+ calls 1))
               (put (path-expand "lib/gerbil-parser/module.ssi" root) "interface")
               (put (path-expand "lib/gerbil-parser/module.o1" root) "object")))
           (base (lambda (root) (list '("compile") (library-publication-inputs root (key "base")))))
           (whole (lambda (root) (list '("compile") (library-publication-inputs root)))))
          (act (call-with-library-publication output build base))
          (sequential
            ((previous-path (getenv "PATH")))
            (protect
              (body
                (act (setenv "PATH" (string-append output ":" previous-path)))
                (raises (call-with-library-publication output build whole #f) Error?))
              (cleanup (act (setenv "PATH" previous-path))))
            (equal calls 1)
            (sequential
              ((added (path-expand "added.ss")))
              (act (put added "(def added 1)"))
              (raises (call-with-library-publication output build whole #f) Error?)
              (act (delete-file added))
              (equal calls 1)
              (act (call-with-library-publication output build whole #f))
              (equal calls 2)
              (equal (call-with-library-reader output (lambda () 'extended)) 'extended)
              (act
                (put
                  (path-expand (string-append "lib/" (ns "consumer") "/new.o1") output)
                  "changed"))
              (raises (call-with-library-reader output void) Error?)))))))
  (scenario "a reverse consumer cannot claim a dependency's namespace"
    (using
      (with-packages)
      (key ns source output declare)
      (act (declare "collision" (list (key "base")) (ns "dependency")))
      (raises (package-input-policy (key "base") output) Error?))))

#!/usr/bin/env gxi
;;; Native object products retain Gerbil's bootstrap and Gambit's native link order.
(import (only-in :gerbil/compiler compile-exe compile-module)
        (only-in :std/list/list delete-duplicates/hash)
        (only-in :std/sync/wg make-wg wg-add! wg-wait!)
        (only-in :std/misc/process run-process/batch))

;; Report actual compiler GC work while large immutable images are lowered.
;; This changes the compiler process diagnostics, never generated program code.
(def compiler-progress-expression
  "(begin (port-settings-set! (current-output-port) (list buffering: #f)) (gc-report-set! #t))")

(def (unique paths)
  (delete-duplicates/hash paths from-end?: #t))
(def (replace-extension path extension)
  (string-append (path-strip-extension path) extension))
(def (newer? source target)
  (or (not (file-exists? target))
      (> (time->seconds (file-info-last-modification-time (file-info source)))
         (time->seconds (file-info-last-modification-time (file-info target))))))

(def (run-static-jobs sources task)
  ;; One standard queue keeps every worker available to the remaining closure.
  (let (cores (string->number (getenv "GERBIL_BUILD_CORES" "1")))
    (unless (and (integer? cores) (exact? cores) (> cores 0))
      (error "GERBIL_BUILD_CORES must be a positive integer" cores))
    (let ((ordered (list-sort (lambda (left right)
                               (> (file-info-size (file-info left))
                                  (file-info-size (file-info right)))) sources))
          (workers (make-wg cores)))
      (for-each (lambda (source) (wg-add! workers (lambda () (task source)))) ordered)
      (wg-wait! workers))))

(def (bootstrap-identities stub)
  (let (definition (call-with-input-file stub read))
    (unless (and (list? definition) (= (length definition) 3)
                 (eq? (car definition) 'define)
                 (eq? (cadr definition) 'builtin-modules)
                 (list? (caddr definition)) (= (length (caddr definition)) 2)
                 (eq? (car (caddr definition)) 'quote)
                 (list? (cadr (caddr definition)))
                 (andmap string? (cadr (caddr definition))))
      (error "unexpected pinned SDK native bootstrap" definition))
    (cadr (caddr definition))))

(def (link-groups gsc binary c-files)
  ;; Gambit's documented incremental base chain retains SDK module order.
  ;; Sixteen modules per C unit is a build partition, not a parser policy.
  (let loop ((remaining c-files) (base #f) (index 0) (result []))
    (if (null? remaining)
      (reverse result)
      (let* ((count (min 16 (length remaining)))
             (group (take remaining count))
             (rest (drop remaining count))
             (target (string-append binary "__link_" (number->string index) ".c")))
        (displayln "CONFORMANCE-LINK-GROUP " index " modules=" count)
        (force-output)
        (run-process/batch
          [gsc "-verbose" "-link"
           (if base ["-l" (path-strip-extension base)] []) ...
           "-o" target group ...])
        (loop rest target (+ index 1) (cons target result))))))

(def (main (owner "conformance"))
  (let* ((library? (equal? owner "library-objects"))
         (source (path-expand (if library?
                                "t/fixtures/ffi/library-closure.ss"
                                "t/conformance-main.ss")))
         (binary (path-expand (if library? "bin/gerbil-parser-library-objects"
                                           "bin/gerbil-parser-conformance")
                   (getenv "GERBIL_PATH")))
         (home (getenv "GERBIL_BUILD_PREFIX" (gerbil-home)))
         (library (path-expand "lib" home))
         (static (path-expand "static" library))
         (gsc (path-expand "bin/gsc" home))
         (stub (string-append binary "__exe.scm")))
    (unless (member owner '("conformance" "library-objects"))
      (error "unknown native object owner" owner))
    (when library?
      (compile-module source
        [output-dir: (path-expand "lib" (getenv "GERBIL_PATH"))
         invoke-gsc: #f optimize: #f generate-ssxi: #t static: #t keep-scm: #t verbose: #f]))
    ;; Public compiler API generates the bootstrap, including admitted identities.
    (compile-exe source [output-file: binary invoke-gsc: #f verbose: #f])
    (let* ((identities (bootstrap-identities stub))
           (entry (if library? "gerbil-parser/t/fixtures/ffi/library-closure"
                               "gerbil-parser/t/conformance-main"))
           (_ (unless (member entry identities)
                (error "native product entry is absent from SDK bootstrap" entry)))
           (ordered
            (append (filter (lambda (name)
                              (and (not (equal? name entry))
                                   (not (string-prefix? "gerbil/core" name)))) identities)
                    [entry]))
           (roots (unique (cons (path-expand "lib" (getenv "GERBIL_PATH"))
                                (cons library (load-path)))))
           (sources
            (map (lambda (name)
                   (let* ((basename (string-append
                                      (string-join (string-split name #\/) "__") ".scm"))
                          (path (find file-exists?
                                  (map (lambda (root)
                                         (path-expand basename (path-expand "static" root))) roots))))
                     (or path (error "native bootstrap source is missing" name)))) ordered))
           (user-sources (filter (lambda (path) (not (string-prefix? static path))) sources)))
      (when library?
        (for-each (lambda (source) (displayln "NATIVE-LIBRARY-SOURCE " source)) user-sources)
        (force-output))
      (let* ((includes (unique (map path-directory sources)))
             (cc-options (string-append "-v -Q -fopt-info-all -ftrack-macro-expansion=0 "
                            (string-join
                              (map (lambda (path) (string-append "-I" path)) includes) " ")))
             (ld-options (call-with-input-file (path-expand "libgerbil.ldd" library) read)))
        (run-static-jobs user-sources
          (lambda (source)
            (let ((target (replace-extension source ".o"))
                  (c-file (replace-extension source ".c")))
              ;; One worker owns translation and its object; no global phase barrier.
              (when (newer? source c-file)
                (displayln "STATIC-TRANSLATE " source) (force-output)
                (run-process/batch [gsc "-verbose" "-c" "-o" c-file
                                    "-e" compiler-progress-expression source])
                (displayln "STATIC-TRANSLATED " source) (force-output))

              (when (newer? c-file target)
                (displayln "STATIC-OBJECT " c-file) (force-output)
                (run-process/batch [gsc "-verbose" "-cc-options" cc-options
                                   "-obj" "-o" target c-file])
                (displayln "STATIC-OBJECT-READY " c-file) (force-output)))))
        (for-each (lambda (source)
          (unless (file-exists? (replace-extension source ".o"))
            (error "native library object is missing" source))) sources)
        (if library?
          (begin (displayln "COMPILED-LIBRARY-OBJECTS-READY") (force-output))
        (let* ((stub-c (replace-extension stub ".c"))
               (stub-object (replace-extension stub ".o"))
               (objects (map (lambda (path) (replace-extension path ".o")) sources))
               (c-files (append (map (lambda (path) (replace-extension path ".c")) sources)
                                [stub-c])))
          (for-each (lambda (object)
                      (unless (file-exists? object)
                        (error "SDK or native object is missing" object))) objects)
          (run-process/batch [gsc "-verbose" "-c" "-o" stub-c stub])
          (run-process/batch [gsc "-verbose" "-cc-options" cc-options
                             "-obj" "-o" stub-object stub-c])
          (let (links (link-groups gsc binary c-files))
            (run-static-jobs links
              (lambda (link-file)
                ;; Library bases omit C main; the final incremental link owns it.
                (let* ((final? (equal? link-file (last links)))
                       (options (string-append cc-options (if final? "" " -D___LIBRARY"))))
                  (displayln "CONFORMANCE-LINK-OBJECT " link-file) (force-output)
                  (run-process/batch [gsc "-verbose" "-cc-options" options "-obj"
                                     "-o" (replace-extension link-file ".o") link-file])
                  (displayln "CONFORMANCE-LINK-OBJECT-READY " link-file) (force-output))))
            ;; SDK objects are reused unchanged; only project objects are built.
            (run-process/batch [(getenv "GERBIL_GCC" "gcc") "-w" "-o" binary
                               objects ... stub-object
                               (map (lambda (path) (replace-extension path ".o")) links) ...
                               (string-append "-Wl,-rpath," library)
                               "-L" library "-lgambit" ld-options ...])
            (displayln "CONFORMANCE-LINKED " binary) (force-output))))))))

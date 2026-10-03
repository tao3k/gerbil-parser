#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Bound retained expansion/runtime state to one native test owner per process.
(import (only-in :std/misc/process run-process))

(def (test-files directory)
  (if (not (eq? (file-type directory) 'directory))
    (list directory)
    (apply append
   (map (lambda (name)
          (if (string-prefix? "." name) '()
            (let (path (path-expand name directory))
              (cond ((eq? (file-type path) 'directory) (test-files path))
                    ((string-suffix? "-test.ss" name) (list path))
                    (else '())))))
        (list-sort string<? (directory-files directory))))))

(def (main . roots)
  (let (files (apply append (map test-files roots)))
    (when (null? files) (error "native test suite has no modules" roots))
    (for-each
     (lambda (file)
       (let (cases 0)
         (displayln "MODULE-BATCH-START " file) (force-output)
         (run-process
          ["gxi" "t/fixtures/tla-sany-differential/watch.ss" "gxi"
           "-e" (string-append
                    "(load \"t/fixtures/tla-sany-differential/preload.ss\") "
                    "(prefer-native-interfaces!) "
                    "(preload-module \"gerbil-parser/src/compiler/parser-ir\") "
                    "(preload-module \"gerbil/tools/gxtest\") "
                    "(preload-module \"gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process\") "
                    "(preload-test-imports " (object->string file) ")")
           "-e" (string-append
                    "(import :gerbil/tools/gxtest :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process) (let (status (main \"-v\" \"6\" "
                    (object->string file) ")) "
                    "(displayln \"HARNESS-RETURN status=\" status) (force-output) "
                    "(##gc) (displayln \"FINAL-GC-OK\") (force-output) "
                    "(test-child-process-exit! status))")]
          stderr-redirection: #t
          coprocess:
          (lambda (process)
            (close-output-port process)
            (let loop ()
              (let (line (read-line process))
                (unless (eof-object? line)
                  (when (string-prefix? "CASE-OK " line)
                    (set! cases (+ cases 1)))
                  (displayln line) (force-output) (loop))))))
         (when (zero? cases) (error "native test module ran no cases" file))
         (displayln "MODULE-BATCH-OK " file " cases=" cases) (force-output))) files)
    (displayln "NATIVE-SUITE-OK modules=" (length files))
    (displayln "OK")))

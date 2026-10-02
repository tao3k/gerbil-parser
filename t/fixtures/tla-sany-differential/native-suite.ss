#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Bound retained expansion/runtime state to one native test owner per process.
(import (only-in :std/misc/process run-process))

(def (test-files directory)
  (apply append
   (map (lambda (name)
          (if (string-prefix? "." name) '()
            (let (path (path-expand name directory))
              (cond ((eq? (file-type path) 'directory) (test-files path))
                    ((string-suffix? "-test.ss" name) (list path))
                    (else '())))))
        (list-sort string<? (directory-files directory)))))

(def (main . roots)
  (let (files (apply append (map test-files roots)))
    (when (null? files) (error "native test suite has no modules" roots))
    (for-each
     (lambda (file)
       (displayln "MODULE-BATCH-START " file) (force-output)
       (run-process
        ["gxi" "t/fixtures/tla-sany-differential/watch.ss" "gxi"
         "-e" "(load \"t/fixtures/tla-sany-differential/preload.ss\") (preload-module \"gerbil-parser/languages/tla-plus/sany-candidate\") (preload-module \"gerbil-parser/src/compiler/parser-ir\") (preload-module \"gerbil/tools/gxtest\")"
         "-e" (string-append "(import :gerbil/tools/gxtest) (main \"-v\" \"6\" "
                              (object->string file) ")")]
        stderr-redirection: #t
        coprocess:
        (lambda (process)
          (close-output-port process)
          (let loop ()
            (let (line (read-line process))
              (unless (eof-object? line)
                (displayln line) (force-output) (loop))))))
       (displayln "MODULE-BATCH-OK " file) (force-output)) files)
    (displayln "NATIVE-SUITE-OK modules=" (length files))
    (displayln "OK")))

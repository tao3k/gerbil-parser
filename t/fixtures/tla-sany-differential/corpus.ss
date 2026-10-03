#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Scheme owns the immutable corpus, parser, validity, and lossless receipts.
(import (only-in :std/string/misc string-trim)
        (only-in :std/misc/process run-process)
        (only-in :std/misc/ports read-all-as-string)
        :gerbil-parser/languages/tla-plus/sany-candidate
        :gerbil-parser/src/runtime/artifact)

(def +examples-pin+ "ceeaa904140e3e03781cb2a79cd6c6d8b8b08e10")

(def (main root)
  (let* ((head (string-trim (run-process ["git" "-C" root "rev-parse" "HEAD"])))
         (status (string-trim (run-process ["git" "-C" root "status" "--porcelain"])))
         (paths (filter (lambda (path) (not (string-empty? path)))
                        (string-split (run-process ["git" "-C" root "ls-files" "*.tla"]) #\newline))))
    (unless (and (equal? head +examples-pin+) (string-empty? status) (= (length paths) 424))
      (error "corpus must be the clean immutable 424-file Examples checkout" head status (length paths)))
    (displayln "CORPUS-PIN-OK " head " files=424") (force-output)
    (for-each
     (lambda (relative)
       (displayln "CORPUS-START " relative) (force-output)
       (let* ((source (call-with-input-file (path-expand relative root) read-all-as-string))
              (artifact (parse-tla-plus-sany-candidate source)))
         (unless (and (parse-artifact-success? artifact) (parse-artifact-valid? artifact)
                      (equal? source (parse-artifact-roundtrip artifact)))
           (error "corpus syntax or lossless artifact gate failed" relative
                  (parse-artifact-ref artifact 'diagnostics)))
         (displayln "CORPUS-OK " relative) (force-output))) paths)
    (displayln "CORPUS-OK accepted=424 rejected=0 invalid=0 roundtrip-failed=0")))

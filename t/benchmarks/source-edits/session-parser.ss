#!/usr/bin/env gxi
;;; Complete Scheme session workload; fresh parsing is an independent semantic control.
(import (only-in :gerbil-parser/languages/bash/parser bash-source-language)
        (only-in :gerbil-parser/src/language/source
                 parse-source-language parse-source-language/session source-language-session-artifact
                 source-language-session-scanned-token-count source-language-session-reused-token-count)
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-valid? parse-artifact-roundtrip))
(export main)
(def (main . _args)
  (let* ((prefix (string-join (make-list 64 "echo prefix\n") ""))
         (tail (string-join (make-list 64 "echo tail\n") ""))
         (before (string-append prefix "cat <<A\nα\nA\n" tail))
         (history (parse-source-language/session bash-source-language before)))
    (for-each
     (lambda (fixture)
       (let ((name (car fixture)) (source (cadr fixture)))
         (for-each
          (lambda (session?)
            (##gc)
            (let* ((cpu (cpu-time))
                   (result (if session?
                             (parse-source-language/session bash-source-language source history)
                             (parse-source-language bash-source-language source)))
                   (cpu-ms (* 1000 (- (cpu-time) cpu)))
                   (artifact (if session? (source-language-session-artifact result) result))
                   (fresh (parse-source-language bash-source-language source)))
              (unless (and (equal? artifact fresh) (parse-artifact-valid? artifact)
                           (string=? (parse-artifact-roundtrip artifact) source))
                (error "session changed complete parser artifact" name session?))
              (write (list 'fixture name 'session session? 'cpu-ms cpu-ms
                           'scanned (and session? (source-language-session-scanned-token-count result))
                           'reused (and session? (source-language-session-reused-token-count result))))
              (newline) (force-output)))
          '(#f #t #t #f))))
     (list (list 'prefix (string-append "猫 " before))
           (list 'middle (string-append prefix "cat <<'A'\nβ\nA\n" tail))
           (list 'tail (string-append before "echo 終\n"))
           (list 'grammar-rejection (string-append "| echo bad\n" tail))
           (list 'lexical-rejection "cat <<MISSING\nunfinished\n"))))
  (displayln "SOURCE-SESSION-PARSER-OK") (force-output))

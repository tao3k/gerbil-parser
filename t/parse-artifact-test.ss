#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        :gerbil-parser/src/runtime/artifact
        (only-in :gerbil-parser/src/runtime/identity sha256-bytes)
        :gerbil-parser/src/runtime/cst
        :gerbil-parser/src/runtime/parser
        :gerbil-parser/languages/arithmetic/parser)

(def (artifact-replace artifact key value)
  (map (lambda (row)
         (if (eq? (car row) key) (cons key value) row))
       artifact))

(def (remove-token-id events rejected-id)
  (let loop ((rest events) (found '()))
    (cond
     ((null? rest) (reverse found))
     ((and (token-event? (car rest))
           (= (token-event-id (car rest)) rejected-id))
      (loop (cdr rest) found))
     (else (loop (cdr rest) (cons (car rest) found))))))

(def parse-artifact-tests
  (test-suite "ParseArtifact contract"
    (test-case "canonical SHA-256 identities match fixed digest vectors"
      (check (sha256-text "")
             => "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
      (check (sha256-text "abc")
             => "sha256:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
      (check (sha256-text "λ中文🙂")
             => "sha256:769868d0dd7575e844f10175e18222eb50c31aacd8f3cce0f3ce55fe2f0e53e2")
      (let ((bytes (make-u8vector 256)))
        (let fill ((index 0))
          (when (< index 256)
            (u8vector-set! bytes index index)
            (fill (+ index 1))))
        (check (sha256-bytes bytes)
               => "sha256:40aff2e9d2d8922e47afd4648e6967497158785fbd1da870e7110266bf944880")))
    (test-case "identity and event replay are deterministic"
      (let* ((source "alpha + (2 * beta)")
             (first (parse-source arithmetic-parser source))
             (second (parse-source arithmetic-parser source))
             (first-cst (parse-artifact->cst first))
             (second-cst (parse-artifact->cst second)))
        (check first => second)
        (check (parse-artifact-ref first 'grammarDigest)
               => (parse-artifact-ref second 'grammarDigest))
        (check (parse-artifact-ref first 'sourceDigest)
               => (sha256-text source))
        (check first-cst => second-cst)))
    (test-case "source identity keeps UTF-8 byte extent"
      (let* ((source "λ + @")
             (artifact (parse-source arithmetic-parser source))
             (bytes (string->utf8 source)))
        (check (parse-artifact-ref artifact 'sourceByteLength)
               => (u8vector-length bytes))
        (check (parse-artifact-ref artifact 'sourceDigest)
               => (sha256-text source))
        (check (sha256-bytes bytes) => (sha256-text source))))
    (test-case "token byte coverage preserves every UTF-8 width boundary"
      (let (base (parse-source arithmetic-parser "@"))
        (for-each
         (lambda (row)
           (let* ((text (string (integer->char (car row))))
                  (width (cadr row))
                  (event (vector 'token 0 'raw text 0 width))
                  (artifact
                   (artifact-replace
                    (artifact-replace
                     (artifact-replace base 'sourceDigest (sha256-text text))
                     'sourceByteLength width) 'events (list event))))
             (check (parse-artifact-valid? artifact) => #t)
             (check (parse-artifact-roundtrip artifact) => text)
             ;; Keep advertised coverage self-consistent but falsify UTF-8 width.
             (vector-set! event 5 (+ width 1))
             (check (parse-artifact-valid?
                     (artifact-replace artifact 'sourceByteLength (+ width 1))) => #f)))
         '((0 1) (127 1) (128 2) (2047 2) (2048 3)
           (55295 3) (57344 3) (65535 3) (65536 4) (1114111 4)))))
    (test-case "reconstruction owns its output and ignores untrusted advertised capacity"
      (let* ((source "alpha + 2")
             (artifact (parse-source arithmetic-parser source))
             (view (parse-artifact-roundtrip artifact)))
        (string-set! view 0 #\b)
        (check (parse-artifact-roundtrip artifact) => source)
        (check (parse-artifact-valid? artifact) => #t)
        (check (parse-artifact-valid?
                (artifact-replace artifact 'sourceByteLength #x100000000)) => #f))
      (let (empty (parse-source arithmetic-parser ""))
        (check (parse-artifact-roundtrip empty) => "")))
    (test-case "a missing token event fails closed"
      (let* ((artifact (parse-source arithmetic-parser "1 + 2"))
             (corrupt
              (artifact-replace
               artifact 'events
               (remove-token-id (parse-artifact-events artifact) 2))))
        (check (parse-artifact-valid? corrupt) => #f)))
    (test-case "a duplicate token identity fails closed"
      (let* ((artifact (parse-source arithmetic-parser "1 + 2"))
             (events (parse-artifact-events artifact))
             (first-token
              (let loop ((rest events))
                (if (token-event? (car rest))
                  (car rest)
                  (loop (cdr rest)))))
             (corrupt
              (artifact-replace artifact 'events
                                (cons first-token events))))
        (check (parse-artifact-valid? corrupt) => #f)))
    (test-case "an unbalanced root finish fails closed"
      (let* ((artifact (parse-source arithmetic-parser "1 + 2"))
             (events (parse-artifact-events artifact))
             (corrupt-events
              (reverse
               (cons (vector 'finish-node 0 'SourceFile 4)
                     (cdr (reverse events)))))
             (corrupt
              (artifact-replace artifact 'events corrupt-events)))
        (check (parse-artifact-valid? corrupt) => #f)))
    (test-case "source identity cannot drift from canonical token events"
      (let* ((artifact (parse-source arithmetic-parser "1 + 2"))
             (corrupt
              (artifact-replace artifact 'sourceDigest
                                (sha256-text "different source"))))
        (check (parse-artifact-valid? corrupt) => #f)))
    (test-case "rejected artifacts expose no structural event"
      (let ((artifact (parse-source arithmetic-parser "1 + @")))
        (check (parse-artifact-status artifact) => 'rejected)
        (check (parse-artifact-valid? artifact) => #t)
        (check (let loop ((rest (parse-artifact-events artifact)))
                 (or (null? rest)
                     (and (token-event? (car rest))
                          (loop (cdr rest)))))
               => #t)))))

(export parse-artifact-tests)

;; gxtest discovers only exported names ending in -test.
(def parse-artifact-test parse-artifact-tests)
(export parse-artifact-test)

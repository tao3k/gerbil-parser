#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
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

(deflanguage-parser-tests parse-artifact-tests "ParseArtifact contract"
  (engine)
    (scenario "source-bound admission proves lexemes and identity without roundtrip ownership"
      (each (source) ('("" "alpha + 2" "λ中文🙂@"))
        (scope ((artifact (parse-source arithmetic-parser source)))
          (equal (parse-artifact-valid-for-source? artifact source)  #t)
          (equal (parse-artifact-valid-for-source? artifact (string-append source "x"))  #f)
          (equal (parse-artifact-valid-for-source? artifact #f)  #f)
          (equal (parse-artifact-valid-for-source?
                   (artifact-replace artifact 'sourceByteLength 4294967296) source)  #f)
          (equal (parse-artifact-valid-for-source?
                   (artifact-replace artifact 'sourceDigest (sha256-text "unrelated")) source)  #f)))
      (sequential ((artifact (parse-source arithmetic-parser "λ"))
             (forged (artifact-replace artifact 'sourceDigest (sha256-text "β"))))
        (equal (parse-artifact-valid-for-source? forged "β")  #f)
        (equal (parse-artifact-valid-for-source? artifact "β")  #f)
        (equal (parse-artifact-roundtrip artifact)  "λ")))
    (scenario "canonical SHA-256 identities match fixed digest vectors"
      (equal (sha256-text "")
              "sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855")
      (equal (sha256-text "abc")
              "sha256:ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
      (equal (sha256-text "λ中文🙂")
              "sha256:769868d0dd7575e844f10175e18222eb50c31aacd8f3cce0f3ce55fe2f0e53e2")
      (scope ((bytes (make-u8vector 256)))
        (iterate fill ((index 0))
          (when (< index 256)
            (act (u8vector-set! bytes index index))
            (continue fill (+ index 1))))
        (equal (sha256-bytes bytes)
                "sha256:40aff2e9d2d8922e47afd4648e6967497158785fbd1da870e7110266bf944880")))
    (property "identity and event replay are deterministic"
      (bindings (source "alpha + (2 * beta)") (first (parse-source arithmetic-parser source)) (second (parse-source arithmetic-parser source)) (first-cst (parse-artifact->cst first)) (second-cst (parse-artifact->cst second)))
      (equal first  second)
      (equal (parse-artifact-ref first 'grammarDigest)
                (parse-artifact-ref second 'grammarDigest))
      (equal (parse-artifact-ref first 'sourceDigest)
                (sha256-text source))
      (equal first-cst  second-cst))
    (property "source identity keeps UTF-8 byte extent"
      (bindings (source "λ + @") (artifact (parse-source arithmetic-parser source)) (bytes (string->utf8 source)))
      (equal (parse-artifact-ref artifact 'sourceByteLength)
                (u8vector-length bytes))
      (equal (parse-artifact-ref artifact 'sourceDigest)
                (sha256-text source))
      (equal (sha256-bytes bytes)  (sha256-text source)))
    (scenario "token byte coverage preserves every UTF-8 width boundary"
      (scope ((base (parse-source arithmetic-parser "@")))
        (each (row) ('((0 1) (127 1) (128 2) (2047 2) (2048 3)
           (55295 3) (57344 3) (65535 3) (65536 4) (1114111 4)))
           (sequential ((text (string (integer->char (car row))))
                  (width (cadr row))
                  (event (vector 'token 0 'raw text 0 width))
                  (artifact
                   (artifact-replace
                    (artifact-replace
                     (artifact-replace base 'sourceDigest (sha256-text text))
                     'sourceByteLength width) 'events (list event))))
             (equal (parse-artifact-valid? artifact)  #t)
             (equal (parse-artifact-valid-for-source? artifact text)  #t)
             (equal (parse-artifact-roundtrip artifact)  text)
             ;; Keep advertised coverage self-consistent but falsify UTF-8 width.
             (act (vector-set! event 5 (+ width 1)))
             (equal (parse-artifact-valid?
                     (artifact-replace artifact 'sourceByteLength (+ width 1)))  #f)))))
    (scenario "reconstruction owns its output and ignores untrusted advertised capacity"
      (sequential ((source "alpha + 2")
             (artifact (parse-source arithmetic-parser source))
             (view (parse-artifact-roundtrip artifact)))
        (act (string-set! view 0 #\b))
        (equal (parse-artifact-roundtrip artifact)  source)
        (equal (parse-artifact-valid? artifact)  #t)
        (equal (parse-artifact-valid?
                (artifact-replace artifact 'sourceByteLength #x100000000))  #f))
      (scope ((empty (parse-source arithmetic-parser "")))
        (equal (parse-artifact-roundtrip empty)  "")))
    (property "a missing token event fails closed"
      (bindings (artifact (parse-source arithmetic-parser "1 + 2")) (corrupt
              (artifact-replace
               artifact 'events
               (remove-token-id (parse-artifact-events artifact) 2))))
      (equal (parse-artifact-valid? corrupt)  #f))
    (property "a duplicate token identity fails closed"
      (bindings (artifact (parse-source arithmetic-parser "1 + 2")) (events (parse-artifact-events artifact)) (first-token
              (let loop ((rest events))
                (if (token-event? (car rest))
                  (car rest)
                  (loop (cdr rest))))) (corrupt
              (artifact-replace artifact 'events
                                (cons first-token events))))
      (equal (parse-artifact-valid? corrupt)  #f))
    (property "an unbalanced root finish fails closed"
      (bindings (artifact (parse-source arithmetic-parser "1 + 2")) (events (parse-artifact-events artifact)) (corrupt-events
              (reverse
               (cons (vector 'finish-node 0 'SourceFile 4)
                     (cdr (reverse events))))) (corrupt
              (artifact-replace artifact 'events corrupt-events)))
      (equal (parse-artifact-valid? corrupt)  #f))
    (property "source identity cannot drift from canonical token events"
      (bindings (artifact (parse-source arithmetic-parser "1 + 2")) (corrupt
              (artifact-replace artifact 'sourceDigest
                                (sha256-text "different source"))))
      (equal (parse-artifact-valid? corrupt)  #f))
    (property "rejected artifacts expose no structural event"
      (bindings (artifact (parse-source arithmetic-parser "1 + @")))
      (equal (parse-artifact-status artifact)  'rejected)
      (equal (parse-artifact-valid? artifact)  #t)
      (equal (let loop ((rest (parse-artifact-events artifact)))
                 (or (null? rest)
                     (and (token-event? (car rest))
                          (loop (cdr rest)))))
                #t)))


;; gxtest discovers only exported names ending in -test.
(def parse-artifact-test parse-artifact-tests)
(export parse-artifact-test)

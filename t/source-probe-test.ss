#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import :std/test
        (only-in :gerbil-parser/languages/hcl/parser hcl-parser)
        (only-in :gerbil-parser/src/compiler/machine parser-machine-runtime parser-machine-ir)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 lr-prepare lr-initial-checkpoint lr-checkpoint-feed
                 lr-checkpoint-lexical-mode lr-runtime-lexical-mode-catalog lr-lexical-mode-id)
        (only-in :gerbil-parser/src/runtime/token make-token token-lexeme token-end)
        (only-in :gerbil-parser/src/runtime/probe
                 make-source-probe-cache source-probe-scan source-probe-take!))

(def (value-mode)
  (let (initial (lr-initial-checkpoint (parser-machine-runtime hcl-parser) '()))
    (let-values (((status identifier)
                  (lr-checkpoint-feed initial (make-token 'identifier "value" 0 5))))
      (check status => 'checkpoint)
      (let-values (((status value)
                    (lr-checkpoint-feed identifier (make-token 'punctuation "=" 6 7))))
        (check status => 'checkpoint)
        (lr-checkpoint-lexical-mode value)))))

(def source-probe-test
  (test-suite "request-owned lexical probes"
    (test-case "UTF8 byte and character positions both bind the canonical token"
      (let* ((source " \"λ中😀\"") (mode (value-mode))
             (cache (make-source-probe-cache hcl-parser source)))
        (let-values (((token next-character) (source-probe-scan cache 1 1 mode)))
          (check (token-end token) => 12)
          (check next-character => 6)
          (check (source-probe-take! cache 1 0 mode) => #f)
          (check (source-probe-take! cache 0 1 mode) => #f)
          (let (taken (source-probe-take! cache 1 1 mode))
            (check (eq? (car taken) token) => #t)
            (check (cdr taken) => next-character))
          (check (source-probe-take! cache 1 1 mode) => #f))))
    (test-case "equal numeric mode IDs from another runtime cannot share probes"
      (let* ((mode (value-mode))
             (other (lr-prepare (cdr (assq 'lr-spec (parser-machine-ir hcl-parser)))))
             (other-mode (vector-ref (lr-runtime-lexical-mode-catalog other)
                                     (lr-lexical-mode-id mode)))
             (cache (make-source-probe-cache hcl-parser "001")))
        (check (lr-lexical-mode-id other-mode) => (lr-lexical-mode-id mode))
        (let-values (((token next) (source-probe-scan cache 0 0 mode)))
          (check (source-probe-take! cache 0 0 other-mode) => #f)
          (check (eq? (car (source-probe-take! cache 0 0 mode)) token) => #t))))
    (test-case "source requests own distinct bounded slots"
      (let* ((mode (value-mode))
             (first (make-source-probe-cache hcl-parser "001"))
             (second (make-source-probe-cache hcl-parser "002")))
        (let-values (((token next) (source-probe-scan first 0 0 mode)))
          (check (source-probe-take! second 0 0 mode) => #f))
        (let-values (((token next) (source-probe-scan second 0 0 mode)))
          (check (token-lexeme (car (source-probe-take! second 0 0 mode))) => "002"))
        (check (token-lexeme (car (source-probe-take! first 0 0 mode))) => "001")))
    (test-case "future probes survive trivia and passed probes expire"
      (let* ((mode (value-mode))
             (cache (make-source-probe-cache hcl-parser " 001")))
        (let-values (((token next) (source-probe-scan cache 1 1 mode)))
          (check (source-probe-take! cache 0 0 mode) => #f)
          (check (eq? (car (source-probe-take! cache 1 1 mode)) token) => #t))
        (let-values (((token next) (source-probe-scan cache 1 1 mode)))
          (check (source-probe-take! cache 4 4 mode) => #f)
          (check (source-probe-take! cache 1 1 mode) => #f))))
    (test-case "scanner exceptions propagate and clear earlier successful probes"
      (let* ((mode (value-mode))
             (cache (make-source-probe-cache hcl-parser "001")))
        (let-values (((token next) (source-probe-scan cache 0 0 mode)))
          (check (with-catch (lambda (_) #t)
                   (lambda () (source-probe-scan cache -1 -1 mode) #f)) => #t)
          (check (source-probe-take! cache 0 0 mode) => #f))))))
(export source-probe-test)

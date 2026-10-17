#!/usr/bin/env gxi
(import :gerbil-parser/language-test-support)
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

(defparser-check (value-mode)
  (scope ((initial (lr-initial-checkpoint (parser-machine-runtime hcl-parser) '())))
    (scope-values (((status identifier)
                  (lr-checkpoint-feed initial (make-token 'identifier "value" 0 5))))
      (equal status  'checkpoint)
      (scope-values (((status value)
                    (lr-checkpoint-feed identifier (make-token 'punctuation "=" 6 7))))
        (equal status  'checkpoint)
        (act (lr-checkpoint-lexical-mode value))))))

(deflanguage-parser-tests source-probe-test "request-owned lexical probes"
  (engine)
    (scenario "UTF8 byte and character positions both bind the canonical token"
      (sequential ((source " \"λ中😀\"") (mode (parser-check-result value-mode ))
             (cache (make-source-probe-cache hcl-parser source)))
        (scope-values (((token next-character) (source-probe-scan cache 1 1 mode)))
          (equal (token-end token)  12)
          (equal next-character  6)
          (equal (source-probe-take! cache 1 0 mode)  #f)
          (equal (source-probe-take! cache 0 1 mode)  #f)
          (scope ((taken (source-probe-take! cache 1 1 mode)))
            (equal (eq? (car taken) token)  #t)
            (equal (cdr taken)  next-character))
          (equal (source-probe-take! cache 1 1 mode)  #f))))
    (scenario "equal numeric mode IDs from another runtime cannot share probes"
      (sequential ((mode (parser-check-result value-mode ))
             (other (lr-prepare (cdr (assq 'lr-spec (parser-machine-ir hcl-parser)))))
             (other-mode (vector-ref (lr-runtime-lexical-mode-catalog other)
                                     (lr-lexical-mode-id mode)))
             (cache (make-source-probe-cache hcl-parser "001")))
        (equal (lr-lexical-mode-id other-mode)  (lr-lexical-mode-id mode))
        (scope-values (((token next) (source-probe-scan cache 0 0 mode)))
          (equal (source-probe-take! cache 0 0 other-mode)  #f)
          (equal (eq? (car (source-probe-take! cache 0 0 mode)) token)  #t))))
    (scenario "source requests own distinct bounded slots"
      (sequential ((mode (parser-check-result value-mode ))
             (first (make-source-probe-cache hcl-parser "001"))
             (second (make-source-probe-cache hcl-parser "002")))
        (scope-values (((token next) (source-probe-scan first 0 0 mode)))
          (equal (source-probe-take! second 0 0 mode)  #f))
        (scope-values (((token next) (source-probe-scan second 0 0 mode)))
          (equal (token-lexeme (car (source-probe-take! second 0 0 mode)))  "002"))
        (equal (token-lexeme (car (source-probe-take! first 0 0 mode)))  "001")))
    (scenario "future probes survive trivia and passed probes expire"
      (sequential ((mode (parser-check-result value-mode ))
             (cache (make-source-probe-cache hcl-parser " 001")))
        (scope-values (((token next) (source-probe-scan cache 1 1 mode)))
          (equal (source-probe-take! cache 0 0 mode)  #f)
          (equal (eq? (car (source-probe-take! cache 1 1 mode)) token)  #t))
        (scope-values (((token next) (source-probe-scan cache 1 1 mode)))
          (equal (source-probe-take! cache 4 4 mode)  #f)
          (equal (source-probe-take! cache 1 1 mode)  #f))))
    (scenario "scanner exceptions propagate and clear earlier successful probes"
      (sequential ((mode (parser-check-result value-mode ))
             (cache (make-source-probe-cache hcl-parser "001")))
        (scope-values (((token next) (source-probe-scan cache 0 0 mode)))
          (equal (with-catch (lambda (_) #t)
                   (lambda () (source-probe-scan cache -1 -1 mode) #f))  #t)
          (equal (source-probe-take! cache 0 0 mode)  #f)))))

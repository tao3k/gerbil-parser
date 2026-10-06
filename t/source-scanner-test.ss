;;; Engine checkpoints are owned by the creating worker, including shared text.
(import :std/test
        (only-in :gerbil-parser/src/runtime/source-scanner
                 make-source-scanner source-scanner-tokens source-scanner-initial-state source-scanner-step
                 source-scan-state-with-context source-scan-state-byte-offset)
        (only-in :gerbil-parser/src/runtime/token token-start token-end token-kind token-lexeme)
        (only-in :gerbil-parser/src/runtime/source-scanner source-scan-state-context)
        (only-in :gerbil-parser/src/runtime/contextual-scanner
                 +empty-delimiter-queue+ delimiter-queue-enqueue delimiter-queue-take
                 delimiter-queue-list delimiter-obligation-marker delimiter-obligation-quoted?
                 contextual-scan-state-canonical decode-marker)
        (only-in :gerbil-parser/src/language/source source-language-scanner-factory))
(import (only-in :gerbil-parser/languages/bash/grammar bash-source-language))
(def (pending-markers state)
 (map car (cdr (assq 'pending (contextual-scan-state-canonical state)))))
(def (rejected? thunk)
  (with-catch (lambda (_) #t) (lambda () (thunk) #f)))
(def source-scanner-test
  (test-suite "source scanner worker ownership"
    (test-case "shared FIFO preserves order and branches without copying the pending prefix"
      (let* ((a (decode-marker 'raw "A" #f)) (b (decode-marker 'raw "B" #f))
             (first (delimiter-queue-enqueue +empty-delimiter-queue+ a))
             (second (delimiter-queue-enqueue first b)))
        (check (map delimiter-obligation-marker (delimiter-queue-list first)) => '("A"))
        (let-values (((head rest) (delimiter-queue-take second)))
          (check (delimiter-obligation-marker head) => "A")
          (check (map delimiter-obligation-marker (delimiter-queue-list rest)) => '("B"))
          (check (map delimiter-obligation-marker (delimiter-queue-list second)) => '("A" "B")))
        (check (rejected? (lambda () (delimiter-queue-enqueue first 'callback))) => #t)
        (check (rejected? (lambda () (delimiter-queue-take +empty-delimiter-queue+))) => #t)))
    (test-case "Bash uses engine quote removal including preserved double-quote escapes"
      (for-each
       (lambda (row)
         (let (marker (decode-marker 'shell-quote-removal (car row) #f))
           (check (delimiter-obligation-marker marker) => (cadr row))
           (check (delimiter-obligation-quoted? marker) => (caddr row))))
       '(("A" "A" #f) ("'A'" "A" #t) ("\"a\\q\"" "a\\q" #t)
         ("\"a\\$\"" "a$" #t) ("'a\\q'" "a\\q" #t)
         ("A\\\nB" "AB" #f) ("''" "" #t)))
      (check (rejected? (lambda () (decode-marker 'shell-quote-removal "" #f))) => #t)
      (check (rejected? (lambda () (decode-marker 'shell-quote-removal "'A" #f))) => #t)
      (check (rejected? (lambda () (decode-marker 'arbitrary "A" #f))) => #t))
    (test-case "Bash queued checkpoint replay retains its own pending obligations"
      (let* ((worker ((source-language-scanner-factory bash-source-language) "<<A <<B\nα\nA\nβ\nB\n"))
             (initial (source-scanner-initial-state worker)))
        (let-values (((open after-open) (source-scanner-step worker initial 'command)))
          (let-values (((marker queued) (source-scanner-step worker after-open 'command)))
            (check (pending-markers (source-scan-state-context queued)) => '("A"))
            (let-values (((again replay) (source-scanner-step worker after-open 'command)))
              (check (token-lexeme again) => "A")
              (check (pending-markers (source-scan-state-context replay)) => '("A")))
            (check (pending-markers (source-scan-state-context initial)) => '())))))
    (test-case "large Bash deferred batches drain in declaration order"
      (let* ((markers (map (lambda (n) (string-append "END" (number->string n))) (iota 128)))
             (source (string-append "cat "
                       (apply string-append (map (lambda (name) (string-append "<<" name " ")) markers))
                       "\n" (apply string-append (map (lambda (name) (string-append "α\n" name "\n")) markers))))
             (tokens (source-scanner-tokens ((source-language-scanner-factory bash-source-language) source) 'command)))
        (check (map token-lexeme (filter (lambda (token) (eq? (token-kind token) 'heredoc-marker)) tokens)) => markers)
        (check (map token-lexeme (filter (lambda (token) (eq? (token-kind token) 'heredoc-end)) tokens))
               => (map (lambda (name) (string-append name "\n")) markers))
        (check (apply string-append (map token-lexeme tokens)) => source)))
    (test-case "another worker cannot use a checkpoint over the exact same string"
      (let* ((source "αx") (calls 0)
             (step (lambda (_source offset context _mode)
                     (set! calls (+ calls 1))
                     (values 'character (+ offset 1) context)))
             (left (make-source-scanner source 'left step))
             (right (make-source-scanner source 'right step))
             (initial (source-scanner-initial-state left)))
        (check (rejected? (lambda () (source-scanner-step right initial 'test))) => #t)
        (check calls => 0)
        (check (rejected? (lambda ()
                           (source-scanner-step right (source-scan-state-with-context initial 'changed) 'test))) => #t)
        (check calls => 0)))
    (test-case "owned checkpoint replay and context updates preserve UTF-8 spans"
      (let* ((worker (make-source-scanner "αx" 'initial
                      (lambda (_source offset context _mode)
                        (if (= offset 2) (values #f offset context)
                          (values 'character (+ offset 1) context)))))
             (initial (source-scanner-initial-state worker)))
        (let-values (((first next) (source-scanner-step worker initial 'test)))
          (check (list (token-start first) (token-end first)) => '(0 2))
          (check (source-scan-state-byte-offset initial) => 0)
          (let-values (((again replay) (source-scanner-step worker initial 'test)))
            (check (list (token-start again) (token-end again)) => '(0 2))
            (check (source-scan-state-byte-offset replay) => 2))
          (let-values (((second end) (source-scanner-step worker (source-scan-state-with-context next 'updated) 'test)))
            (check (list (token-start second) (token-end second)) => '(2 3))
            (let-values (((eof final) (source-scanner-step worker end 'test)))
              (check eof => #f)
              (check (source-scan-state-byte-offset final) => 3))))))))
(export source-scanner-test)

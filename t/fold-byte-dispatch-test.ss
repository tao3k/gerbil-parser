(import :std/test ../src/compiler/event-fold-byte-dispatch)
(export fold-byte-dispatch-test)
(def fold-byte-dispatch-test
  (test-suite "Prepared byte dispatch effects"
    (test-case "distinct bytes select only relevant ordered state branches"
      (let ((plan (fold-byte-dispatch
        '(if (and (state ready) (line-byte-equal? (line-index cursor) 97))
             ((set-bool ready (bool #f)))
             ((if (line-byte-equal? (line-index cursor) 98)
                  ((set-bool ready (bool #t)))
                  ((if (line-byte-equal? (line-index cursor) 97)
                       ((token Line start end)) ()))))) 'cursor)))
        (check (map car (vector-ref plan 0)) => '(97 98))
        (check (cdr (assv 98 (vector-ref plan 0))) => '((set-bool ready (bool #t))))
        (check (cdr (assv 97 (vector-ref plan 0)))
          => '((if (and (state ready)) ((set-bool ready (bool #f))) ((token Line start end)))))
        (check (vector-ref plan 1) => '())))
    (test-case "effects and non-loop positions cannot select byte regions"
      (let* ((forms '((if (and (state ready) (line-byte-equal? (line-index cursor) 97))
                          ((set-bool ready (bool #f))) ((token Line start end)))
                     (if (line-byte-equal? (line-index cursor) 98)
                         ((set-bool ready (bool #t))) ())))
             (regions (fold-byte-regions forms 'cursor)))
        (check (map car (vector-ref regions 0)) => '((97) (98)))
        (check (vector-ref regions 1) => '((token Line start end)))
        (check (cdr (cadr (vector-ref regions 0)))
               => '((token Line start end)
                    (if (line-byte-equal? (line-index cursor) 98)
                        ((set-bool ready (bool #t))) ())))
        (for-each
         (lambda (blocked)
           (check (fold-byte-regions
                   (list `(if ,blocked ((token Line start end)) ())) 'cursor) => #f))
         '((and (future-heading-title? "*" " " (uint 1) "END")
                (line-byte-equal? (line-index cursor) 97))
           (line-byte-equal? start 97)))
        ;; Repeated byte classes share one ordered body, not duplicate machines.
        (let (plan (fold-byte-regions
                    '((if (line-bytes-any-in? (line-index cursor)
                            (line-step (line-index cursor)) (97 98))
                          ((token Line start end)) ())) 'cursor))
          (check (map car (vector-ref plan 0)) => '((97 98)))))
      (let ((nested
             '((join-once handled
                  ((if (state ready)
                       ((if (line-byte-equal? (line-index cursor) 97)
                            ((set-bool handled (bool #t))) ())) ()))
                  ((with-source-bounds start end
                     ((if (line-byte-equal? (line-index cursor) 98)
                          ((token Line start end)) ()))))))))
        (let (plan (fold-byte-regions nested 'cursor))
          (check (map car (vector-ref plan 0)) => '((97)))
          (check (vector-ref plan 1)
                 => '((join-once handled ((if (state ready) () ()))
                       ((with-source-bounds start end
                          ((if (line-byte-equal? (line-index cursor) 98)
                               ((token Line start end)) ())))))))))
      (let (choice '(or (and (line-byte-equal? (line-index cursor) 97)
                            (line-byte-equal? (line-step (line-index cursor)) 98))
                       (and (line-byte-equal? (line-index cursor) 99)
                            (line-byte-equal? (line-step (line-index cursor)) 100))))
        (check (fold-necessary-bytes choice 'cursor) => '(97 99))
        (check (fold-byte-prefilter `(and (state active) ,choice) 'cursor)
          => `(and (line-bytes-any-in? (line-index cursor)
                      (line-step (line-index cursor)) (97 99))
                   (state active) ,choice)))
      (check (fold-necessary-bytes
        '(or (line-byte-equal? (line-index cursor) 97) (bool #t)) 'cursor) => #f)
      (check (fold-necessary-bytes
        '(or (and (future-heading-title? "*" " " (uint 1) "END")
                  (line-byte-equal? (line-index cursor) 97))
             (line-byte-equal? (line-index cursor) 98)) 'cursor) => #f)
      (check (fold-byte-prefilter
        '(and (state first) (and (state second) (and (state third)
              (line-byte-equal? (line-index cursor) 97))) (state last)) 'cursor)
        => '(and (line-byte-equal? (line-index cursor) 97)
                 (state first) (state second) (state third) (state last)))
      (check (fold-byte-prefilter
        '(and (state active)
              (and (and (line-byte-equal? (line-index cursor) 104)
                        (line-byte-equal? (line-step (line-index cursor)) 116))
                   (line-byte-equal? (line-step (line-step (line-index cursor))) 116)))
        'cursor)
        => '(and (line-byte-equal? (line-index cursor) 104) (state active)
                 (line-byte-equal? (line-step (line-index cursor)) 116)
                 (line-byte-equal? (line-step (line-step (line-index cursor))) 116)))
      (check (fold-byte-prefilter
        '(and (state active)
              (and (future-heading-title? "*" " " (uint 1) "END")
                   (line-byte-equal? (line-index cursor) 104))) 'cursor) => #f)
      (check (fold-byte-prefilter
        '(and (state active) (line-bytes-any-in? (line-index cursor)
                  (line-step (line-index cursor)) (36 92)) (state ready)) 'cursor)
        => '(and (line-bytes-any-in? (line-index cursor)
                  (line-step (line-index cursor)) (36 92)) (state active) (state ready)))
      (check (fold-byte-prefilter
        '(and (future-heading-title? "*" " " (uint 1) "END")
              (line-byte-equal? (line-index cursor) 97)) 'cursor) => #f)
      (check (fold-byte-prefilter
        '(and (state active) (line-bytes-any-in? start end (36))) 'cursor) => #f)
      (check (fold-byte-dispatch
        '(if (and (future-heading-title? "*" " " (uint 1) "END")
                  (line-byte-equal? (line-index cursor) 97)) ()
             ((if (line-byte-equal? (line-index cursor) 98) () ()))) 'cursor) => #f)
      (check (fold-byte-dispatch
        '(if (line-byte-equal? start 97) () ((if (line-byte-equal? start 98) () ())))
        'cursor) => #f))))

(import :gerbil-parser/language-test-support)
(import :std/test ../src/compiler/event-fold-byte-dispatch)

(deflanguage-parser-tests fold-byte-dispatch-test "Prepared byte dispatch effects"
  (engine)
    (scenario "distinct bytes select only relevant ordered state branches"
      (scope ((plan (fold-line-negative-region
                  '((if (and (state active) (line-starts-with "#+"))
                        ((set-bool active (bool #f))) ((token Line start end)))
                    (if (line-has-word-after-prefix? "Clock:")
                        ((finish-node)) ())))))
        (equal (vector-ref plan 0)  '((start 67 108 111 99 107 58) (start 35 43)))
        (equal (vector-ref plan 1)
           '((if (and (%line-prefix-hit) (and (state active) (line-starts-with "#+")))
                   ((set-bool active (bool #f))) ((token Line start end)))
               (if (and (%line-prefix-hit) (line-has-word-after-prefix? "Clock:"))
                   ((finish-node)) ()))))
      (each (predicate) ('((and (future-heading-title? "*" " " (uint 1) "END") (line-starts-with "#"))
         (line-starts-with "")))
         (equal (fold-line-negative-region
                  (list `(if ,predicate ((token Line start end)) ())))  #f))
      (equal (fold-line-negative-region
               '((with-source-bounds start end
                    ((if (line-starts-with "#") ((token Line start end)) ())))))  #f)
      (scope ((plan (fold-line-negative-region
                  '((if (and (state active)
                             (line-byte-equal? (line-skip-horizontal start) 35))
                        ((token Line start end)) ())))))
        (equal (vector-ref plan 0)  '((horizontal 35))))
      (equal (fold-line-negative-region
               '((if (and (future-heading-title? "*" " " (uint 1) "END")
                          (line-byte-equal? (line-skip-horizontal start) 35))
                     ((token Line start end)) ())))  #f)
      (scope ((plan (fold-byte-dispatch
        '(if (and (state ready) (line-byte-equal? (line-index cursor) 97))
             ((set-bool ready (bool #f)))
             ((if (line-byte-equal? (line-index cursor) 98)
                  ((set-bool ready (bool #t)))
                  ((if (line-byte-equal? (line-index cursor) 97)
                       ((token Line start end)) ()))))) 'cursor)))
        (equal (map car (vector-ref plan 0))  '(97 98))
        (equal (cdr (assv 98 (vector-ref plan 0)))  '((set-bool ready (bool #t))))
        (equal (cdr (assv 97 (vector-ref plan 0)))
           '((if (and (state ready)) ((set-bool ready (bool #f))) ((token Line start end)))))
        (equal (vector-ref plan 1)  '())))
    (scenario "effects and non-loop positions cannot select byte regions"
      (sequential ((forms '((if (and (state ready) (line-byte-equal? (line-index cursor) 97))
                          ((set-bool ready (bool #f))) ((token Line start end)))
                     (if (line-byte-equal? (line-index cursor) 98)
                         ((set-bool ready (bool #t))) ())))
             (regions (fold-byte-regions forms 'cursor)))
        (equal (map car (vector-ref regions 0))  '((97) (98)))
        (equal (vector-ref regions 1)  '((token Line start end)))
        (equal (cdr (cadr (vector-ref regions 0)))
                '((token Line start end)
                    (if (line-byte-equal? (line-index cursor) 98)
                        ((set-bool ready (bool #t))) ())))
        (each (blocked) ('((and (future-heading-title? "*" " " (uint 1) "END")
                (line-byte-equal? (line-index cursor) 97))
           (line-byte-equal? start 97)))
           (equal (fold-byte-regions
                   (list `(if ,blocked ((token Line start end)) ())) 'cursor)  #f))
        ;; Repeated byte classes share one ordered body, not duplicate machines.
        (scope ((plan (fold-byte-regions
                    '((if (line-bytes-any-in? (line-index cursor)
                            (line-step (line-index cursor)) (97 98))
                          ((token Line start end)) ())) 'cursor)))
          (equal (map car (vector-ref plan 0))  '((97 98)))))
      (scope ((nested
             '((join-once handled
                  ((if (state ready)
                       ((if (line-byte-equal? (line-index cursor) 97)
                            ((set-bool handled (bool #t))) ())) ()))
                  ((with-source-bounds start end
                     ((if (line-byte-equal? (line-index cursor) 98)
                          ((token Line start end)) ()))))))))
        (scope ((plan (fold-byte-regions nested 'cursor)))
          (equal (map car (vector-ref plan 0))  '((97)))
          (equal (vector-ref plan 1)
                  '((join-once handled ((if (state ready) () ()))
                       ((with-source-bounds start end
                          ((if (line-byte-equal? (line-index cursor) 98)
                               ((token Line start end)) ())))))))))
      (scope ((choice '(or (and (line-byte-equal? (line-index cursor) 97)
                            (line-byte-equal? (line-step (line-index cursor)) 98))
                       (and (line-byte-equal? (line-index cursor) 99)
                            (line-byte-equal? (line-step (line-index cursor)) 100)))))
        (equal (fold-necessary-bytes choice 'cursor)  '(97 99))
        (equal (fold-byte-prefilter `(and (state active) ,choice) 'cursor)
           `(and (line-bytes-any-in? (line-index cursor)
                      (line-step (line-index cursor)) (97 99))
                   (state active) ,choice)))
      (equal (fold-necessary-bytes
        '(or (line-byte-equal? (line-index cursor) 97) (bool #t)) 'cursor)  #f)
      (equal (fold-necessary-bytes
        '(or (and (future-heading-title? "*" " " (uint 1) "END")
                  (line-byte-equal? (line-index cursor) 97))
             (line-byte-equal? (line-index cursor) 98)) 'cursor)  #f)
      (equal (fold-byte-prefilter
        '(and (state first) (and (state second) (and (state third)
              (line-byte-equal? (line-index cursor) 97))) (state last)) 'cursor)
         '(and (line-byte-equal? (line-index cursor) 97)
                 (state first) (state second) (state third) (state last)))
      (equal (fold-byte-prefilter
        '(and (state active)
              (and (and (line-byte-equal? (line-index cursor) 104)
                        (line-byte-equal? (line-step (line-index cursor)) 116))
                   (line-byte-equal? (line-step (line-step (line-index cursor))) 116)))
        'cursor)
         '(and (line-byte-equal? (line-index cursor) 104) (state active)
                 (line-byte-equal? (line-step (line-index cursor)) 116)
                 (line-byte-equal? (line-step (line-step (line-index cursor))) 116)))
      (equal (fold-byte-prefilter
        '(and (state active)
              (and (future-heading-title? "*" " " (uint 1) "END")
                   (line-byte-equal? (line-index cursor) 104))) 'cursor)  #f)
      (equal (fold-byte-prefilter
        '(and (state active) (line-bytes-any-in? (line-index cursor)
                  (line-step (line-index cursor)) (36 92)) (state ready)) 'cursor)
         '(and (line-bytes-any-in? (line-index cursor)
                  (line-step (line-index cursor)) (36 92)) (state active) (state ready)))
      (equal (fold-byte-prefilter
        '(and (future-heading-title? "*" " " (uint 1) "END")
              (line-byte-equal? (line-index cursor) 97)) 'cursor)  #f)
      (equal (fold-byte-prefilter
        '(and (state active) (line-bytes-any-in? start end (36))) 'cursor)  #f)
      (equal (fold-byte-dispatch
        '(if (and (future-heading-title? "*" " " (uint 1) "END")
                  (line-byte-equal? (line-index cursor) 97)) ()
             ((if (line-byte-equal? (line-index cursor) 98) () ()))) 'cursor)  #f)
      (equal (fold-byte-dispatch
        '(if (line-byte-equal? start 97) () ((if (line-byte-equal? start 98) () ())))
        'cursor)  #f)))

(import :std/test ../src/compiler/event-fold-shared-probes)
(export fold-shared-probes-test)
(def (condition members (from 'start))
  `(line-bytes-any-in? ,from (line-content-end) ,members))
(def (decision members body (from 'start))
  `(if ,(condition members from) ,body ()))
(def fold-shared-probes-test
  (test-suite "Shared source probes"
    (test-case "one byte class preserves independent ordered decisions"
      (let* ((forms (list (decision '(36 92) '((set-uint saved (uint 1))))
                          (decision '(36 97) '((token Line start end)))))
             (plan (car (fold-fuse-byte-probes forms))))
        (check (car plan) => '%shared-byte-probes)
        (check (cadr plan) => (car forms))
        (check (caddr plan) => (cadr forms))
        (check (map (cut u8vector-ref (list-ref plan 3) <>) '(36 92 97 98))
               => '(3 1 2 0))
        (check (list-ref plan 4) => 3)
        (check (length (fold-fuse-byte-probes
                (list (decision '(97)
                         '((for-line-bytes cursor start end
                            ((if (line-bytes-any-in? (line-index cursor)
                                   (line-step (line-index cursor)) (97))
                                 ((set-uint saved (uint 1))) ()))))
                      (decision '(98) '()))))) => 1)
        (check (list-ref (car (fold-fuse-byte-probes
                       (list (decision '() '()) (decision '(97) '())))) 4) => 2)))
    (test-case "range writes helpers and frame changes prevent fusion"
      (for-each
        (lambda (body)
          (let (forms (list (decision '(97) body '(state-offset saved))
                            (decision '(98) '() '(state-offset saved))))
            (check (fold-fuse-byte-probes forms) => forms)))
        '(((set-uint saved (uint 1)))
          ((call-source-helper slice start end))
          ((with-source-bounds start end ())))))))

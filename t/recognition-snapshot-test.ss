#!/usr/bin/env gxi
;;; -*- Gerbil -*-

(import :std/test
        (only-in :gerbil-parser/languages/arithmetic/v1/parser arithmetic-parser parse-arithmetic-v1)
        (only-in :gerbil-parser/languages/hcl/v2-24/parser hcl-v2-24-parser parse-hcl-v2-24)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success?)
        (only-in :gerbil-parser/src/runtime/funcs recognition-sequence->list)
        (only-in :gerbil-parser/languages/tla-plus/parser tla-plus-layout-parser)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 current-lr-recognition-observer lr-prepare lr-parse/prepared
                 lr-recognition-fragment? lr-recognition-fragment-children lr-recognition-fragment-end
                 lr-recognition-fragment-token-count lr-recognition-fragment-value
                 lr-recognition-project)
        (only-in :gerbil-parser/src/runtime/incremental
                 make-incremental-session incremental-session-artifact
                 incremental-session-recognition-root incremental-session-project-artifact
                 parse-incremental-session apply-edit make-edit))

(def (check-projection machine source session)
  (let* ((root (incremental-session-recognition-root session))
         (projected (and root (lr-recognition-project root))))
    (check (lr-recognition-fragment? root) => #t)
    (check (recognition-sequence->list projected)
           => (recognition-sequence->list (lr-recognition-fragment-value root)))
    (check (incremental-session-project-artifact session)
           => (incremental-session-artifact session))))

(def (retained-prefix-piece root bound)
  (let loop ((pending (list root)))
    (and (pair? pending)
         (let (piece (car pending))
           (cond
            ((not (lr-recognition-fragment? piece)) (loop (cdr pending)))
            ((and (<= (lr-recognition-fragment-end piece) bound)
                  (> (lr-recognition-fragment-token-count piece) 1)) piece)
            (else (loop (append (lr-recognition-fragment-children piece)
                                (cdr pending)))))))))

(def (contains-identical-piece? root expected)
  (let loop ((pending (list root)))
    (and (pair? pending)
         (or (eq? (car pending) expected)
             (loop (append
                    (if (lr-recognition-fragment? (car pending))
                      (lr-recognition-fragment-children (car pending)) '())
                    (cdr pending)))))))

(def recognition-snapshot-test
  (test-suite "private grammar recognition snapshots"
    (test-case "ordinary sessions allocate no grammar snapshot"
      (check (incremental-session-recognition-root
              (make-incremental-session arithmetic-parser "1 + 2")) => #f))
    (test-case "grammar structure independently projects complete artifacts"
      (for-each
       (lambda (row)
         (check-projection (car row) (cdr row)
                           (make-incremental-session (car row) (cdr row) #t)))
       (list (cons arithmetic-parser "001 + 002 * (003 + 004)")
             (cons hcl-v2-24-parser "value = 001\nother = { nested = [1, 2] }\n")
             (cons hcl-v2-24-parser "value = \"λ中😀\" /* retained trivia */\n")
             (cons hcl-v2-24-parser ""))))
    (test-case "captured prefix and successive topology changes remain projectable"
      (let ((source (apply string-append (make-list 80 "value = 001\n")))
                 (session (make-incremental-session
                           hcl-v2-24-parser
                           (apply string-append (make-list 80 "value = 001\n")) #t)))
        (for-each
         (lambda (edit)
           (let-values (((next ignored) (parse-incremental-session session edit)))
             (set! source (apply-edit source edit))
             (set! session next)
             (check (incremental-session-artifact session) => (parse-hcl-v2-24 source))
             (check-projection hcl-v2-24-parser source session)))
         (list (make-edit (* 40 12) 0 "other = 002\n")
               (make-edit (* 40 12) 12 "")
               (make-edit 0 0 "other = 002\n")
               (make-edit 0 12 "")))))
    (test-case "resume physically shares unchanged grammar prefix fragments"
      (let* ((source (apply string-append (make-list 80 "value = 001\n")))
             (session (make-incremental-session hcl-v2-24-parser source #t))
             (piece (retained-prefix-piece (incremental-session-recognition-root session) 120)))
        (check (lr-recognition-fragment? piece) => #t)
        (let-values (((next ignored)
                      (parse-incremental-session session (make-edit 480 0 "other = 002\n"))))
          (check (contains-identical-piece? (incremental-session-recognition-root next) piece)
                 => #t))))
    (test-case "deep left recursion projects without recursive Scheme traversal"
      (let* ((source (string-join (make-list 1600 "001") " + "))
             (session (make-incremental-session arithmetic-parser source #t)))
        (check-projection arithmetic-parser source session)
        (check (lr-recognition-fragment-token-count
                (incremental-session-recognition-root session)) => 3199)))
    (test-case "event-only fast paths discard stale grammar snapshots"
      (let ((session (make-incremental-session arithmetic-parser "001 + 002" #t)))
        (let-values (((next ignored) (parse-incremental-session session (make-edit 6 3 "003"))))
          (check (incremental-session-artifact next) => (parse-arithmetic-v1 "001 + 003"))
          (check (incremental-session-recognition-root next) => #f)
          (let-values (((resumed ignored)
                        (parse-incremental-session next (make-edit 0 0 "004 + "))))
            (check-projection arithmetic-parser "004 + 001 + 003" resumed)))))
    (test-case "selective GLR cannot publish a deterministic reuse root"
      (let ((observed 'not-called)
            (runtime
             (lr-prepare
              (compile-lr-spec
               '((source-file (choice (reference first-path) (reference second-path)))
                 (first-path (alias SourceFile (field value (token identifier))))
                 (second-path (alias SourceFile (field value (token identifier)))))
               'source-file 'selective-glr))))
        (parameterize ((current-lr-recognition-observer
                        (lambda (root) (set! observed root))))
          (let-values (((root rest)
                        (lr-parse/prepared runtime (list (make-token 'identifier "x" 0 1)))))
            (check rest => '())))
        (check observed => #f)))
    (test-case "layout execution retains only its authoritative artifact"
      (let (session
            (make-incremental-session
             tla-plus-layout-parser
             "---- MODULE J ----\nInit ==\n  /\\ TRUE\n  /\\ FALSE\n====\n" #t))
        (check (parse-artifact-success? (incremental-session-artifact session)) => #t)
        (check (incremental-session-recognition-root session) => #f)))
    (test-case "invalid input cannot retain a reusable root"
      (let (session (make-incremental-session arithmetic-parser "1 +" #t))
        (check (parse-artifact-success? (incremental-session-artifact session)) => #f)
        (check (incremental-session-recognition-root session) => #f)))))

(export recognition-snapshot-test)

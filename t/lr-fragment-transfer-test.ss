;;; -*- Gerbil -*-
;;; Exact terminal admission at the authoritative LR transfer boundary.
(import :std/test
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/runtime/token make-token token-kind)
        (only-in :gerbil-parser/src/runtime/artifact make-success-parse-artifact)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 current-lr-recognition-observer lr-prepare lr-parse/prepared
                 lr-initial-checkpoint lr-checkpoint-before-shift
                 lr-checkpoint-fragment-compatible?
                 lr-checkpoint-inject-fragment lr-checkpoint-resume))
(export lr-fragment-transfer-test)
(def lr-fragment-transfer-test
  (test-suite "LR fragment transfer admission"
    (test-case "LR transfer validates terminal yield and the current input boundary"
      (let* ((runtime (lr-prepare
                       (compile-lr-spec
                        '((source-file (alias SourceFile
                                        (sequence (token identifier) (token identifier)))))
                        'source-file)))
             (old (list (make-token 'identifier "猫" 0 3)
                        (make-token 'identifier "b" 4 5)))
             (captured #f))
        (parameterize ((current-lr-recognition-observer
                        (lambda (root) (set! captured root))))
          (let-values (((root rest) (lr-parse/prepared runtime old)))
            (check rest => '())))
        (let* ((tokens (list (make-token 'identifier "猫" 7 10)
                            (make-token 'identifier "b" 11 12)))
               (checkpoint (lr-checkpoint-before-shift
                            (lr-initial-checkpoint runtime '()) (car tokens))))
          (check (lr-checkpoint-fragment-compatible? checkpoint captured) => #t)
          (let-values (((root rest)
                        (lr-checkpoint-resume
                         (lr-checkpoint-inject-fragment checkpoint captured 7 tokens))))
            (check rest => '())
            (let-values (((fresh fresh-rest) (lr-parse/prepared runtime tokens)))
              (def (publish value)
                (make-success-parse-artifact
                 "transfer-test" "       猫 b"
                 (list (make-token 'space "       " 0 7) (car tokens)
                       (make-token 'space " " 10 11) (cadr tokens))
                 value (lambda (token) (eq? (token-kind token) 'space))))
              (check (publish root) => (publish fresh))))
          (for-each
           (lambda (wrong)
             (check-exception
              (lr-checkpoint-inject-fragment checkpoint captured 7 wrong) true))
           (list (list (car tokens) (make-token 'identifier "c" 11 12))
                 (list (car tokens) (make-token 'identifier "b" 12 13))
                 (list (car tokens) (make-token 'number "b" 11 12))
                 (list (car tokens))
                 (list (car tokens) (cadr tokens) (cadr tokens))))
          (check-exception
           (lr-checkpoint-inject-fragment checkpoint captured 0 old) true)
          (check-exception
           (lr-checkpoint-inject-fragment
            (lr-checkpoint-before-shift (lr-initial-checkpoint runtime '())
                                       (make-token 'identifier "犬" 7 10))
            captured 7 tokens) true)
          ;; Rejection does not consume or mutate the certified continuation.
          (let-values (((root rest)
                        (lr-checkpoint-resume
                         (lr-checkpoint-inject-fragment checkpoint captured 7 tokens))))
            (check rest => '())))))
))

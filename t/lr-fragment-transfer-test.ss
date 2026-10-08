;;; -*- Gerbil -*-
;;; Exact terminal admission at the authoritative LR transfer boundary.
(import :std/test
        (only-in :gerbil-parser/src/runtime/funcs recognition-sequence->list)
        (only-in :gerbil-parser/src/runtime/recognition recognition-child-value)
        (only-in :gerbil-parser/src/compiler/lr-compiler compile-lr-spec)
        (only-in :gerbil-parser/src/runtime/token make-token token? token-kind token-lexeme token-start token-end)
        (only-in :gerbil-parser/src/runtime/artifact make-success-parse-artifact)
        (only-in :gerbil-parser/src/runtime/lr-parser
                 current-lr-recognition-observer current-lr-transfer-yield-observer lr-prepare lr-parse/prepared
                 lr-initial-checkpoint lr-checkpoint-before-shift
                 lr-checkpoint-fragment-compatible?
                 lr-checkpoint-inject-fragment lr-checkpoint-resume lr-checkpoint-feed
                 lr-recognition-fragment? lr-recognition-fragment-children lr-recognition-fragment-token-count
                 lr-recognition-view? lr-recognition-view-base lr-recognition-view-delta
                 lr-recognition-project lr-recognition-relocate))
(export lr-fragment-transfer-test)
;;; Frozen f21204f grammar-tree yield traversal: matched admission reference.
(def (reference-transfer-yield fragment delta tokens)
  (let loop ((pending (list (cons fragment delta))) (rest tokens) (visited 0))
    (if (null? pending) (values (null? rest) visited)
      (let* ((frame (car pending)) (piece (car frame)) (shift (cdr frame)) (visited (+ visited 1)))
        (cond
         ((lr-recognition-view? piece)
          (loop (cons (cons (lr-recognition-view-base piece) (+ shift (lr-recognition-view-delta piece)))
                      (cdr pending)) rest visited))
         ((lr-recognition-fragment? piece)
          (loop (append (map (lambda (child) (cons child shift)) (lr-recognition-fragment-children piece))
                        (cdr pending)) rest visited))
         ((and (pair? rest) (token? (car rest)) (token? piece)
               (eq? (token-kind (car rest)) (token-kind piece))
               (equal? (token-lexeme (car rest)) (token-lexeme piece))
               (= (token-start (car rest)) (+ shift (token-start piece)))
               (= (token-end (car rest)) (+ shift (token-end piece))))
          (loop (cdr pending) (cdr rest) visited))
         (else (values #f visited)))))))
(def (capture-transfer-root runtime tokens)
  (let (captured #f)
    (parameterize ((current-lr-recognition-observer (lambda (root) (set! captured root))))
      (let-values (((root rest) (lr-parse/prepared runtime tokens))) (check rest => '())))
    captured))
(def (shift-transfer-tokens tokens delta)
  (map (lambda (token)
         (make-token (token-kind token) (token-lexeme token)
                     (+ delta (token-start token)) (+ delta (token-end token)))) tokens))

(def (publish-transfer root prefix tokens)
  (let* ((source (string-append (make-string prefix #\space)
                               (string-join (map token-lexeme tokens) " ")))
         (stream
          (append (if (zero? prefix) '() (list (make-token 'space (make-string prefix #\space) 0 prefix)))
                  (let loop ((rest tokens))
                    (if (null? (cdr rest)) rest
                      (cons (car rest)
                            (cons (make-token 'space " " (token-end (car rest)) (token-start (cadr rest)))
                                  (loop (cdr rest)))))))))
    (make-success-parse-artifact "yield-test" source stream root (lambda (token) (eq? (token-kind token) 'space)))))

(def lr-fragment-transfer-test
  (test-suite "LR fragment transfer admission"
    (test-case "shared yields eliminate unary grammar traversal without relaxing terminal checks"
      (let* ((names (map (lambda (n) (string->symbol (string-append "layer" (number->string n)))) (iota 64)))
             (grammar
              (cons (list 'source-file (list 'alias 'SourceFile (list 'reference (car names))))
                    (map (lambda (index)
                           (list (list-ref names index)
                                 (if (= index 63) '(sequence (token identifier) (token identifier))
                                   (list 'reference (list-ref names (+ index 1)))))) (iota 64))))
             (runtime (lr-prepare (compile-lr-spec grammar 'source-file)))
             (old (list (make-token 'identifier "猫" 0 3) (make-token 'identifier "b" 4 5)))
             (captured (capture-transfer-root runtime old))
             (tokens (shift-transfer-tokens old 7))
             (checkpoint (lr-checkpoint-before-shift (lr-initial-checkpoint runtime '()) (car tokens)))
             (visits #f))
        (let-values (((matches baseline-visits) (reference-transfer-yield captured 7 tokens)))
          (check matches => #t)
          (check baseline-visits => 67)
          (parameterize ((current-lr-transfer-yield-observer (lambda (count) (set! visits count))))
            (let-values (((root rest)
                          (lr-checkpoint-resume (lr-checkpoint-inject-fragment checkpoint captured 7 tokens))))
              (let-values (((fresh rest) (lr-parse/prepared runtime tokens)))
                (check (publish-transfer root (token-start (car tokens)) tokens)
                       => (publish-transfer fresh (token-start (car tokens)) tokens)))))
          (check visits => 3))
        (let (wrong (list (car tokens) (make-token 'identifier "c" 11 12)))
          (let-values (((matches count) (reference-transfer-yield captured 7 wrong))) (check matches => #f))
          (check-exception (lr-checkpoint-inject-fragment checkpoint captured 7 wrong) true))))
    (test-case "terminal yields compose relocated subtrees with fresh sibling tokens"
      (let* ((runtime (lr-prepare (compile-lr-spec
                                  '((source-file (alias SourceFile (sequence (reference pair) (token identifier))))
                                    (pair (sequence (token identifier) (token identifier)))) 'source-file)))
             (old (list (make-token 'identifier "猫" 0 3) (make-token 'identifier "b" 4 5)
                        (make-token 'identifier "c" 6 7)))
             (captured (capture-transfer-root runtime old))
             (pair-fragment
              (let walk ((pending (list captured)))
                (and (pair? pending)
                     (let (piece (car pending))
                       (cond ((not (lr-recognition-fragment? piece)) (walk (cdr pending)))
                             ((= (lr-recognition-fragment-token-count piece) 2) piece)
                             (else (walk (append (lr-recognition-fragment-children piece) (cdr pending)))))))))
             (moved (shift-transfer-tokens old 7))
             (checkpoint (parameterize ((current-lr-recognition-observer (lambda (_) #f)))
                           (lr-checkpoint-before-shift (lr-initial-checkpoint runtime '()) (car moved))))
             (with-pair (lr-checkpoint-inject-fragment checkpoint pair-fragment 7 (take moved 2)))
             (rebuilt #f))
        (parameterize ((current-lr-recognition-observer (lambda (root) (set! rebuilt root))))
          (let-values (((status next) (lr-checkpoint-feed with-pair (caddr moved))))
            (check status => 'checkpoint)
            (let-values (((root rest) (lr-checkpoint-resume next))) (check rest => '()))))
        (let* ((later (shift-transfer-tokens moved 5))
               (next (lr-checkpoint-before-shift (lr-initial-checkpoint runtime '()) (car later))))
          (let-values (((matches count) (reference-transfer-yield rebuilt 5 later))) (check matches => #t))
          (let-values (((root rest) (lr-checkpoint-resume (lr-checkpoint-inject-fragment next rebuilt 5 later))))
            (let-values (((fresh rest) (lr-parse/prepared runtime later)))
              (check (publish-transfer root 12 later) => (publish-transfer fresh 12 later)))
            (check (publish-transfer root 12 later)
                   => (publish-transfer
                       (recognition-child-value
                        (car (recognition-sequence->list
                              (lr-recognition-project (lr-recognition-relocate rebuilt 5) later)))) 12 later)))
          (let (wrong (list (car later) (make-token 'identifier "b" 17 18) (caddr later)))
            (check-exception (lr-checkpoint-inject-fragment next rebuilt 5 wrong) true)))))
    (test-case "epsilon reductions disappear from the nonempty transfer yield"
      (let* ((runtime (lr-prepare (compile-lr-spec
                                  '((source-file (alias SourceFile
                                                   (sequence (optional (token identifier)) (literal "!")))))
                                  'source-file)))
             (tokens (list (make-token 'symbol "!" 0 1)))
             (captured (capture-transfer-root runtime tokens))
             (checkpoint (lr-initial-checkpoint runtime tokens))
             (visits #f))
        (parameterize ((current-lr-transfer-yield-observer (lambda (count) (set! visits count))))
          (let-values (((root rest) (lr-checkpoint-resume
                                    (lr-checkpoint-inject-fragment checkpoint captured 0 tokens))))
            (let-values (((fresh rest) (lr-parse/prepared runtime tokens)))
              (check (publish-transfer root 0 tokens) => (publish-transfer fresh 0 tokens)))))
        (check visits => 1)))
    (test-case "long left-recursive yields remain shared and reject a changed last terminal"
      (let* ((runtime (lr-prepare (compile-lr-spec
                                  '((source-file (alias SourceFile (repeat1 (token identifier))))) 'source-file)))
             (old (map (lambda (n) (make-token 'identifier "α" (* n 3) (+ (* n 3) 2))) (iota 512)))
             (captured (capture-transfer-root runtime old))
             (tokens (shift-transfer-tokens old 7))
             (checkpoint (lr-checkpoint-before-shift (lr-initial-checkpoint runtime '()) (car tokens)))
             (visits #f))
        (parameterize ((current-lr-transfer-yield-observer (lambda (count) (set! visits count))))
          (let-values (((root rest) (lr-checkpoint-resume
                                    (lr-checkpoint-inject-fragment checkpoint captured 7 tokens))))
            (let-values (((fresh rest) (lr-parse/prepared runtime tokens)))
              (check (publish-transfer root 7 tokens) => (publish-transfer fresh 7 tokens)))))
        (check visits => 1023)
        (let* ((final (last tokens))
               (wrong (append (take tokens 511)
                              (list (make-token 'identifier "β" (token-start final) (token-end final))))))
          (let-values (((matches count) (reference-transfer-yield captured 7 wrong))) (check matches => #f))
          (check-exception (lr-checkpoint-inject-fragment checkpoint captured 7 wrong) true))
        ;; Rejected transfer retains both checkpoint and shared yield.
        (let-values (((root rest) (lr-checkpoint-resume
                                  (lr-checkpoint-inject-fragment checkpoint captured 7 tokens))))
          (check rest => '()))))
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

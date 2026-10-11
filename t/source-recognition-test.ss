;;; Source-backed recognition spans are independent of language semantics.
(import :std/test
        :gerbil-parser/src/runtime/recognition
        (only-in :gerbil-parser/src/runtime/token make-token token-lexeme token-start token-end))
(export source-recognition-test)
(def (rejects? thunk)
  (with-catch (lambda (_) #t) (lambda () (thunk) #f)))
(def source-recognition-test
  (test-suite "source-backed recognition spans"
    (test-case "all Unicode widths and nonmonotonic endpoints agree with UTF-8"
      (let* ((text "aα中😀\n")
             (source (prepare-recognition-source (make-token 'source text 13 24))))
        (for-each (lambda (at)
                    (check (recognition-source-offset source at)
                           => (+ 13 (u8vector-length (string->utf8 (substring text 0 at))))))
                  '(5 0 4 1 3 2))
        (let* ((token (recognition-source-token source 'value 1 4))
               (node (recognition-source-node source 'Expression 1 4
                                             (list (make-recognition-child 'value token)))))
          (check (token-lexeme token) => "α中😀")
          (check (token-start token) => 14)
          (check (token-end token) => 23)
          (check (recognition-value-start node) => 14)
          (check (recognition-value-end node) => 23))))
    (test-case "ASCII and a late multibyte scalar admit exact boundaries"
      (for-each
       (lambda (text)
         (let (source (prepare-recognition-source
                       (make-token 'source text 7 (+ 7 (u8vector-length (string->utf8 text))))))
           (let loop ((at 0))
             (when (<= at (string-length text))
               (check (recognition-source-offset source at)
                      => (+ 7 (u8vector-length (string->utf8 (substring text 0 at)))))
               (loop (+ at 1))))))
       '("" "abc" "abcα" "abcα中😀")))
    (test-case "original text and returned views cannot invalidate admitted offsets"
      (let* ((text (string-copy "αx"))
             (source (prepare-recognition-source (make-token 'source text 2 5)))
             (view (recognition-source-text source)))
        (string-set! text 0 #\a)
        (string-set! view 0 #\b)
        (check (recognition-source-text source) => "αx")
        (check (token-lexeme (recognition-source-token source 'value 0 1)) => "α")
        (check (recognition-source-offset source 1) => 4)))
    (test-case "malformed byte spans and out-of-source boundaries reject"
      (for-each (lambda (token)
                  (check (rejects? (lambda () (prepare-recognition-source token))) => #t))
                (list #f (make-token 'source "α" 0 1) (make-token 'source "" 3 4)))
      (let (source (prepare-recognition-source (make-token 'source "α" 0 2)))
        (for-each (lambda (at)
                    (check (rejects? (lambda () (recognition-source-offset source at))) => #t))
                  '(-1 2 0.5))
        (check (rejects? (lambda () (recognition-source-token source 'value 1 0))) => #t)
        (check (rejects? (lambda () (recognition-source-node source 'Value 0 2 '()))) => #t)))))

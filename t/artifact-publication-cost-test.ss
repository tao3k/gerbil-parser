;;; Publication observations describe real work and preserve canonical products.
(import :std/test
        (only-in :gerbil-parser/src/runtime/artifact
                 make-success-parse-artifact make-success-parse-artifact/canonical-events
                 make-success-parse-artifact/raw-event-tape make-failure-parse-artifact
                 parse-artifact-events parse-artifact-valid-for-source? sha256-text)
        (only-in :gerbil-parser/src/runtime/recognition
                 make-recognition-node make-recognition-child)
        (only-in :gerbil-parser/src/runtime/event-program event-program-node-value)
        (only-in :gerbil-parser/src/runtime/token make-token)
        (only-in :gerbil-parser/src/runtime/parse-cost current-parser-cost-observer with-parser-cost-stage)
        (only-in ./benchmarks/parser-stage-cost/benchmark measure-parser-stages
                 measure-artifact-event-storage copy-canonical-event-storage
                 measure-parser-component measure-parser-batch))
(export artifact-publication-cost-test)

(def (observe-publication thunk)
  (let (rows '())
    (let (value
          (parameterize ((current-parser-cost-observer
                          (lambda (name receipt) (set! rows (cons (cons name receipt) rows)))))
            (thunk)))
      (values value (reverse rows)))))
(def (never-trivia? token) #f)
(def +publication-test-grammar+ (sha256-text "publication-cost-contract"))

(def artifact-publication-cost-test
  (test-suite "artifact publication cost contracts"
    (test-case "shared sampler validates dimensions before warmup side effects"
      (let* ((calls 0) (thunk (lambda () (set! calls (+ calls 1)) 'complete)))
        (for-each
         (lambda (sizes)
           (check-exception
            (measure-parser-component 'invalid (car sizes) (cadr sizes) thunk 'complete) true))
         '((0 1) (-1 1) (1.0 1) (1 0) (1 1.0)))
        (check-exception (measure-parser-component 'invalid 1 3 thunk 'complete 2) true)
        (check calls => 0)))
    (test-case "shared sampler preserves complete batches and original failures"
      (let* ((calls 0)
             (thunk (lambda () (set! calls (+ calls 1)) '(complete result)))
             (summary (parameterize ((current-output-port (open-output-string)))
                        (measure-parser-component 'shared 2 2 thunk '(complete result)))))
        (check calls => 5)
        (check (cdr (assq 'sampleCount summary)) => 2)
        (check (length (cdr (assq 'samples summary))) => 2)
        (check (cdr (assq 'allocationScope summary)) => 'single-caller))
      (check (with-catch (lambda (condition) condition)
               (lambda () (measure-parser-batch 'failed 0 1
                            (lambda () (raise 'original-failure)) 'complete)))
             => 'original-failure))
    (test-case "storage control copies containers and shares canonical payloads"
      (let* ((lexeme (string-copy "α🙂"))
             (events (list (vector 'start-node 0 'Root 0)
                           (vector 'start-field 'value 0)
                           (vector 'token 0 'word lexeme 0 6)
                           (vector 'finish-field 'value 6)
                           (vector 'finish-node 0 'Root 6)))
             (copy (copy-canonical-event-storage events)))
        (check copy => events)
        (let loop ((original events) (copied copy))
          (unless (null? original)
            (check (eq? original copied) => #f)
            (check (eq? (car original) (car copied)) => #f)
            (loop (cdr original) (cdr copied))))
        (check (eq? (vector-ref (list-ref copy 2) 3) lexeme) => #t)
        (vector-set! (car copy) 2 'Changed)
        (check (vector-ref (car events) 2) => 'Root)))
    (test-case "storage measurement admits accepted empty Unicode and rejected products"
      (for-each
       (lambda (source)
         (let* ((end (u8vector-length (string->utf8 source)))
                (tokens (if (zero? end) '() (list (make-token 'word source 0 end))))
                (root (make-recognition-node 'Root 0 end
                        (map (lambda (token) (make-recognition-child 'value token)) tokens))))
           (for-each
            (lambda (artifact)
              (let (summary (parameterize ((current-output-port (open-output-string)))
                               (measure-artifact-event-storage artifact source 3)))
                (check (cdr (assq 'samples summary)) => 3)
                (check (cdr (assq 'events summary)) => (length (parse-artifact-events artifact)))
                (check (parse-artifact-valid-for-source? artifact source) => #t)))
            (list (make-success-parse-artifact +publication-test-grammar+ source tokens root never-trivia?)
                  (make-failure-parse-artifact +publication-test-grammar+ source tokens
                    '((message . "expected another token")))))))
       '("" "α🙂"))
      (check-exception (measure-artifact-event-storage '() "" 1) true)
      (check-exception (measure-artifact-event-storage '() "" 0) true))
    (test-case "recognition and event publication preserve Unicode and empty artifacts"
      (for-each
       (lambda (source)
         (let* ((bytes (string->utf8 source)) (end (u8vector-length bytes))
                (token (and (positive? end) (make-token 'word source 0 end)))
                (tokens (if token (list token) '()))
                (root (make-recognition-node 'Root 0 end
                        (if token (list (make-recognition-child #f token)) '())))
                (program (event-program-node-value 'Root 0 end token))
                (reference (make-success-parse-artifact +publication-test-grammar+
                             source tokens root never-trivia?)))
           (for-each
            (lambda (value)
              (let-values (((artifact rows)
                            (observe-publication
                             (lambda () (make-success-parse-artifact +publication-test-grammar+
                                          source tokens value never-trivia?)))))
                (check artifact => reference)
                (check (parse-artifact-valid-for-source? artifact source) => #t)
                (check (map car rows)
                       => '(artifact-source-encoding artifact-event-publication artifact-source-identity))
                (check (map (lambda (row) (cdr (assq 'completed (cdr row)))) rows) => '(#t #t #t))))
            (list root program))))
       '("" "abc" "α🙂"))
      (check (current-parser-cost-observer) => #f))
    (test-case "committed tape and supplied canonical events report only owned work"
      (let* ((source "α") (bytes (string->utf8 source))
             (token (make-token 'word source 0 2)) (tokens (list token))
             (root (make-recognition-node 'Root 0 2 (list (make-recognition-child #f token))))
             (reference (make-success-parse-artifact +publication-test-grammar+ source tokens root never-trivia?))
             (tape (vector 'open-node 'Root 0 'token token 0 'close-node 'Root 2
                           'invalid-unused-tail #f #f)))
        (let-values (((artifact rows)
                      (observe-publication
                       (lambda () (make-success-parse-artifact/raw-event-tape
                                    +publication-test-grammar+ source tokens tape 3 never-trivia? bytes)))))
          (check artifact => reference)
          (check (parse-artifact-valid-for-source? artifact source) => #t)
          (check (map car rows) => '(artifact-event-publication artifact-source-identity)))
        (let-values (((artifact rows)
                      (observe-publication
                       (lambda () (make-success-parse-artifact/canonical-events
                                    +publication-test-grammar+ source (parse-artifact-events reference) bytes)))))
          (check artifact => reference)
          ;; Already supplied bytes/events are capabilities, not executed stages.
          (check (map car rows) => '(artifact-source-identity)))))
    (test-case "rejected artifact publication remains complete and lossless"
      (let* ((source "α") (tokens (list (make-token 'invalid source 0 2)))
             (diagnostic '((message . "expected word")))
             (publish (lambda () (make-failure-parse-artifact +publication-test-grammar+ source tokens diagnostic)))
             (reference (publish)))
        (let-values (((artifact rows) (observe-publication publish)))
          (check artifact => reference)
          (check (parse-artifact-valid-for-source? artifact source) => #t)
          (check (map car rows)
                 => '(artifact-event-publication artifact-source-encoding artifact-source-identity)))))
    (test-case "failed tape publication retains its error and incomplete observation"
      (let ((rows '()) (publish
            (lambda () (make-success-parse-artifact/raw-event-tape +publication-test-grammar+
                         "" '() (vector 'unknown #f 0) 1 never-trivia? (make-u8vector 0)))))
        (def (failure thunk)
          (with-catch (lambda (condition) (list (error-message condition) (error-irritants condition))) thunk))
        (let (reference (failure publish))
          (check (parameterize ((current-parser-cost-observer
                                 (lambda (name receipt) (set! rows (cons (cons name receipt) rows)))))
                   (failure publish)) => reference))
        (check (map car rows) => '(artifact-event-publication))
        (check (cdr (assq 'completed (cdar rows))) => #f)
        (check (current-parser-cost-observer) => #f)))
    (test-case "stage summaries distinguish failed intervals from complete requests"
      (let* ((reference (make-failure-parse-artifact +publication-test-grammar+ "" '()
                           '((message . "recovered publication failure"))))
             (parse (lambda (source)
                      (with-catch (lambda (condition) reference)
                        (lambda () (with-parser-cost-stage 'recoverable-publication
                                     (raise 'original-condition))))))
             (summary (parameterize ((current-output-port (open-output-string)))
                        (measure-parser-stages parse "" 3))))
        (for-each
         (lambda (row)
           (check (cdr (assq 'request-samples row)) => 3)
           (check (cdr (assq 'samples row)) => 3)
           (if (eq? (cdr (assq 'stage row)) 'recoverable-publication)
             (begin (check (cdr (assq 'completed-samples row)) => 0)
                    (check (cdr (assq 'incomplete-samples row)) => 3))
             (begin (check (cdr (assq 'completed-samples row)) => 3)
                    (check (cdr (assq 'incomplete-samples row)) => 0))))
         summary)
        (check (length summary) => 2)
        (check (current-parser-cost-observer) => #f)))))

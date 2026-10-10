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
        (only-in ./benchmarks/parser-stage-cost/benchmark measure-parser-stages))
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

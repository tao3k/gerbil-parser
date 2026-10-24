(import :gerbil-parser/language-test-support)
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
                 measure-parser-component measure-parser-batch parser-cpu-milliseconds))


(def (observe-publication thunk)
  (let (rows '())
    (let (value
          (parameterize ((current-parser-cost-observer
                          (lambda (name receipt) (set! rows (cons (cons name receipt) rows)))))
            (thunk)))
      (values value (reverse rows)))))
(def (never-trivia? token) #f)
(def +publication-test-grammar+ (sha256-text "publication-cost-contract"))

(deflanguage-parser-tests artifact-publication-cost-test "artifact publication cost contracts"
  (engine)
    (scenario "equal CPU clock intervals retain exact endpoint identity"
      (scope ((before (make-f64vector 8 0.0)) (after (make-f64vector 8 0.0)))
        (act (f64vector-set! before 0 40.1))
        (act (f64vector-set! after 0 40.106735))
        (scope ((first (parser-cpu-milliseconds before after)))
          (act (f64vector-set! before 0 30.1))
          (act (f64vector-set! after 0 30.106735))
          (equal first  6.735)
          (equal (parser-cpu-milliseconds before after)  first)
          ;; A genuine one-microsecond separation must remain separated.
          (act (f64vector-set! after 1 0.000001))
          (equal (parser-cpu-milliseconds before after)  6.736)
          (equal (< first (parser-cpu-milliseconds before after))  #t)
          ;; User and system intervals use the same clock representation.
          (act (f64vector-set! after 0 30.106734))
          (equal (parser-cpu-milliseconds before after)  first))))
    (scenario "wide field-bearing children retain source order and reject improper tails"
      (each (width) ('(0 1 1024))
         (sequential ((source (make-string width #\a))
                (tokens (map (lambda (at) (make-token 'word "a" at (+ at 1))) (iota width)))
                (children (map (lambda (token) (make-recognition-child 'value token)) tokens))
                (root (make-recognition-node 'Root 0 width children))
                (artifact (make-success-parse-artifact +publication-test-grammar+ source tokens root never-trivia?)))
           (equal (parse-artifact-valid-for-source? artifact source)  #t)
           (equal (length (parse-artifact-events artifact))  (+ 2 (* 3 width)))
           (when (pair? children)
             (raises
              (make-success-parse-artifact +publication-test-grammar+ source tokens
                (make-recognition-node 'Root 0 width (cons (car children) #f)) never-trivia?) true)))))
    (scenario "shared sampler validates dimensions before warmup side effects"
      (sequential ((calls 0) (thunk (lambda () (set! calls (+ calls 1)) 'complete)))
        (each (sizes) ('((0 1) (-1 1) (1.0 1) (1 0) (1 1.0)))
           (raises
            (measure-parser-component 'invalid (car sizes) (cadr sizes) thunk 'complete) true))
        (raises (measure-parser-component 'invalid 1 3 thunk 'complete 2) true)
        (equal calls  0)))
    (scenario "shared sampler preserves complete batches and original failures"
      (sequential ((calls 0)
             (thunk (lambda () (set! calls (+ calls 1)) '(complete result)))
             (summary (parameterize ((current-output-port (open-output-string)))
                        (measure-parser-component 'shared 2 2 thunk '(complete result)))))
        (equal calls  5)
        (equal (cdr (assq 'sampleCount summary))  2)
        (equal (length (cdr (assq 'samples summary)))  2)
        (equal (cdr (assq 'allocationScope summary))  'single-caller))
      (equal (with-catch (lambda (condition) condition)
               (lambda () (measure-parser-batch 'failed 0 1
                            (lambda () (raise 'original-failure)) 'complete)))
              'original-failure))
    (scenario "storage control copies containers and shares canonical payloads"
      (sequential ((lexeme (string-copy "α🙂"))
             (events (list (vector 'start-node 0 'Root 0)
                           (vector 'start-field 'value 0)
                           (vector 'token 0 'word lexeme 0 6)
                           (vector 'finish-field 'value 6)
                           (vector 'finish-node 0 'Root 6)))
             (copy (copy-canonical-event-storage events)))
        (equal copy  events)
        (iterate loop ((original events) (copied copy))
          (unless (null? original)
            (equal (eq? original copied)  #f)
            (equal (eq? (car original) (car copied))  #f)
            (continue loop (cdr original) (cdr copied))))
        (equal (eq? (vector-ref (list-ref copy 2) 3) lexeme)  #t)
        (act (vector-set! (car copy) 2 'Changed))
        (equal (vector-ref (car events) 2)  'Root)))
    (scenario "storage measurement admits accepted empty Unicode and rejected products"
      (each (source) ('("" "α🙂"))
         (sequential ((end (u8vector-length (string->utf8 source)))
                (tokens (if (zero? end) '() (list (make-token 'word source 0 end))))
                (root (make-recognition-node 'Root 0 end
                        (map (lambda (token) (make-recognition-child 'value token)) tokens))))
           (each (artifact) ((list (make-success-parse-artifact +publication-test-grammar+ source tokens root never-trivia?)
                  (make-failure-parse-artifact +publication-test-grammar+ source tokens
                    '((message . "expected another token")))))
              (scope ((summary (parameterize ((current-output-port (open-output-string)))
                               (measure-artifact-event-storage artifact source 3))))
                (equal (cdr (assq 'samples summary))  3)
                (equal (cdr (assq 'events summary))  (length (parse-artifact-events artifact)))
                (equal (parse-artifact-valid-for-source? artifact source)  #t)))))
      (raises (measure-artifact-event-storage '() "" 1) true)
      (raises (measure-artifact-event-storage '() "" 0) true))
    (scenario "recognition and event publication preserve Unicode and empty artifacts"
      (each (source) ('("" "abc" "α🙂"))
         (sequential ((bytes (string->utf8 source)) (end (u8vector-length bytes))
                (token (and (positive? end) (make-token 'word source 0 end)))
                (tokens (if token (list token) '()))
                (root (make-recognition-node 'Root 0 end
                        (if token (list (make-recognition-child #f token)) '())))
                (program (event-program-node-value 'Root 0 end token))
                (reference (make-success-parse-artifact +publication-test-grammar+
                             source tokens root never-trivia?)))
           (each (value) ((list root program))
              (scope-values (((artifact rows)
                            (observe-publication
                             (lambda () (make-success-parse-artifact +publication-test-grammar+
                                          source tokens value never-trivia?)))))
                (equal artifact  reference)
                (equal (parse-artifact-valid-for-source? artifact source)  #t)
                (equal (map car rows)
                        '(artifact-source-encoding artifact-event-publication artifact-source-identity))
                (equal (map (lambda (row) (cdr (assq 'completed (cdr row)))) rows)  '(#t #t #t))))))
      (equal (current-parser-cost-observer)  #f))
    (scenario "committed tape and supplied canonical events report only owned work"
      (sequential ((source "α") (bytes (string->utf8 source))
             (token (make-token 'word source 0 2)) (tokens (list token))
             (root (make-recognition-node 'Root 0 2 (list (make-recognition-child #f token))))
             (reference (make-success-parse-artifact +publication-test-grammar+ source tokens root never-trivia?))
             (tape (vector 'open-node 'Root 0 'token token 0 'close-node 'Root 2
                           'invalid-unused-tail #f #f)))
        (scope-values (((artifact rows)
                      (observe-publication
                       (lambda () (make-success-parse-artifact/raw-event-tape
                                    +publication-test-grammar+ source tokens tape 3 never-trivia? bytes)))))
          (equal artifact  reference)
          (equal (parse-artifact-valid-for-source? artifact source)  #t)
          (equal (map car rows)  '(artifact-event-publication artifact-source-identity)))
        (scope-values (((artifact rows)
                      (observe-publication
                       (lambda () (make-success-parse-artifact/canonical-events
                                    +publication-test-grammar+ source (parse-artifact-events reference) bytes)))))
          (equal artifact  reference)
          ;; Already supplied bytes/events are capabilities, not executed stages.
          (equal (map car rows)  '(artifact-source-identity)))))
    (scenario "rejected artifact publication remains complete and lossless"
      (sequential ((source "α") (tokens (list (make-token 'invalid source 0 2)))
             (diagnostic '((message . "expected word")))
             (publish (lambda () (make-failure-parse-artifact +publication-test-grammar+ source tokens diagnostic)))
             (reference (publish)))
        (scope-values (((artifact rows) (observe-publication publish)))
          (equal artifact  reference)
          (equal (parse-artifact-valid-for-source? artifact source)  #t)
          (equal (map car rows)
                  '(artifact-event-publication artifact-source-encoding artifact-source-identity)))))
    (scenario "failed tape publication retains its error and incomplete observation"
      (scope ((rows '()) (publish
            (lambda () (make-success-parse-artifact/raw-event-tape +publication-test-grammar+
                         "" '() (vector 'unknown #f 0) 1 never-trivia? (make-u8vector 0)))))
(locals ((failure (lambda ( thunk)
          (with-catch (lambda (condition) (list (error-message condition) (error-irritants condition))) thunk))))

        (scope ((reference (failure publish)))
          (equal (parameterize ((current-parser-cost-observer
                                 (lambda (name receipt) (set! rows (cons (cons name receipt) rows)))))
                   (failure publish))  reference))
        (equal (map car rows)  '(artifact-event-publication))
        (equal (cdr (assq 'completed (cdar rows)))  #f)
        (equal (current-parser-cost-observer)  #f))))
    (scenario "stage summaries distinguish failed intervals from complete requests"
      (sequential ((reference (make-failure-parse-artifact +publication-test-grammar+ "" '()
                           '((message . "recovered publication failure"))))
             (parse (lambda (source)
                      (with-catch (lambda (condition) reference)
                        (lambda () (with-parser-cost-stage 'recoverable-publication
                                     (raise 'original-condition))))))
             (summary (parameterize ((current-output-port (open-output-string)))
                        (measure-parser-stages parse "" 3))))
        (each (row) (summary)
           (equal (cdr (assq 'request-samples row))  3)
           (equal (cdr (assq 'samples row))  3)
           (branch (eq? (cdr (assq 'stage row)) 'recoverable-publication)
             (group (equal (cdr (assq 'completed-samples row))  0)
                    (equal (cdr (assq 'incomplete-samples row))  3))
             (group (equal (cdr (assq 'completed-samples row))  3)
                    (equal (cdr (assq 'incomplete-samples row))  0))))
        (equal (length summary)  2)
        (equal (current-parser-cost-observer)  #f))))

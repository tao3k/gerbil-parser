;;; -*- Gerbil -*-
;;; In-process actor scheduling costs; uses the same prepared language machine.
(import :gerbil-parser/languages/gql/iso-39075-2024/parser
        (only-in :gerbil-parser/parser-actor-support
                 spawn-parser-actor parser-actor-parse parser-actor-stop!)
        (only-in ./matched-stages measure-gql-component))
(export main profile-gql-actors)

(def (profile-gql-actors source samples batch-count clients workers)
  (unless (and (integer? samples) (> samples 0)
               (integer? batch-count) (> batch-count 0)
               (integer? clients) (<= 1 clients batch-count)
               (integer? workers) (<= 1 workers clients))
    (error "invalid actor benchmark dimensions" samples batch-count clients workers))
  (let* ((expected (parse-gql-iso-39075-2024 source))
         (actors (list->vector
                  (map (lambda (_) (spawn-parser-actor gql-iso-parser)) (iota workers)))))
    (try
     (measure-gql-component
      (list 'actor-requests (cons 'clients clients) (cons 'workers workers))
      samples batch-count
      (lambda ()
        (let (threads
              (map
               (lambda (index)
                 (spawn
                  (lambda ()
                    (let loop ((remaining (+ (quotient batch-count clients)
                                             (if (< index (modulo batch-count clients)) 1 0)))
                               (last #f))
                      (if (zero? remaining) last
                        (loop (- remaining 1)
                              (parser-actor-parse
                               (vector-ref actors (modulo index workers)) source)))))))
               (iota clients)))
          (map (lambda (thread) (thread-join! thread 5)) threads)))
      (make-list clients expected) batch-count 'process-with-thread-switches)
     (finally (for-each parser-actor-stop! (vector->list actors))))))

(def (main . args)
  (let ((samples (if (pair? args) (string->number (car args)) 40))
        (batch-count (if (> (length args) 1) (string->number (cadr args)) 100)))
    (for-each (lambda (dimensions)
                (profile-gql-actors +gql-representative-query+
                                    samples batch-count (car dimensions) (cadr dimensions)))
              '((1 1) (4 1) (4 4)))
    (displayln "GQL-ACTORS-OK") (force-output)))

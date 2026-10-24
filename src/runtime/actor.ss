;;; -*- Gerbil -*-
;;; Optional in-process actor boundary; importing this library starts no threads.
(import :std/actor
        (only-in ../compiler/machine parser-machine?)
        (only-in ./parser parse-source))
(export spawn-parser-actor parser-actor-parse parser-actor-stop!)

(def +parser-protocol+ "/gerbil-parser/parse/v1")
(defstruct !ParseSource (source))

;; The application owns the returned LocalActor and the machine's lifetime.
;; Install generated drivers before spawning; never mutate an active machine.
(def (spawn-parser-actor machine)
  (unless (parser-machine? machine) (error "expected a prepared parser machine"))
  (LocalActor
   (spawn
    (lambda ()
      (let loop ()
        (<- protocol: +parser-protocol+
            ((!ParseSource source)
             (let (result
                   (with-catch
                    (lambda (exception)
                      (!Error (call-with-output-string
                               (lambda (port) (display-exception exception port)))))
                    (lambda () (!OK (parse-source machine source)))))
               (--> result))
             (loop))
            protocol: proto:/actor/control
            ((!Ping) (--> (!OK 'OK)) (loop))
            ((!Shutdown) (--> (!OK 'shutdown)))))))))

(def (check-timeout timeout)
  (unless (and (real? timeout) (> timeout 0))
    (error "expected a positive actor request timeout" timeout)))

;; Gerbil's envelope continuation correlates replies from concurrent callers.
;; Expired queued envelopes are discarded by <-. Timeout does not preempt a
;; parse that already started. Snapshot caller-owned mutable strings on submit.
(def (parser-actor-parse actor source (timeout 5))
  (unless (string? source) (error "parse source must be a string" source))
  (check-timeout timeout)
  (with-result
   (->> actor (!ParseSource (string-copy source)) +parser-protocol+
        timeout: timeout)
   (lambda (message) (error "parser actor request failed" message))))

;; Shutdown is ordered after previously queued work; completion joins the child.
(def (parser-actor-stop! actor (timeout 5))
  (check-timeout timeout)
  (let (result
        (with-result
         (->> actor (!Shutdown) proto:/actor/control timeout: timeout)
         (lambda (message) (error "parser actor shutdown failed" message))))
    (thread-join! (LocalActor-thread actor) timeout)
    result))

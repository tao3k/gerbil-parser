;;; -*- Gerbil -*-
(import :std/test
        (only-in :gerbil-parser/parser-actor-support
                 spawn-parser-actor parser-actor-parse parser-actor-stop!)
        :gerbil-parser/languages/gql/parser
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-success?))
(export gql-actor-test)
(def gql-actor-test
  (test-suite "GQL optional library actor"
    (test-case "concurrent callers preserve exact independent artifacts"
      (let* ((actor (spawn-parser-actor gql-parser))
             (sources '("RETURN 1" "RETURN '你好'" "RETURN @" "RETURN 2"))
             (expected (map parse-gql sources)))
        (try
         (let (clients (map (lambda (source)
                             (spawn (lambda () (parser-actor-parse actor source))))
                           sources))
           (check (map (lambda (client) (thread-join! client 3)) clients)
                  => expected))
         (finally (check (parser-actor-stop! actor) => 'shutdown)))))
    (test-case "invalid requests do not poison the actor"
      (let (actor (spawn-parser-actor gql-parser))
        (try
         (check-exception (parser-actor-parse actor 42) true)
         (check-exception (parser-actor-parse actor "RETURN 1" 0) true)
         (check (parse-artifact-success? (parser-actor-parse actor "RETURN 1")) => #t)
         (finally (parser-actor-stop! actor)))))
    (test-case "shutdown terminates the owned actor"
      (let (actor (spawn-parser-actor gql-parser))
        (check (parser-actor-stop! actor) => 'shutdown)
        (check-exception (parser-actor-parse actor "RETURN 1" .05) true)))))

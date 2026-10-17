(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
(import :std/test
        (only-in :gerbil-parser/parser-actor-support
                 spawn-parser-actor parser-actor-parse parser-actor-stop!)
        :gerbil-parser/languages/gql/parser
        (only-in :gerbil-parser/src/runtime/artifact parse-artifact-success?))

(deflanguage-parser-tests gql-actor-test "GQL optional library actor"
  (engine)
    (scenario "concurrent callers preserve exact independent artifacts"
      (sequential ((actor (spawn-parser-actor gql-parser))
             (sources '("RETURN 1" "RETURN '你好'" "RETURN @" "RETURN 2"))
             (expected (map parse-gql sources)))
        (protect (body
         (scope ((clients (map (lambda (source)
                             (spawn (lambda () (parser-actor-parse actor source))))
                           sources)))
           (equal (map (lambda (client) (thread-join! client 3)) clients)
                   expected))
         ) (cleanup (equal (parser-actor-stop! actor)  'shutdown)))))
    (scenario "invalid requests do not poison the actor"
      (scope ((actor (spawn-parser-actor gql-parser)))
        (protect (body
         (raises (parser-actor-parse actor 42) true)
         (raises (parser-actor-parse actor "RETURN 1" 0) true)
         (equal (parse-artifact-success? (parser-actor-parse actor "RETURN 1"))  #t)
         ) (cleanup (act (parser-actor-stop! actor))))))
    (scenario "shutdown terminates the owned actor"
      (scope ((actor (spawn-parser-actor gql-parser)))
        (equal (parser-actor-stop! actor)  'shutdown)
        (raises (parser-actor-parse actor "RETURN 1" .05) true))))

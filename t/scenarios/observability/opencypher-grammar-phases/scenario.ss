;;; -*- Gerbil -*-
;;; Grammar-owned POO Flow phase observation without global instrumentation.

(import (only-in :std/srfi/13 string-contains)
        (only-in :std/sugar filter)
        :poo-flow/src/module-system/observability/interface
        (only-in :gerbil-parser/languages/cypher/opencypher-2024-1/parser
                 opencypher-2024-1-language-grammar
                 parse-opencypher-2024-1)
        (only-in :gerbil-parser/src/language/descriptor
                 language-grammar-observability
                 language-grammar-with-observability)
        (only-in :gerbil-parser/src/language/entry parse-language-source)
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-roundtrip
                 parse-artifact-success?
                 parse-artifact-valid?))
(export opencypher-grammar-observability-scenario
        opencypher-grammar-observability-scenario-pass?)

(def +observed-phases+
  '(lexical-analysis significant-token-filter selective-glr-execution
    lr-execution artifact-materialization))

(def (row-ref row key)
  (let (entry (assq key row)) (and entry (cdr entry))))

(def (opencypher-grammar-observability-scenario)
  (let* ((source "RETURN count(*) AS n\n")
         (plain (parse-opencypher-2024-1 source))
         (policy (poo-flow-debug-call-policy 'gerbil-parser/runtime 4))
         (observed-grammar
          (language-grammar-with-observability
           opencypher-2024-1-language-grammar policy))
         (port (open-output-string))
         (observed
          (parameterize ((current-error-port port))
            (parse-language-source observed-grammar source)))
         (output (get-output-string port)))
    (list
     (cons 'schema "gerbil-parser.grammar-observability.v1")
     (cons 'defaultUnobserved
           (not (language-grammar-observability
                 opencypher-2024-1-language-grammar)))
     (cons 'observedPhases
           (filter (lambda (phase)
                     (string-contains output (symbol->string phase)))
                   +observed-phases+))
     ;; Older released POO Flow call receipts do not yet carry elapsed time.
     ;; Report the upstream capability without inventing a parser-local timer;
     ;; linked development HEADs make this true.
     (cons 'upstreamElapsedReceipt
           (and (string-contains output "elapsed-nanoseconds") #t))
     (cons 'plainSuccess (parse-artifact-success? plain))
     (cons 'observedSuccess (parse-artifact-success? observed))
     (cons 'observedValid (parse-artifact-valid? observed))
     (cons 'artifactEqual
           (equal? (parse-artifact-roundtrip observed)
                   (parse-artifact-roundtrip plain))))))

(def (opencypher-grammar-observability-scenario-pass? receipt)
  (and (equal? (row-ref receipt 'schema)
               "gerbil-parser.grammar-observability.v1")
       (row-ref receipt 'defaultUnobserved)
       (equal? (row-ref receipt 'observedPhases) +observed-phases+)
       (row-ref receipt 'plainSuccess)
       (row-ref receipt 'observedSuccess)
       (row-ref receipt 'observedValid)
       (row-ref receipt 'artifactEqual)))

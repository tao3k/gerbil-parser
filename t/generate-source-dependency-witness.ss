;;; Serialize actual producer declarations with the shared source-data macros.
(import (only-in "fixtures/event-walk-source.ss" read-source-definition)
        (only-in "../src/testing/source-witness.ss"
                 parser-source-data write-parser-source-data source-data-current?))
(export main)
(def (bindings)
  (parser-source-data
    (ParserEngine.SourceDependencyWitness moduleSource
      (call-with-input-file "src/testing/source-execution.ss" read-all))
    (ParserEngine.SourceDependencyWitness resolutionSource
      (read-source-definition "src/testing/source-execution.ss" 'source-reference-resolved?))
    (ParserEngine.SourceDependencyWitness readerSource
      (read-source-definition "t/fixtures/event-walk-source.ss" 'read-source-definition-port))
    (ParserEngine.SourceDependencyWitness matcherSource
      (read-source-definition "t/fixtures/event-walk-source.ss" 'source-declaration-matches?))
    (ParserEngine.SourceDependencyWitness snapshotSource
      (read-source-definition "t/fixtures/event-walk-source.ss" 'copy-source-data))
    (ParserEngine.SourceDependencyWitness transformSource
      (read-source-definition "t/fixtures/event-walk-source.ss" 'transform-source-declaration))
    (ParserEngine.SourceDependencyWitness adapterSource
      (read-source-definition "t/fixtures/event-walk-source.ss" 'source-procedure))
    (ParserEngine.SourceDependencyWitness eventAdapterSource
      (read-source-definition "t/event-walk-source-test.ss" 'source-procedure))
    (ParserEngine.SourceDependencyWitness recordSource
      (read-source-definition "src/testing/source-execution.ss" 'expanded-source-dependencies))
    (ParserEngine.SourceDependencyWitness referenceSource
      (read-source-definition "src/testing/source-execution.ss" 'source-dependency-reference%))
    (ParserEngine.SourceDependencyWitness assignmentSource
      (read-source-definition "src/testing/source-execution.ss" 'source-dependency-assignment%))
    (ParserEngine.SourceDependencyWitness prepareSource
      (read-source-definition "src/testing/source-execution.ss" 'prepare-source-procedure))))
(def (main mode output)
  (let (sources (bindings))
    (cond
     ((equal? mode "module")
      (let (rendered (call-with-output-string (lambda (port) (write-parser-source-data sources port))))
        (call-with-output-file output (lambda (port) (write-string rendered port)))))
     ((equal? mode "check")
      (unless (source-data-current? sources output) (error "source dependency proof input is stale")))
     (else (error "expected module|check output")))))

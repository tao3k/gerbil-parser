(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; POO graph rules are validated and resolve against one grammar identity.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/languages/arithmetic/parser
                 arithmetic-language-grammar)
        (only-in :gerbil-parser/graph-projection-support
                 make-graph-projection make-graph-node make-graph-field
                 graph-projection? graph-projection-digest))
(import (only-in :gerbil-parser/src/compiler/graph-projection-runtime
                 graph-projection-runtime-source))


(deflanguage-parser-tests graph-projection-aot-test "POO graph projection AOT"
  (engine)
    (property "typed graph rules bind to generated node and token kinds"
      (bindings (projection
              (make-graph-projection
               (list (make-graph-node
                      'Expression "document" "root"
                      (list (make-graph-field 'Number "value")))))) (source (graph-projection-runtime-source
                      arithmetic-language-grammar projection)) (digest (graph-projection-digest
                      arithmetic-language-grammar projection)))
      (equal (graph-projection? projection)  #t)
      (equal (and (string-contains source "GraphProjectionSpec")
                    (string-contains source "projection_digest: \"sha256:")
                    (string-contains source "name: \"value\"")
                    (string-contains source "mode: GraphFieldMode::Append")
                    #t)
                #t)
      (equal (if (string-contains
                    source (string-append "projection_digest: \""
                                          digest "\""))
                 #t #f)
                #t))
    (property "field cardinality is checked and changes the generated contract"
      (bindings (append-rule
              (make-graph-projection
               (list (make-graph-node
                      'Expression "document" "root"
                      (list (make-graph-field 'Number "value")))))) (each-rule
              (make-graph-projection
               (list (make-graph-node
                      'Expression "document" "root"
                      (list (make-graph-field 'Number "value" 'each)))))) (source (graph-projection-runtime-source
                      arithmetic-language-grammar each-rule)))
      (equal (if (string-contains source "mode: GraphFieldMode::Each")
                 #t #f)
                #t)
      (equal (equal? (graph-projection-digest
                        arithmetic-language-grammar append-rule)
                       (graph-projection-digest
                        arithmetic-language-grammar each-rule))
                #f)
      (equal
         (with-catch
          (lambda (error) #t)
          (lambda () (make-graph-field 'Number "value" 'invented) #f))
          #t))
    (property "empty append fields remain Scheme-declared AOT graph values"
      (bindings (projection
              (make-graph-projection
               (list (make-graph-node
                      'Expression "document" "root"
                      (list (make-graph-field
                             'Number "value" 'append-or-empty)))))) (source (graph-projection-runtime-source
                      arithmetic-language-grammar projection)))
      (equal (graph-projection? projection)  #t)
      (equal (if (string-contains
                    source "mode: GraphFieldMode::AppendOrEmpty")
                 #t #f)
                #t))
    (property "node text fields resolve against node kinds"
      (bindings (projection
              (make-graph-projection
               (list (make-graph-node
                      'Expression "document" "root"
                      (list (make-graph-field
                             'Expression "source" 'node-text)))))) (source (graph-projection-runtime-source
                      arithmetic-language-grammar projection)))
      (equal (graph-projection? projection)  #t)
      (equal (if (string-contains
                    source "mode: GraphFieldMode::NodeText")
                 #t #f)
                #t))
    (property "duplicate graph node owners are rejected"
      (bindings)
      (equal
       (with-catch
        (lambda (error) #t)
        (lambda ()
          (make-graph-projection
           (list (make-graph-node 'Expression "document" "root" '())
                 (make-graph-node 'Expression "element" "duplicate" '())))
          #f))
        #t))
    (property "wrong-category syntax references are rejected at AOT"
      (bindings)
      (equal
       (with-catch
        (lambda (error) #t)
        (lambda ()
          (graph-projection-runtime-source
           arithmetic-language-grammar
           (make-graph-projection
            (list (make-graph-node 'Number "document" "root" '()))))
          #f))
        #t)))

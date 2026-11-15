(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; POO graph rules are validated and resolve against one grammar identity.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/languages/arithmetic/parser
                 arithmetic-language-grammar)
        (only-in :gerbil-parser/graph-projection-support
                 make-graph-projection make-graph-node make-graph-field
                 graph-projection? graph-projection-digest fold-graph-catalog))
(import (only-in :gerbil-parser/src/compiler/graph-projection-runtime
                 graph-projection-runtime-source graph-projection-ordinal-rules)
        (only-in :gerbil-parser/event-fold-program-support
                 make-fold-graph fold-graph-start! fold-graph-token!
                 fold-graph-finish! fold-graph-publish!))


(def (expression-product projection navigation?)
  (let-values (((_ rules) (graph-projection-ordinal-rules arithmetic-language-grammar projection)))
    (let* ((rule (car rules))
           (root (car rule))
           (number (caar (cadddr rule)))
           (sink (make-fold-graph (string->utf8 "α3") "grammar"
                                  (fold-graph-catalog arithmetic-language-grammar projection)
                                  navigation?)))
      (fold-graph-start! sink root)
      (fold-graph-token! sink number 0 2)
      (fold-graph-token! sink number 2 3)
      (fold-graph-finish! sink)
      (values (fold-graph-publish! sink) root number))))

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
    (scenario "Fold publishes the shared fields and byte intervals with optional navigation"
      (scope ((projection
               (make-graph-projection
                (list (make-graph-node
                       'Expression "document" "root"
                       (list (make-graph-field 'Number "values" 'each)
                             (make-graph-field 'Number "merged")
                             (make-graph-field 'Expression "source" 'node-text)))))))
        (equal
         (let-values (((full root number) (expression-product projection #t))
                      ((sparse _root _number) (expression-product projection #f)))
           (and (equal? (vector-ref full 0) "grammar")
                (equal? (u32vector->list (vector-ref full 1))
                        (list root #xffffffff 0 3 3
                              number 0 0 2 2 number 0 2 3 3))
                (= (u32vector-length (vector-ref sparse 1)) 0)
                (equal? (vector-ref full 2) (vector-ref sparse 2))
                (equal? (vector-ref full 2)
                        '#(#(0 #f 0 3 1 ((0 (0 . 2)) (1 (0 . 2) (2 . 3))
                                         (0 (2 . 3)) (2 (0 . 3))))))))
         #t)))
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

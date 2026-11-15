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

;; A selected descendant of omitted mapped nodes still belongs to the nearest
;; selected ancestor. Omitted tokens must not leak into that ancestor's fields.
(def (selected-expression-product projection navigation? (keep-root? #t))
  (let-values (((_ rules) (graph-projection-ordinal-rules arithmetic-language-grammar projection)))
    (let* ((root (caar rules)) (number (caar (cadddr (car rules))))
           (sink (make-fold-graph (string->utf8 "1234567") "grammar"
                                  (fold-graph-catalog arithmetic-language-grammar projection)
                                  navigation? (lambda (_kind _parent start)
                                                (or (and keep-root? (= start 0)) (= start 3))))))
      (fold-graph-start! sink root)
      (fold-graph-token! sink number 0 1)
      (fold-graph-start! sink root)
      (fold-graph-token! sink number 1 2)
      (fold-graph-start! sink root)
      (fold-graph-token! sink number 2 3)
      (fold-graph-start! sink root)
      (fold-graph-token! sink number 3 4)
      (fold-graph-finish! sink)
      (fold-graph-token! sink number 4 5)
      (fold-graph-finish! sink)
      (fold-graph-token! sink number 5 6)
      (fold-graph-finish! sink)
      (fold-graph-token! sink number 6 7)
      (fold-graph-finish! sink)
      (fold-graph-publish! sink))))

(deflanguage-parser-tests graph-projection-aot-test "POO graph projection AOT"
  (engine)
    (scenario "Fold owner boundaries reject unrelated values"
      (each (owner) ('(#f () #() #(1 2 3)))
        (raises (fold-graph-start! owner 0) true)
        (raises (fold-graph-token! owner 0 0 1) true)
        (raises (fold-graph-finish! owner) true)
        (raises (fold-graph-publish! owner) true)))
    (scenario "Nested Element rows preserve preorder and parents"
      (each (navigation?) ('(#f #t))
        (scope ((rows
                 (let* ((projection (make-graph-projection
                                     (list (make-graph-node 'Expression "document" "root"
                                            (list (make-graph-field 'Number "value"))))))
                        (catalog (fold-graph-catalog arithmetic-language-grammar projection))
                        (root (let find ((index 0))
                                (if (vector-ref catalog index) index (find (+ index 1)))))
                        (inputs (vector-ref (vector-ref catalog root) 1))
                        (number (let find ((index 0))
                                  (if (pair? (vector-ref inputs index)) index (find (+ index 1)))))
                        (sink (make-fold-graph (string->utf8 (make-string 33 #\x))
                                              "grammar" catalog navigation?)))
                   (fold-graph-start! sink root)
                   (for-each (lambda (offset)
                               (fold-graph-start! sink root)
                               (fold-graph-token! sink number offset (+ offset 1))
                               (fold-graph-finish! sink)) (iota 33))
                   (fold-graph-finish! sink)
                   (vector-ref (fold-graph-publish! sink) 2))))
          (equal (vector-length rows) 34)
          (equal (vector->list (subvector (vector-ref rows 0) 1 5)) '(#f 0 33 34))
          (each (offset) ((iota 33))
            (equal (vector->list (subvector (vector-ref rows (+ offset 1)) 1 5))
                   (list 0 offset (+ offset 1) (+ offset 2)))))))
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
                        '#(#(0 #f 0 3 1 ((0 (0 . 2)) (1 (0 . 3))
                                         (0 (2 . 3)) (2 (0 . 3))))))))
         #t)))
    (scenario "selected descendants restore omitted owners without leaking fields"
      (scope ((projection
               (make-graph-projection
                (list (make-graph-node
                       'Expression "document" "root"
                       (list (make-graph-field 'Number "values")
                             (make-graph-field 'Expression "source" 'node-text)))))))
        (equal
         (let ((full (selected-expression-product projection #t))
               (sparse (selected-expression-product projection #f)))
           (and (= (u32vector-length (vector-ref full 1)) 55)
                (= (u32vector-length (vector-ref sparse 1)) 0)
                (equal? (vector-ref full 2) (vector-ref sparse 2))
                (equal? (vector-ref full 2)
                        '#(#(0 #f 0 7 2 ((0 (0 . 1) (6 . 7)) (1 (0 . 7))))
                           #(0 0 3 4 2 ((0 (3 . 4)) (1 (3 . 4))))))))
         #t)
        (each (navigation?) ('(#f #t))
          (equal
           (vector-ref (selected-expression-product projection navigation? #f) 2)
           '#(#(0 #f 3 4 1 ((0 (3 . 4)) (1 (3 . 4)))))))))
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

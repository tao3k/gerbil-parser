;;; -*- Gerbil -*-
;;; POO graph rules are validated and resolve against one grammar identity.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/languages/arithmetic/v1/grammar
                 arithmetic-language-grammar)
        (only-in :gerbil-parser/graph-projection-support
                 make-graph-projection make-graph-node make-graph-field
                 graph-projection? graph-projection-digest))
(import (only-in :gerbil-parser/src/compiler/graph-projection-rowan
                 graph-projection-rowan-source))
(export graph-projection-aot-test)

(def graph-projection-aot-test
  (test-suite "POO graph projection AOT"
    (test-case "typed graph rules bind to generated node and token kinds"
      (let* ((projection
              (make-graph-projection
               (list (make-graph-node
                      'Expression "document" "root"
                      (list (make-graph-field 'Number "value"))))))
             (source (graph-projection-rowan-source
                      arithmetic-language-grammar projection))
             (digest (graph-projection-digest
                      arithmetic-language-grammar projection)))
        (check (graph-projection? projection) => #t)
        (check (and (string-contains source "GraphProjectionSpec")
                    (string-contains source "projection_digest: \"sha256:")
                    (string-contains source "name: \"value\"")
                    (string-contains source "mode: GraphFieldMode::Append")
                    #t)
               => #t)
        (check (if (string-contains
                    source (string-append "projection_digest: \""
                                          digest "\""))
                 #t #f)
               => #t)))
    (test-case "field cardinality is checked and changes the generated contract"
      (let* ((append-rule
              (make-graph-projection
               (list (make-graph-node
                      'Expression "document" "root"
                      (list (make-graph-field 'Number "value"))))))
             (each-rule
              (make-graph-projection
               (list (make-graph-node
                      'Expression "document" "root"
                      (list (make-graph-field 'Number "value" 'each))))))
             (source (graph-projection-rowan-source
                      arithmetic-language-grammar each-rule)))
        (check (if (string-contains source "mode: GraphFieldMode::Each")
                 #t #f)
               => #t)
        (check (equal? (graph-projection-digest
                        arithmetic-language-grammar append-rule)
                       (graph-projection-digest
                        arithmetic-language-grammar each-rule))
               => #f)
        (check
         (with-catch
          (lambda (error) #t)
          (lambda () (make-graph-field 'Number "value" 'invented) #f))
         => #t)))
    (test-case "empty append fields remain Scheme-declared AOT graph values"
      (let* ((projection
              (make-graph-projection
               (list (make-graph-node
                      'Expression "document" "root"
                      (list (make-graph-field
                             'Number "value" 'append-or-empty))))))
             (source (graph-projection-rowan-source
                      arithmetic-language-grammar projection)))
        (check (graph-projection? projection) => #t)
        (check (if (string-contains
                    source "mode: GraphFieldMode::AppendOrEmpty")
                 #t #f)
               => #t)))
    (test-case "duplicate graph node owners are rejected"
      (check
       (with-catch
        (lambda (error) #t)
        (lambda ()
          (make-graph-projection
           (list (make-graph-node 'Expression "document" "root" '())
                 (make-graph-node 'Expression "element" "duplicate" '())))
          #f))
       => #t))
    (test-case "wrong-category syntax references are rejected at AOT"
      (check
       (with-catch
        (lambda (error) #t)
        (lambda ()
          (graph-projection-rowan-source
           arithmetic-language-grammar
           (make-graph-projection
            (list (make-graph-node 'Number "document" "root" '()))))
          #f))
       => #t))))

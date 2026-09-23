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
                    (string-contains source "name: \"value\"") #t)
               => #t)
        (check (if (string-contains
                    source (string-append "projection_digest: \""
                                          digest "\""))
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

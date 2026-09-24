;;; -*- Gerbil -*-
;;; Engine-specific, exact AST test syntax over canonical ParseArtifact v1.

(import (only-in :std/test check)
        (only-in ../runtime/artifact
                 parse-artifact-roundtrip parse-artifact-success?
                 parse-artifact-valid?)
        (only-in ../runtime/cst
                 parse-artifact->cst
                 syntax-node? syntax-node-kind syntax-node-start
                 syntax-node-end syntax-node-children
                 syntax-field? syntax-field-name syntax-field-start
                 syntax-field-end syntax-field-children)
        (only-in ../runtime/token
                 token? token-kind token-lexeme token-start token-end))
(export check-parser-ast parser-ast-pattern parser-ast-diff
        parser-artifact-ast-diff)

;; This is an AST grammar for this parser engine, not a general data matcher.
;; Fields and byte spans are mandatory; omitted structure cannot pass by a
;; source-roundtrip-only assertion.
(defrules parser-ast-pattern (node field token)
  ((_ (node kind start end child ...))
   (list 'node 'kind start end
         (list (parser-ast-pattern child) ...)))
  ((_ (field name start end child ...))
   (list 'field 'name start end
         (list (parser-ast-pattern child) ...)))
  ((_ (token kind lexeme start end))
   (list 'token 'kind lexeme start end)))

(def (ast-children-diff actual expected path)
  (cond
   ((not (= (length actual) (length expected)))
    (list path 'child-count (length expected) (length actual)))
   (else
    (let loop ((actual actual) (expected expected) (index 0))
      (and (pair? actual)
           (or (parser-ast-diff
                (car actual) (car expected)
                (cons index path))
               (loop (cdr actual) (cdr expected) (+ index 1))))))))

(def (ast-part-diff actual expected path type predicate kind start end children)
  (cond
   ((not (predicate actual)) (list path 'category type actual))
   ((not (eq? (kind actual) (cadr expected)))
    (list path 'kind (cadr expected) (kind actual)))
   ((not (= (start actual) (caddr expected)))
    (list path 'start (caddr expected) (start actual)))
   ((not (= (end actual) (cadddr expected)))
    (list path 'end (cadddr expected) (end actual)))
   (else (ast-children-diff (children actual)
                            (car (cddddr expected)) path))))

(def (parser-ast-diff actual expected (path '()))
  (case (car expected)
    ((node)
     (ast-part-diff actual expected path 'node
                    syntax-node? syntax-node-kind
                    syntax-node-start syntax-node-end syntax-node-children))
    ((field)
     (ast-part-diff actual expected path 'field
                    syntax-field? syntax-field-name
                    syntax-field-start syntax-field-end syntax-field-children))
    ((token)
     (cond
      ((not (token? actual)) (list path 'category 'token actual))
      ((not (eq? (token-kind actual) (cadr expected)))
       (list path 'kind (cadr expected) (token-kind actual)))
      ((not (equal? (token-lexeme actual) (caddr expected)))
       (list path 'lexeme (caddr expected) (token-lexeme actual)))
      ((not (= (token-start actual) (cadddr expected)))
       (list path 'start (cadddr expected) (token-start actual)))
      ((not (= (token-end actual) (car (cddddr expected))))
       (list path 'end (car (cddddr expected)) (token-end actual)))
      (else #f)))
    (else (error "invalid parser AST expectation" expected))))

;; One engine-owned acceptance contract. A rejected or malformed artifact
;; produces a typed test difference instead of throwing during CST replay.
(def (parser-artifact-ast-diff artifact source expected)
  (cond
   ((not (parse-artifact-valid? artifact))
    (list '() 'invalid-artifact))
   ((not (parse-artifact-success? artifact))
    (list '() 'status 'accepted 'rejected))
   ((not (equal? (parse-artifact-roundtrip artifact) source))
    (list '() 'source source (parse-artifact-roundtrip artifact)))
   (else
    (parser-ast-diff (parse-artifact->cst artifact) expected))))

(defrules check-parser-ast ()
  ((_ artifact source expected)
   (let (result artifact)
     (check (parser-artifact-ast-diff
             result source (parser-ast-pattern expected))
            => #f))))

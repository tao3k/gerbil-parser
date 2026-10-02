#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Project a bounded TLA+ expression family for SANY structural comparison.

(import (only-in :std/misc/ports read-all-as-string)
        (only-in :std/encoding/json json->string)
        :gerbil-parser/languages/tla-plus/sany-candidate
        (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-success? parse-artifact-valid?
                 parse-artifact-roundtrip)
        :gerbil-parser/src/runtime/cst
        (only-in :gerbil-parser/src/runtime/token token? token-lexeme))

(def (child-field node name)
  (let loop ((children (syntax-node-children node)))
    (cond
     ((null? children) (error "missing syntax field" (syntax-node-kind node) name))
     ((and (syntax-field? (car children))
           (eq? (syntax-field-name (car children)) name))
      (car children))
     (else (loop (cdr children))))))

(def (first-descendant value predicate)
  (cond
   ((predicate value) value)
   ((syntax-node? value)
    (let loop ((children (syntax-node-children value)))
      (and (pair? children)
           (or (first-descendant (car children) predicate)
               (loop (cdr children))))))
   ((syntax-field? value)
    (let loop ((children (syntax-field-children value)))
      (and (pair? children)
           (or (first-descendant (car children) predicate)
               (loop (cdr children))))))
   (else #f)))

(def (field-node node name)
  (or (first-descendant (child-field node name) syntax-node?)
      (error "syntax field has no node" (syntax-node-kind node) name)))

(def (field-text node name)
  (let (token (first-descendant (child-field node name) token?))
    (if token (token-lexeme token)
        (error "syntax field has no token" (syntax-node-kind node) name))))

(def (project-expression node)
  (case (syntax-node-kind node)
    ((Expression)
     (vector (field-text node 'operator)
             (project-expression (field-node node 'left))
             (project-expression (field-node node 'right))))
    ((GroupedExpression)
     (project-expression (field-node node 'expression)))
    ((NameExpression)
     (field-text node 'name))
    (else (error "unsupported differential expression"
                 (syntax-node-kind node)))))

(def (operator-definitions value)
  (cond
   ((syntax-node? value)
    (append
     (if (eq? (syntax-node-kind value) 'OperatorDefinition)
       (list value) '())
     (apply append (map operator-definitions (syntax-node-children value)))))
   ((syntax-field? value)
    (apply append (map operator-definitions (syntax-field-children value))))
   (else '())))

(def (emit-shapes!)
  (let* ((path (car (reverse (command-line))))
         (source (call-with-input-file path read-all-as-string))
         (artifact (parse-tla-plus-sany-candidate source)))
    (if (not (parse-artifact-success? artifact))
      (displayln (json->string (hash ("accepted" #f))))
      (begin
        (unless (and (parse-artifact-valid? artifact)
                     (equal? source (parse-artifact-roundtrip artifact)))
          (error "candidate artifact is not source-exact" path))
        (let (shapes (hash))
          (for-each
           (lambda (definition)
             (hash-put! shapes
                        (field-text definition 'name)
                        (project-expression (field-node definition 'body))))
           (operator-definitions (parse-artifact->cst artifact)))
          (displayln (json->string
                      (hash ("accepted" #t) ("shapes" shapes))
                      sort-keys: #t)))))))

(emit-shapes!)

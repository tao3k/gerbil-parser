;;; -*- Gerbil -*-
;;; Canonical POO-native ISO GQL query syntax graph and source projection.
;;;
;;; The syntax graph is language-owned. Domain packages extend these
;;; prototypes and never reimplement GQL quoting or source rendering.

(import (only-in :clan/poo/object .o .ref .slot? object?)
        (only-in :gerbil/core string-join)
        (only-in :std/list/list every)
        (only-in :std/string/misc string-concatenate string-subst))

(export GqlQueryNode.
        GqlQueryStep.
        GqlQueryPath.
        GqlQueryProperty.
        GqlQueryLiteral.
        GqlQueryEquals.
        GqlQueryProjection.
        GqlQueryProgram.
        gql-query-program?
        gql-query-program->source)

(def (gql-has-slots? value slots)
  (and (object? value)
       (every (lambda (slot) (.slot? value slot)) slots)))

(def GqlQueryNode.
  (.o syntax-kind: 'gql.node binding: #f label: #f))

(def GqlQueryStep.
  (.o syntax-kind: 'gql.step relation: #f target: #f next: #f))

(def GqlQueryPath.
  (.o syntax-kind: 'gql.path start: #f next: #f))

(def GqlQueryProperty.
  (.o syntax-kind: 'gql.property binding: #f property: #f))

(def GqlQueryLiteral.
  (.o syntax-kind: 'gql.literal literal-kind: #f value: #f))

(def GqlQueryEquals.
  (.o syntax-kind: 'gql.equals left: #f right: #f))

(def GqlQueryProjection.
  (.o syntax-kind: 'gql.projection expression: #f next: #f))

(def GqlQueryProgram.
  (.o syntax-kind: 'gql.program match: #f where: #f project: #f))

(def (gql-query-node? value)
  (and (gql-has-slots? value '(syntax-kind binding label))
       (eq? (.ref value 'syntax-kind) 'gql.node)
       (symbol? (.ref value 'binding))
       (symbol? (.ref value 'label))))

(def (gql-query-step? value)
  (and (gql-has-slots? value '(syntax-kind relation target next))
       (eq? (.ref value 'syntax-kind) 'gql.step)
       (symbol? (.ref value 'relation))
       (gql-query-node? (.ref value 'target))
       (or (not (.ref value 'next))
           (gql-query-step? (.ref value 'next)))))

(def (gql-query-path? value)
  (and (gql-has-slots? value '(syntax-kind start next))
       (eq? (.ref value 'syntax-kind) 'gql.path)
       (gql-query-node? (.ref value 'start))
       (or (not (.ref value 'next))
           (gql-query-step? (.ref value 'next)))))

(def (gql-query-property? value)
  (and (gql-has-slots? value '(syntax-kind binding property))
       (eq? (.ref value 'syntax-kind) 'gql.property)
       (symbol? (.ref value 'binding))
       (symbol? (.ref value 'property))))

(def (gql-query-literal? value)
  (and (gql-has-slots? value '(syntax-kind literal-kind value))
       (eq? (.ref value 'syntax-kind) 'gql.literal)
       (case (.ref value 'literal-kind)
         ((string) (string? (.ref value 'value)))
         ((symbol) (symbol? (.ref value 'value)))
         ((integer) (exact-integer? (.ref value 'value)))
         ((boolean) (boolean? (.ref value 'value)))
         (else #f))))

(def (gql-query-equals? value)
  (and (gql-has-slots? value '(syntax-kind left right))
       (eq? (.ref value 'syntax-kind) 'gql.equals)
       (gql-query-property? (.ref value 'left))
       (gql-query-literal? (.ref value 'right))))

(def (gql-query-projection? value)
  (and (gql-has-slots? value '(syntax-kind expression next))
       (eq? (.ref value 'syntax-kind) 'gql.projection)
       (gql-query-property? (.ref value 'expression))
       (or (not (.ref value 'next))
           (gql-query-projection? (.ref value 'next)))))

(def (gql-query-program? value)
  (and (gql-has-slots? value '(syntax-kind match where project))
       (eq? (.ref value 'syntax-kind) 'gql.program)
       (gql-query-path? (.ref value 'match))
       (or (not (.ref value 'where))
           (gql-query-equals? (.ref value 'where)))
       (gql-query-projection? (.ref value 'project))))

(def (gql-name value)
  (if (symbol? value) (symbol->string value) value))

(def (gql-string value)
  (string-append "'" (string-subst value "'" "''") "'"))

(def (gql-node-document node)
  (string-append "(" (gql-name (.ref node 'binding)) ":"
                 (gql-name (.ref node 'label)) ")"))

(def (gql-path-document path)
  (let loop ((step (.ref path 'next))
             (documents (list (gql-node-document (.ref path 'start)))))
    (if step
      (loop (.ref step 'next)
            (cons
             (string-append "-[:" (gql-name (.ref step 'relation)) "]->"
                            (gql-node-document (.ref step 'target)))
             documents))
      (string-concatenate (reverse documents)))))

(def (gql-property-document property)
  (string-append (gql-name (.ref property 'binding)) "."
                 (gql-name (.ref property 'property))))

(def (gql-literal-document literal)
  (case (.ref literal 'literal-kind)
    ((string) (gql-string (.ref literal 'value)))
    ((symbol) (gql-string (symbol->string (.ref literal 'value))))
    ((integer) (number->string (.ref literal 'value)))
    ((boolean) (if (.ref literal 'value) "TRUE" "FALSE"))))

(def (gql-equals-document equals)
  (string-append (gql-property-document (.ref equals 'left)) " = "
                 (gql-literal-document (.ref equals 'right))))

(def (gql-projection-document projection)
  (let loop ((current projection) (documents '()))
    (if current
      (loop (.ref current 'next)
            (cons (gql-property-document (.ref current 'expression))
                  documents))
      (string-join (reverse documents) ", "))))

(def (gql-query-program->source program)
  (unless (gql-query-program? program)
    (error "invalid GQL query syntax graph" program))
  (string-concatenate
   (list
    (string-append "MATCH " (gql-path-document (.ref program 'match)) "\n")
    (if (.ref program 'where)
      (string-append "WHERE "
                     (gql-equals-document (.ref program 'where)) "\n")
      "")
    (string-append "RETURN "
                   (gql-projection-document (.ref program 'project)) "\n"))))

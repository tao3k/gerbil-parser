;;; -*- Gerbil -*-
;;; Shared POO graph-pattern syntax and constrained MATCH/WHERE/RETURN rendering.

(import (only-in :clan/poo/object .o .ref .slot? object?)
        (only-in :gerbil/core string-join)
        (only-in :std/list/list every)
        (only-in :std/string/misc string-concatenate string-subst))

(export GraphSyntaxNode.
        GraphSyntaxStep.
        GraphSyntaxPath.
        GraphSyntaxProperty.
        GraphSyntaxLiteral.
        GraphSyntaxEquals.
        GraphSyntaxProjection.
        GraphSyntaxProgram.
        graph-syntax-program?
        graph-syntax-program->source)

(def (graph-has-slots? value slots)
  (and (object? value)
       (every (lambda (slot) (.slot? value slot)) slots)))

(def GraphSyntaxNode.
  (.o syntax-kind: 'graph.syntax.node binding: #f label: #f))

(def GraphSyntaxStep.
  (.o syntax-kind: 'graph.syntax.step relation: #f target: #f next: #f))

(def GraphSyntaxPath.
  (.o syntax-kind: 'graph.syntax.path start: #f next: #f))

(def GraphSyntaxProperty.
  (.o syntax-kind: 'graph.syntax.property binding: #f property: #f))

(def GraphSyntaxLiteral.
  (.o syntax-kind: 'graph.syntax.literal literal-kind: #f value: #f))

(def GraphSyntaxEquals.
  (.o syntax-kind: 'graph.syntax.equals left: #f right: #f))

(def GraphSyntaxProjection.
  (.o syntax-kind: 'graph.syntax.projection expression: #f next: #f))

(def GraphSyntaxProgram.
  (.o syntax-kind: 'graph.syntax.program match: #f where: #f project: #f))

(def (graph-syntax-node? value)
  (and (graph-has-slots? value '(syntax-kind binding label))
       (eq? (.ref value 'syntax-kind) 'graph.syntax.node)
       (symbol? (.ref value 'binding))
       (symbol? (.ref value 'label))))

(def (graph-syntax-step? value (remaining 256))
  (and (positive? remaining) (graph-has-slots? value '(syntax-kind relation target next))
       (eq? (.ref value 'syntax-kind) 'graph.syntax.step)
       (symbol? (.ref value 'relation))
       (graph-syntax-node? (.ref value 'target))
       (or (not (.ref value 'next))
           (graph-syntax-step? (.ref value 'next) (- remaining 1)))))

(def (graph-syntax-path? value)
  (and (graph-has-slots? value '(syntax-kind start next))
       (eq? (.ref value 'syntax-kind) 'graph.syntax.path)
       (graph-syntax-node? (.ref value 'start))
       (or (not (.ref value 'next))
           (graph-syntax-step? (.ref value 'next) 256))))

(def (graph-syntax-property? value)
  (and (graph-has-slots? value '(syntax-kind binding property))
       (eq? (.ref value 'syntax-kind) 'graph.syntax.property)
       (symbol? (.ref value 'binding))
       (symbol? (.ref value 'property))))

(def (graph-syntax-literal? value)
  (and (graph-has-slots? value '(syntax-kind literal-kind value))
       (eq? (.ref value 'syntax-kind) 'graph.syntax.literal)
       (case (.ref value 'literal-kind)
         ((string) (string? (.ref value 'value)))
         ((symbol) (symbol? (.ref value 'value)))
         ((integer) (exact-integer? (.ref value 'value)))
         ((boolean) (boolean? (.ref value 'value)))
         (else #f))))

(def (graph-syntax-equals? value)
  (and (graph-has-slots? value '(syntax-kind left right))
       (eq? (.ref value 'syntax-kind) 'graph.syntax.equals)
       (graph-syntax-property? (.ref value 'left))
       (graph-syntax-literal? (.ref value 'right))))

(def (graph-syntax-projection? value (remaining 256))
  (and (positive? remaining) (graph-has-slots? value '(syntax-kind expression next))
       (eq? (.ref value 'syntax-kind) 'graph.syntax.projection)
       (graph-syntax-property? (.ref value 'expression))
       (or (not (.ref value 'next))
           (graph-syntax-projection? (.ref value 'next) (- remaining 1)))))

(def (graph-syntax-program? value)
  (and (graph-has-slots? value '(syntax-kind match where project))
       (eq? (.ref value 'syntax-kind) 'graph.syntax.program)
       (graph-syntax-path? (.ref value 'match))
       (or (not (.ref value 'where))
           (graph-syntax-equals? (.ref value 'where)))
       (graph-syntax-projection? (.ref value 'project))))

(def (graph-name value)
  (if (symbol? value) (symbol->string value) value))

(def (graph-string value)
  (string-append "'" (string-subst value "'" "''") "'"))

(def (graph-node-document node)
  (string-append "(" (graph-name (.ref node 'binding)) ":"
                 (graph-name (.ref node 'label)) ")"))

(def (graph-path-document path)
  (let loop ((step (.ref path 'next))
             (documents (list (graph-node-document (.ref path 'start)))))
    (if step
      (loop (.ref step 'next)
            (cons
             (string-append "-[:" (graph-name (.ref step 'relation)) "]->"
                            (graph-node-document (.ref step 'target)))
             documents))
      (string-concatenate (reverse documents)))))

(def (graph-property-document property)
  (string-append (graph-name (.ref property 'binding)) "."
                 (graph-name (.ref property 'property))))

(def (graph-literal-document literal)
  (case (.ref literal 'literal-kind)
    ((string) (graph-string (.ref literal 'value)))
    ((symbol) (graph-string (symbol->string (.ref literal 'value))))
    ((integer) (number->string (.ref literal 'value)))
    ((boolean) (if (.ref literal 'value) "TRUE" "FALSE"))))

(def (graph-equals-document equals)
  (string-append (graph-property-document (.ref equals 'left)) " = "
                 (graph-literal-document (.ref equals 'right))))

(def (graph-projection-document projection)
  (let loop ((current projection) (documents '()))
    (if current
      (loop (.ref current 'next)
            (cons (graph-property-document (.ref current 'expression))
                  documents))
      (string-join (reverse documents) ", "))))

(def (graph-syntax-program->source program)
  (unless (graph-syntax-program? program)
    (error "invalid graph-pattern syntax" program))
  (string-concatenate
   (list
    (string-append "MATCH " (graph-path-document (.ref program 'match)) "\n")
    (if (.ref program 'where)
      (string-append "WHERE "
                     (graph-equals-document (.ref program 'where)) "\n")
      "")
    (string-append "RETURN "
                   (graph-projection-document (.ref program 'project)) "\n"))))

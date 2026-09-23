;;; -*- Gerbil -*-
;;; POO-native contextual parser declarations; no public alist DSL.

(import (only-in :clan/poo/object .o .ref)
        (only-in :poo-flow/src/module-system/types
                 poo-flow-contract-admit
                 poo-flow-validation-evidence-accepted?)
        (only-in :poo-flow/src/module-system/object-family/interface
                 defpoo-object-family)
        (only-in ./line-structure-types
                 +line-structure-schema+ +line-structure-kind+
                 +heading-line-kind+ +block-line-kind+ +text-line-kind+
                 LineStructureContract HeadingLineContract
                 BlockLineContract TextLineContract))
(export LineStructure. HeadingLine. BlockLine. TextLine.
        make-line-structure make-heading-line make-block-line make-text-line
        line-structure? heading-line? block-line? text-line?
        line-structure-heading line-structure-blocks line-structure-text
        heading-line-marker heading-line-separator
        heading-line-section-node heading-line-heading-node
        heading-line-heading-token
        block-line-opening block-line-closing block-line-case-insensitive
        block-line-indent block-line-block-node block-line-begin-token
        block-line-body-token block-line-end-token
        text-line-node text-line-token)

(def LineStructure. (.ref LineStructureContract 'proto))
(def HeadingLine. (.ref HeadingLineContract 'proto))
(def BlockLine. (.ref BlockLineContract 'proto))
(def TextLine. (.ref TextLineContract 'proto))

(def (admit-line! contract candidate)
  (let (evidence (poo-flow-contract-admit contract candidate #f))
    (unless (poo-flow-validation-evidence-accepted? evidence)
      (error "invalid POO line-structure declaration"
             (.ref evidence 'diagnostics)))
    candidate))

(def (admitted-line? contract candidate)
  (poo-flow-validation-evidence-accepted?
   (poo-flow-contract-admit contract candidate #f)))

(def (make-heading-line marker-value separator-value
                        section-node-value heading-node-value heading-token-value)
  (let (candidate
        (.o (:: @ HeadingLine.)
            kind: +heading-line-kind+
            marker: marker-value
            separator: separator-value
            section-node: section-node-value
            heading-node: heading-node-value
            heading-token: heading-token-value))
    (admit-line! HeadingLineContract candidate)))

(def (make-block-line opening-value closing-value
                      case-insensitive-value indent-value
                      block-node-value begin-token-value
                      body-token-value end-token-value)
  (admit-line! BlockLineContract
            (.o (:: @ BlockLine.)
                kind: +block-line-kind+
                opening: opening-value
                closing: closing-value
                case-insensitive: case-insensitive-value
                indent: indent-value
                block-node: block-node-value
                begin-token: begin-token-value
                body-token: body-token-value
                end-token: end-token-value)))

(def (make-text-line node-value token-value)
  (admit-line! TextLineContract
            (.o (:: @ TextLine.)
                kind: +text-line-kind+
                text-node: node-value
                text-token: token-value)))

(def (make-line-structure heading-value blocks-value text-value)
  (admit-line! LineStructureContract
            (.o (:: @ LineStructure.)
                kind: +line-structure-kind+
                schema: +line-structure-schema+
                heading: heading-value
                blocks: blocks-value
                text: text-value)))

(defpoo-object-family
  (accessors
   (line-structure-heading heading)
   (line-structure-blocks blocks)
   (line-structure-text text)
   (heading-line-marker marker)
   (heading-line-separator separator)
   (heading-line-section-node section-node)
   (heading-line-heading-node heading-node)
   (heading-line-heading-token heading-token)
   (block-line-opening opening)
   (block-line-closing closing)
   (block-line-case-insensitive case-insensitive)
   (block-line-indent indent)
   (block-line-block-node block-node)
   (block-line-begin-token begin-token)
   (block-line-body-token body-token)
   (block-line-end-token end-token)
   (text-line-node text-node)
   (text-line-token text-token))
  (projections))

(def (line-structure? value)
  (admitted-line? LineStructureContract value))
(def (heading-line? value)
  (admitted-line? HeadingLineContract value))
(def (block-line? value)
  (admitted-line? BlockLineContract value))
(def (text-line? value)
  (admitted-line? TextLineContract value))

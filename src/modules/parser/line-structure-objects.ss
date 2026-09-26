;;; -*- Gerbil -*-
;;; POO-native contextual parser declarations; no public alist DSL.

(import (only-in :clan/poo/object .o .ref)
        (only-in :core/types
                 poo-flow-contract-admit
                 poo-flow-validation-evidence-accepted?)
        (only-in :core/object-family/interface
                 defpoo-object-family)
        (only-in ./line-structure-types
                 +line-structure-schema+ +line-structure-kind+
                 +heading-line-kind+ +block-line-kind+ +text-line-kind+
                 +key-value-line-kind+ +block-header-kind+ +inline-link-kind+
                 +heading-fields-kind+
                 +table-line-kind+ +list-line-kind+ +key-line-kind+
                 LineStructureContract HeadingLineContract
                 BlockLineContract TextLineContract KeyValueLineContract
                 BlockHeaderContract InlineLinkContract HeadingFieldsContract
                 TableLineContract ListLineContract KeyLineContract))
(export LineStructure. HeadingLine. BlockLine. TextLine. KeyValueLine. BlockHeader. InlineLink. HeadingFields. TableLine. ListLine. KeyLine.
        make-line-structure make-heading-line make-block-line make-text-line
        make-table-line make-list-line make-key-line
        make-key-value-line make-block-header make-inline-link make-heading-fields
        line-structure? heading-line? block-line? text-line? key-value-line?
        line-structure-heading line-structure-blocks line-structure-text
        line-structure-table line-structure-list line-structure-key-lines
        heading-line-marker heading-line-separator
        heading-line-section-node heading-line-heading-node
        heading-line-heading-token
        heading-line-fields heading-fields-title-token heading-fields-trivia-token
        block-line-opening block-line-opening-mode block-line-closing block-line-case-insensitive
        block-line-indent block-line-block-node block-line-begin-token
        block-line-body-token block-line-end-token
        block-line-unclosed block-line-heading-bound block-line-body-line
        block-line-contents
        block-line-header block-header-argument-token block-header-trivia-token
        key-value-line-marker key-value-line-node
        key-value-line-key-token key-value-line-value-token
        key-value-line-trivia-token
        text-line-node text-line-token text-line-paragraph-node text-line-inline-link
        inline-link-opening inline-link-separator inline-link-closing
        inline-link-node inline-link-target-token
        inline-link-description-token inline-link-trivia-token
        table-line-delimiter table-line-table-node table-line-row-node
        table-line-rule-row-node table-line-cell-node
        table-line-separator-token table-line-cell-token
        table-line-trivia-token table-line-rule-token
        list-line-unordered-markers list-line-ordered
        list-line-tab-width
        list-line-list-node list-line-item-node
        list-line-bullet-token list-line-trivia-token
        key-line-prefix key-line-keys key-line-separator
        key-line-case-insensitive key-line-indent key-line-after-heading
        key-line-repeated key-line-node key-line-key-token
        key-line-value-token key-line-trivia-token)

(def LineStructure. (.ref LineStructureContract 'proto))
(def HeadingLine. (.ref HeadingLineContract 'proto))
(def BlockLine. (.ref BlockLineContract 'proto))
(def TextLine. (.ref TextLineContract 'proto))
(def KeyValueLine. (.ref KeyValueLineContract 'proto))
(def BlockHeader. (.ref BlockHeaderContract 'proto))
(def InlineLink. (.ref InlineLinkContract 'proto))
(def HeadingFields. (.ref HeadingFieldsContract 'proto))
(def TableLine. (.ref TableLineContract 'proto))
(def ListLine. (.ref ListLineContract 'proto))
(def KeyLine. (.ref KeyLineContract 'proto))

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
                        section-node-value heading-node-value heading-token-value
                        (fields-value #f))
  (let (candidate
        (.o (:: @ HeadingLine.)
            kind: +heading-line-kind+
            marker: marker-value
            separator: separator-value
            section-node: section-node-value
            heading-node: heading-node-value
            heading-token: heading-token-value
            fields: fields-value))
    (admit-line! HeadingLineContract candidate)))

(def (make-heading-fields title-token-value trivia-token-value)
  (admit-line! HeadingFieldsContract
               (.o (:: @ HeadingFields.)
                   kind: +heading-fields-kind+
                   title-token: title-token-value
                   trivia-token: trivia-token-value)))

(def (make-block-line opening-value closing-value
                      case-insensitive-value indent-value
                      block-node-value begin-token-value
                      body-token-value end-token-value unclosed-value
                      heading-bound-value body-line-value (header-value #f)
                      (contents-value 'opaque) (opening-mode-value 'literal))
  (admit-line! BlockLineContract
            (.o (:: @ BlockLine.)
                kind: +block-line-kind+
                opening: opening-value
                opening-mode: opening-mode-value
                closing: closing-value
                case-insensitive: case-insensitive-value
                indent: indent-value
                block-node: block-node-value
                begin-token: begin-token-value
                body-token: body-token-value
                end-token: end-token-value
                unclosed: unclosed-value
                heading-bound: heading-bound-value
                contents: contents-value
                body-line: body-line-value
                header: header-value)))

(def (make-block-header argument-token-value trivia-token-value)
  (admit-line! BlockHeaderContract
               (.o (:: @ BlockHeader.)
                   kind: +block-header-kind+
                   argument-token: argument-token-value
                   trivia-token: trivia-token-value)))

(def (make-key-value-line marker-value node-value
                          key-token-value value-token-value trivia-token-value)
  (admit-line! KeyValueLineContract
               (.o (:: @ KeyValueLine.)
                   kind: +key-value-line-kind+
                   marker: marker-value
                   node: node-value
                   key-token: key-token-value
                   value-token: value-token-value
                   trivia-token: trivia-token-value)))

(def (make-inline-link opening-value separator-value closing-value node-value
                       target-token-value description-token-value trivia-token-value)
  (admit-line! InlineLinkContract
               (.o (:: @ InlineLink.)
                   kind: +inline-link-kind+
                   opening: opening-value
                   separator: separator-value
                   closing: closing-value
                   node: node-value
                   target-token: target-token-value
                   description-token: description-token-value
                   trivia-token: trivia-token-value)))

(def (make-text-line node-value token-value (inline-link-value #f)
                     (paragraph-node-value #f))
  (admit-line! TextLineContract
            (.o (:: @ TextLine.)
                kind: +text-line-kind+
                text-node: node-value
                text-token: token-value
                paragraph-node: paragraph-node-value
                inline-link: inline-link-value)))

(def (make-table-line delimiter-value table-node-value row-node-value
                      rule-row-node-value cell-node-value separator-token-value
                      cell-token-value trivia-token-value rule-token-value)
  (admit-line! TableLineContract
               (.o (:: @ TableLine.)
                   kind: +table-line-kind+
                   delimiter: delimiter-value
                   table-node: table-node-value
                   row-node: row-node-value
                   rule-row-node: rule-row-node-value
                   cell-node: cell-node-value
                   separator-token: separator-token-value
                   cell-token: cell-token-value
                   trivia-token: trivia-token-value
                   rule-token: rule-token-value)))

(def (make-list-line unordered-markers-value ordered-value list-node-value
                     item-node-value bullet-token-value trivia-token-value
                     (tab-width-value 8))
  (admit-line! ListLineContract
               (.o (:: @ ListLine.)
                   kind: +list-line-kind+
                   unordered-markers: unordered-markers-value
                   ordered: ordered-value
                   tab-width: tab-width-value
                   list-node: list-node-value
                   item-node: item-node-value
                   bullet-token: bullet-token-value
                   trivia-token: trivia-token-value)))

(def (make-key-line prefix-value keys-value separator-value
                    case-insensitive-value indent-value after-heading-value
                    repeated-value node-value key-token-value
                    value-token-value trivia-token-value)
  (admit-line! KeyLineContract
               (.o (:: @ KeyLine.)
                   kind: +key-line-kind+
                   prefix: prefix-value
                   keys: keys-value
                   separator: separator-value
                   case-insensitive: case-insensitive-value
                   indent: indent-value
                   after-heading: after-heading-value
                   repeated: repeated-value
                   node: node-value
                   key-token: key-token-value
                   value-token: value-token-value
                   trivia-token: trivia-token-value)))

(def (make-line-structure heading-value blocks-value text-value
                          (table-value #f) (list-value #f) (key-lines-value '()))
  (admit-line! LineStructureContract
            (.o (:: @ LineStructure.)
                kind: +line-structure-kind+
                schema: +line-structure-schema+
                heading: heading-value
                blocks: blocks-value
                text: text-value
                table: table-value
                list: list-value
                key-lines: key-lines-value)))

(defpoo-object-family
  (accessors
   (line-structure-heading heading)
   (line-structure-blocks blocks)
   (line-structure-text text)
   (line-structure-table table)
   (line-structure-list list)
   (line-structure-key-lines key-lines)
   (heading-line-marker marker)
   (heading-line-separator separator)
   (heading-line-section-node section-node)
   (heading-line-heading-node heading-node)
   (heading-line-heading-token heading-token)
   (heading-line-fields fields)
   (heading-fields-title-token title-token)
   (heading-fields-trivia-token trivia-token)
   (block-line-opening opening)
   (block-line-opening-mode opening-mode)
   (block-line-closing closing)
   (block-line-case-insensitive case-insensitive)
   (block-line-indent indent)
   (block-line-block-node block-node)
   (block-line-begin-token begin-token)
   (block-line-body-token body-token)
   (block-line-end-token end-token)
   (block-line-unclosed unclosed)
   (block-line-heading-bound heading-bound)
   (block-line-contents contents)
   (block-line-body-line body-line)
   (block-line-header header)
   (block-header-argument-token argument-token)
   (block-header-trivia-token trivia-token)
   (key-value-line-marker marker)
   (key-value-line-node node)
   (key-value-line-key-token key-token)
   (key-value-line-value-token value-token)
   (key-value-line-trivia-token trivia-token)
   (text-line-node text-node)
   (text-line-token text-token)
   (text-line-paragraph-node paragraph-node)
   (text-line-inline-link inline-link)
   (inline-link-opening opening)
   (inline-link-separator separator)
   (inline-link-closing closing)
   (inline-link-node node)
   (inline-link-target-token target-token)
   (inline-link-description-token description-token)
   (inline-link-trivia-token trivia-token)
   (table-line-delimiter delimiter)
   (table-line-table-node table-node)
   (table-line-row-node row-node)
   (table-line-rule-row-node rule-row-node)
   (table-line-cell-node cell-node)
   (table-line-separator-token separator-token)
   (table-line-cell-token cell-token)
   (table-line-trivia-token trivia-token)
   (table-line-rule-token rule-token)
   (list-line-unordered-markers unordered-markers)
   (list-line-ordered ordered)
   (list-line-tab-width tab-width)
   (list-line-list-node list-node)
   (list-line-item-node item-node)
   (list-line-bullet-token bullet-token)
   (list-line-trivia-token trivia-token)
   (key-line-prefix prefix)
   (key-line-keys keys)
   (key-line-separator separator)
   (key-line-case-insensitive case-insensitive)
   (key-line-indent indent)
   (key-line-after-heading after-heading)
   (key-line-repeated repeated)
   (key-line-node node)
   (key-line-key-token key-token)
   (key-line-value-token value-token)
   (key-line-trivia-token trivia-token))
  (projections))

(def (line-structure? value)
  (admitted-line? LineStructureContract value))
(def (heading-line? value)
  (admitted-line? HeadingLineContract value))
(def (block-line? value)
  (admitted-line? BlockLineContract value))
(def (text-line? value)
  (admitted-line? TextLineContract value))
(def (key-value-line? value)
  (admitted-line? KeyValueLineContract value))

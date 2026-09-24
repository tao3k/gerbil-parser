;;; -*- Gerbil -*-
;;; POO line strategy -> typed Rust syntax -> one immutable Rowan table.

(import (only-in ../language/descriptor
                 language-grammar-ir language-grammar-machine)
        (only-in ../runtime/identity sha256-text)
        (only-in ./machine parser-machine-grammar-digest)
        (only-in ./rust-syntax
                 rust-struct rust-static rust-array rust-some rust-none
                 rust-number rust-string rust-identifier rust-module rust-render)
        (only-in ../modules/parser/line-structure-objects
                 line-structure?
                 line-structure-heading line-structure-blocks line-structure-text
                 line-structure-table line-structure-list line-structure-key-lines
                 heading-line-marker heading-line-separator
                 heading-line-section-node heading-line-heading-node
                 heading-line-heading-token heading-line-fields
                 heading-fields-title-token heading-fields-trivia-token
                 block-line-opening block-line-opening-mode block-line-closing block-line-case-insensitive
                 block-line-indent block-line-block-node block-line-begin-token
                 block-line-body-token block-line-end-token block-line-unclosed
                 block-line-heading-bound block-line-body-line
                 block-line-contents
                 block-line-header block-header-argument-token
                 block-header-trivia-token
                 key-value-line-marker key-value-line-node
                 key-value-line-key-token key-value-line-value-token
                 key-value-line-trivia-token
                 text-line-node text-line-token text-line-paragraph-node
                 text-line-inline-link
                 inline-link-opening inline-link-separator inline-link-closing
                 inline-link-node inline-link-target-token
                 inline-link-description-token inline-link-trivia-token
                 table-line-delimiter table-line-table-node table-line-row-node
                 table-line-rule-row-node table-line-cell-node
                 table-line-separator-token table-line-cell-token
                 table-line-trivia-token table-line-rule-token
                 list-line-unordered-markers list-line-ordered list-line-tab-width
                 list-line-list-node list-line-item-node
                 list-line-bullet-token list-line-trivia-token
                 key-line-prefix key-line-keys key-line-separator
                 key-line-case-insensitive key-line-indent key-line-after-heading
                 key-line-repeated key-line-node key-line-key-token
                 key-line-value-token key-line-trivia-token))
(export line-structure-parser-digest
        line-structure-rowan-syntax
        line-structure-rowan-source
        generate-line-structure-rowan-module)

(def (kind-index kinds name category)
  (let loop ((rest kinds) (index 0))
    (cond
     ((null? rest) (error "unknown line-structure syntax kind" name category))
     ((eq? (caar rest) name)
      (unless (eq? (cadar rest) category)
        (error "line-structure syntax kind has wrong category" name category))
      index)
     (else (loop (cdr rest) (+ index 1))))))

(def (kind-value kinds name category)
  (rust-number (kind-index kinds name category)))

(def (marker-value marker)
  (rust-number (char->integer (string-ref marker 0))))

(def (boolean-value value)
  (rust-identifier (if value "true" "false")))

(def (optional-value value project)
  (if value (rust-some (project value)) (rust-none)))

(def (heading-value kinds heading)
  (rust-struct HeadingLineRule
    (marker (marker-value (heading-line-marker heading)))
    (separator (marker-value (heading-line-separator heading)))
    (section_node (kind-value kinds (heading-line-section-node heading) 'node))
    (heading_node (kind-value kinds (heading-line-heading-node heading) 'node))
    (heading_token (kind-value kinds (heading-line-heading-token heading) 'token))
    (fields
     (optional-value
      (heading-line-fields heading)
      (lambda (fields)
        (rust-struct HeadingFieldsRule
          (title_token (kind-value kinds (heading-fields-title-token fields) 'token))
          (trivia_token (kind-value kinds (heading-fields-trivia-token fields) 'token))))))))

(def (body-line-value kinds body-line)
  (rust-struct KeyValueLineRule
    (marker (marker-value (key-value-line-marker body-line)))
    (node (kind-value kinds (key-value-line-node body-line) 'node))
    (key_token (kind-value kinds (key-value-line-key-token body-line) 'token))
    (value_token (kind-value kinds (key-value-line-value-token body-line) 'token))
    (trivia_token (kind-value kinds (key-value-line-trivia-token body-line) 'token))))

(def (block-header-value kinds header)
  (rust-struct BlockHeaderRule
    (argument_token (kind-value kinds (block-header-argument-token header) 'token))
    (trivia_token (kind-value kinds (block-header-trivia-token header) 'token))))

(def (block-recovery-value block)
  (rust-identifier
   (case (block-line-unclosed block)
     ((close-at-eof) "UnclosedBlockPolicy::CloseAtEof")
     ((recover-as-text) "UnclosedBlockPolicy::RecoverAsText")
     (else (error "unknown block recovery policy" block)))))

(def (block-contents-value block)
  (rust-identifier
   (case (block-line-contents block)
     ((opaque) "BlockContents::Opaque")
     ((elements) "BlockContents::Elements")
     (else (error "unknown block contents policy" block)))))

(def (block-value kinds block)
  (rust-struct BlockLineRule
    (opening (rust-string (block-line-opening block)))
    (opening_mode (rust-identifier
                   (case (block-line-opening-mode block)
                     ((literal) "BlockOpeningMode::Literal")
                     ((named-delimited) "BlockOpeningMode::NamedDelimited")
                     ((required-named-argument) "BlockOpeningMode::RequiredNamedArgument")
                     (else (error "unknown block opening mode" block)))))
    (closing (rust-string (block-line-closing block)))
    (case_insensitive (boolean-value (block-line-case-insensitive block)))
    (indent (boolean-value (block-line-indent block)))
    (block_node (kind-value kinds (block-line-block-node block) 'node))
    (begin_token (kind-value kinds (block-line-begin-token block) 'token))
    (body_token (kind-value kinds (block-line-body-token block) 'token))
    (end_token (kind-value kinds (block-line-end-token block) 'token))
    (unclosed (block-recovery-value block))
    (heading_bound (boolean-value (block-line-heading-bound block)))
    (contents (block-contents-value block))
    (body_line (optional-value (block-line-body-line block)
                               (lambda (value) (body-line-value kinds value))))
    (header (optional-value (block-line-header block)
                            (lambda (value) (block-header-value kinds value))))))

(def (inline-link-value kinds inline)
  (rust-struct InlineLinkRule
    (opening (rust-string (inline-link-opening inline)))
    (separator (rust-string (inline-link-separator inline)))
    (closing (rust-string (inline-link-closing inline)))
    (node (kind-value kinds (inline-link-node inline) 'node))
    (target_token (kind-value kinds (inline-link-target-token inline) 'token))
    (description_token (kind-value kinds (inline-link-description-token inline) 'token))
    (trivia_token (kind-value kinds (inline-link-trivia-token inline) 'token))))

(def (table-value kinds table)
  (rust-struct TableLineRule
    (delimiter (marker-value (table-line-delimiter table)))
    (table_node (kind-value kinds (table-line-table-node table) 'node))
    (row_node (kind-value kinds (table-line-row-node table) 'node))
    (rule_row_node (kind-value kinds (table-line-rule-row-node table) 'node))
    (cell_node (kind-value kinds (table-line-cell-node table) 'node))
    (separator_token (kind-value kinds (table-line-separator-token table) 'token))
    (cell_token (kind-value kinds (table-line-cell-token table) 'token))
    (trivia_token (kind-value kinds (table-line-trivia-token table) 'token))
    (rule_token (kind-value kinds (table-line-rule-token table) 'token))))

(def (list-value kinds list-rule)
  (rust-struct ListLineRule
    (unordered_markers (rust-string (list-line-unordered-markers list-rule)))
    (ordered (boolean-value (list-line-ordered list-rule)))
    (tab_width (rust-number (list-line-tab-width list-rule)))
    (list_node (kind-value kinds (list-line-list-node list-rule) 'node))
    (item_node (kind-value kinds (list-line-item-node list-rule) 'node))
    (bullet_token (kind-value kinds (list-line-bullet-token list-rule) 'token))
    (trivia_token (kind-value kinds (list-line-trivia-token list-rule) 'token))))

(def (key-line-value kinds rule)
  (rust-struct KeyLineRule
    (prefix (rust-string (key-line-prefix rule)))
    (keys (rust-array (map rust-string (key-line-keys rule))))
    (separator (marker-value (key-line-separator rule)))
    (case_insensitive (boolean-value (key-line-case-insensitive rule)))
    (indent (boolean-value (key-line-indent rule)))
    (context (rust-identifier
              (if (key-line-after-heading rule)
                "KeyLineContext::AfterHeading"
                "KeyLineContext::Anywhere")))
    (mode (rust-identifier
           (if (key-line-repeated rule)
             "KeyLineMode::Repeated"
             "KeyLineMode::Single")))
    (node (kind-value kinds (key-line-node rule) 'node))
    (key_token (kind-value kinds (key-line-key-token rule) 'token))
    (value_token (kind-value kinds (key-line-value-token rule) 'token))
    (trivia_token (kind-value kinds (key-line-trivia-token rule) 'token))))

(def (line-structure-digest grammar-digest heading blocks text table list-rule key-lines)
  (sha256-text
   (call-with-output-string
    (lambda (port)
      (write
       (list 'gerbil-parser-line-structure-v1 grammar-digest
             (list (heading-line-marker heading)
                   (heading-line-separator heading)
                   (heading-line-section-node heading)
                   (heading-line-heading-node heading)
                   (heading-line-heading-token heading)
                   (let (fields (heading-line-fields heading))
                     (and fields
                          (list (heading-fields-title-token fields)
                                (heading-fields-trivia-token fields)))))
             (map (lambda (block)
                    (list (block-line-opening block)
                          (block-line-opening-mode block)
                          (block-line-closing block)
                          (block-line-case-insensitive block)
                          (block-line-indent block)
                          (block-line-block-node block)
                          (block-line-begin-token block)
                          (block-line-body-token block)
                          (block-line-end-token block)
                          (block-line-unclosed block)
                          (block-line-heading-bound block)
                          (block-line-contents block)
                          (let (body-line (block-line-body-line block))
                            (and body-line
                                 (list (key-value-line-marker body-line)
                                       (key-value-line-node body-line)
                                       (key-value-line-key-token body-line)
                                       (key-value-line-value-token body-line)
                                       (key-value-line-trivia-token body-line))))
                          (let (header (block-line-header block))
                            (and header
                                 (list (block-header-argument-token header)
                                       (block-header-trivia-token header))))))
                  blocks)
             (list (text-line-node text) (text-line-token text)
                   (text-line-paragraph-node text)
                   (let (inline (text-line-inline-link text))
                     (and inline
                          (list (inline-link-opening inline)
                                (inline-link-separator inline)
                                (inline-link-closing inline)
                                (inline-link-node inline)
                                (inline-link-target-token inline)
                                (inline-link-description-token inline)
                                (inline-link-trivia-token inline)))))
             (and table
                  (list (table-line-delimiter table)
                        (table-line-table-node table)
                        (table-line-row-node table)
                        (table-line-rule-row-node table)
                        (table-line-cell-node table)
                        (table-line-separator-token table)
                        (table-line-cell-token table)
                        (table-line-trivia-token table)
                        (table-line-rule-token table)))
             (and list-rule
                  (list (list-line-unordered-markers list-rule)
                        (list-line-ordered list-rule)
                        (list-line-tab-width list-rule)
                        (list-line-list-node list-rule)
                        (list-line-item-node list-rule)
                        (list-line-bullet-token list-rule)
                        (list-line-trivia-token list-rule)))
             (map (lambda (rule)
                    (list (key-line-prefix rule)
                          (key-line-keys rule)
                          (key-line-separator rule)
                          (key-line-case-insensitive rule)
                          (key-line-indent rule)
                          (key-line-after-heading rule)
                          (key-line-repeated rule)
                          (key-line-node rule)
                          (key-line-key-token rule)
                          (key-line-value-token rule)
                          (key-line-trivia-token rule)))
                  key-lines))
       port)))))

(def (line-structure-parser-digest language-grammar structure)
  (unless (line-structure? structure)
    (error "line-structure digest requires a POO parser declaration" structure))
  (line-structure-digest
   (parser-machine-grammar-digest
    (language-grammar-machine language-grammar))
   (line-structure-heading structure)
   (line-structure-blocks structure)
   (line-structure-text structure)
   (line-structure-table structure)
   (line-structure-list structure)
   (line-structure-key-lines structure)))

;; Public input is one validated POO contract; output is a bounded Rust AST.
(def (line-structure-rowan-syntax language-grammar structure)
  (unless (line-structure? structure)
    (error "line-structure AOT requires a POO parser declaration" structure))
  (let* ((ir (language-grammar-ir language-grammar))
         (kinds (cdr (assq 'syntax-kinds ir)))
         (heading (line-structure-heading structure))
         (blocks (line-structure-blocks structure))
         (text (line-structure-text structure))
         (table (line-structure-table structure))
         (list-rule (line-structure-list structure))
         (key-lines (line-structure-key-lines structure))
         (digest (parser-machine-grammar-digest
                  (language-grammar-machine language-grammar)))
         (parser-digest (line-structure-parser-digest
                         language-grammar structure)))
    (rust-module
      '("BlockContents" "BlockHeaderRule" "BlockLineRule" "BlockOpeningMode" "HeadingFieldsRule"
        "HeadingLineRule" "InlineLinkRule" "KeyValueLineRule"
        "LineStructureSpec" "ListLineRule" "KeyLineContext" "KeyLineMode"
        "KeyLineRule" "TableLineRule" "UnclosedBlockPolicy")
      (rust-static STRUCTURE LineStructureSpec
        (rust-struct LineStructureSpec
          (grammar_digest (rust-string digest))
          (parser_digest (rust-string parser-digest))
          (heading (heading-value kinds heading))
          (blocks (rust-array (map (lambda (block) (block-value kinds block)) blocks)))
          (paragraph_node
           (optional-value (text-line-paragraph-node text)
                           (lambda (value) (kind-value kinds value 'node))))
          (table (optional-value table (lambda (value) (table-value kinds value))))
          (list (optional-value list-rule (lambda (value) (list-value kinds value))))
          (key_lines (rust-array (map (lambda (rule) (key-line-value kinds rule)) key-lines)))
          (text_node (kind-value kinds (text-line-node text) 'node))
          (text_token (kind-value kinds (text-line-token text) 'token))
          (inline_link
           (optional-value (text-line-inline-link text)
                           (lambda (value) (inline-link-value kinds value)))))))))

(def (line-structure-rowan-source language-grammar structure)
  (rust-render (line-structure-rowan-syntax language-grammar structure)))

(def (generate-line-structure-rowan-module output-path language-grammar structure)
  (let (source (line-structure-rowan-source language-grammar structure))
    (call-with-output-file output-path
      (lambda (port) (display source port)))))

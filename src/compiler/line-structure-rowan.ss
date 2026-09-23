;;; -*- Gerbil -*-
;;; POO line-structure declaration to immutable generic Rowan table.

(import (only-in ../language/descriptor
                 language-grammar-ir language-grammar-machine)
        (only-in ../runtime/identity sha256-text)
        (only-in ./machine parser-machine-grammar-digest)
        (only-in ../modules/parser/line-structure-objects
                 line-structure?
                 line-structure-heading line-structure-blocks line-structure-text
                 heading-line-marker heading-line-separator
                 heading-line-section-node heading-line-heading-node
                 heading-line-heading-token heading-line-fields
                 heading-fields-title-token heading-fields-trivia-token
                 block-line-opening block-line-closing block-line-case-insensitive
                 block-line-indent block-line-block-node block-line-begin-token
                 block-line-body-token block-line-end-token block-line-unclosed
                 block-line-heading-bound block-line-body-line
                 block-line-header block-header-argument-token
                 block-header-trivia-token
                 key-value-line-marker key-value-line-node
                 key-value-line-key-token key-value-line-value-token
                 key-value-line-trivia-token
                 text-line-node text-line-token text-line-inline-link
                 inline-link-opening inline-link-separator inline-link-closing
                 inline-link-node inline-link-target-token
                 inline-link-description-token inline-link-trivia-token))
(export line-structure-parser-digest
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

(def (emit-kind port kinds name category)
  (display (kind-index kinds name category) port))

(def (emit-block port kinds block)
  (display "    BlockLineRule { opening: " port)
  (write (block-line-opening block) port)
  (display ", closing: " port)
  (write (block-line-closing block) port)
  (display ", case_insensitive: " port)
  (display (if (block-line-case-insensitive block) "true" "false") port)
  (display ", indent: " port)
  (display (if (block-line-indent block) "true" "false") port)
  (display ", block_node: " port)
  (emit-kind port kinds (block-line-block-node block) 'node)
  (display ", begin_token: " port)
  (emit-kind port kinds (block-line-begin-token block) 'token)
  (display ", body_token: " port)
  (emit-kind port kinds (block-line-body-token block) 'token)
  (display ", end_token: " port)
  (emit-kind port kinds (block-line-end-token block) 'token)
  (display ", unclosed: " port)
  (display (case (block-line-unclosed block)
             ((close-at-eof) "UnclosedBlockPolicy::CloseAtEof")
             ((recover-as-text) "UnclosedBlockPolicy::RecoverAsText")
             (else (error "unknown block recovery policy" block))) port)
  (display ", heading_bound: " port)
  (display (if (block-line-heading-bound block) "true" "false") port)
  (display ", body_line: " port)
  (let (body-line (block-line-body-line block))
    (if body-line
      (begin
        (display "Some(KeyValueLineRule { marker: " port)
        (display (char->integer (string-ref (key-value-line-marker body-line) 0)) port)
        (display ", node: " port)
        (emit-kind port kinds (key-value-line-node body-line) 'node)
        (display ", key_token: " port)
        (emit-kind port kinds (key-value-line-key-token body-line) 'token)
        (display ", value_token: " port)
        (emit-kind port kinds (key-value-line-value-token body-line) 'token)
        (display ", trivia_token: " port)
        (emit-kind port kinds (key-value-line-trivia-token body-line) 'token)
        (display " })" port))
      (display "None" port)))
  (display ", header: " port)
  (let (header (block-line-header block))
    (if header
      (begin
        (display "Some(BlockHeaderRule { argument_token: " port)
        (emit-kind port kinds (block-header-argument-token header) 'token)
        (display ", trivia_token: " port)
        (emit-kind port kinds (block-header-trivia-token header) 'token)
        (display " })" port))
      (display "None" port)))
  (display " },\n" port))

(def (line-structure-digest grammar-digest heading blocks text)
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
                          (block-line-closing block)
                          (block-line-case-insensitive block)
                          (block-line-indent block)
                          (block-line-block-node block)
                          (block-line-begin-token block)
                          (block-line-body-token block)
                          (block-line-end-token block)
                          (block-line-unclosed block)
                          (block-line-heading-bound block)
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
                   (let (inline (text-line-inline-link text))
                     (and inline
                          (list (inline-link-opening inline)
                                (inline-link-separator inline)
                                (inline-link-closing inline)
                                (inline-link-node inline)
                                (inline-link-target-token inline)
                                (inline-link-description-token inline)
                                (inline-link-trivia-token inline))))))
       port)))))

(def (line-structure-parser-digest language-grammar structure)
  (unless (line-structure? structure)
    (error "line-structure digest requires a POO parser declaration" structure))
  (line-structure-digest
   (parser-machine-grammar-digest
    (language-grammar-machine language-grammar))
   (line-structure-heading structure)
   (line-structure-blocks structure)
   (line-structure-text structure)))

;; Public input is one validated POO contract. Canonical Parser IR is the
;; internal kind catalog; this projection never interprets author-facing rows.
(def (line-structure-rowan-source language-grammar structure)
  (unless (line-structure? structure)
    (error "line-structure AOT requires a POO parser declaration" structure))
  (let* ((ir (language-grammar-ir language-grammar))
         (kinds (cdr (assq 'syntax-kinds ir)))
         (heading (line-structure-heading structure))
         (blocks (line-structure-blocks structure))
         (text (line-structure-text structure))
         (digest (parser-machine-grammar-digest
                  (language-grammar-machine language-grammar)))
         (parser-digest (line-structure-parser-digest
                         language-grammar structure)))
    (call-with-output-string
     (lambda (port)
       (display "// @generated by gerbil-parser/src/compiler/line-structure-rowan.ss\n" port)
       (display "use gerbil_parser_rowan::{BlockHeaderRule, BlockLineRule, HeadingFieldsRule, HeadingLineRule, InlineLinkRule, KeyValueLineRule, LineStructureSpec, UnclosedBlockPolicy};\n\n" port)
       (display "pub static STRUCTURE: LineStructureSpec = LineStructureSpec {\n" port)
       (display "    grammar_digest: " port)
       (write digest port)
       (display ",\n    parser_digest: " port)
       (write parser-digest port)
       (display ",\n    heading: HeadingLineRule { marker: " port)
       (display (char->integer (string-ref (heading-line-marker heading) 0)) port)
       (display ", separator: " port)
       (display (char->integer (string-ref (heading-line-separator heading) 0)) port)
       (display ", section_node: " port)
       (emit-kind port kinds (heading-line-section-node heading) 'node)
       (display ", heading_node: " port)
       (emit-kind port kinds (heading-line-heading-node heading) 'node)
       (display ", heading_token: " port)
       (emit-kind port kinds (heading-line-heading-token heading) 'token)
       (display ", fields: " port)
       (let (fields (heading-line-fields heading))
         (if fields
           (begin
             (display "Some(HeadingFieldsRule { title_token: " port)
             (emit-kind port kinds (heading-fields-title-token fields) 'token)
             (display ", trivia_token: " port)
             (emit-kind port kinds (heading-fields-trivia-token fields) 'token)
             (display " })" port))
           (display "None" port)))
       (display " },\n    blocks: &[\n" port)
       (for-each (lambda (block) (emit-block port kinds block)) blocks)
       (display "    ],\n    text_node: " port)
       (emit-kind port kinds (text-line-node text) 'node)
       (display ", text_token: " port)
       (emit-kind port kinds (text-line-token text) 'token)
       (display ", inline_link: " port)
       (let (inline (text-line-inline-link text))
         (if inline
           (begin
             (display "Some(InlineLinkRule { opening: " port)
             (write (inline-link-opening inline) port)
             (display ", separator: " port)
             (write (inline-link-separator inline) port)
             (display ", closing: " port)
             (write (inline-link-closing inline) port)
             (display ", node: " port)
             (emit-kind port kinds (inline-link-node inline) 'node)
             (display ", target_token: " port)
             (emit-kind port kinds (inline-link-target-token inline) 'token)
             (display ", description_token: " port)
             (emit-kind port kinds (inline-link-description-token inline) 'token)
             (display ", trivia_token: " port)
             (emit-kind port kinds (inline-link-trivia-token inline) 'token)
             (display " })" port))
           (display "None" port)))
       (display ",\n};\n" port)))))

(def (generate-line-structure-rowan-module output-path language-grammar structure)
  (let (source (line-structure-rowan-source language-grammar structure))
    (call-with-output-file output-path
      (lambda (port) (display source port)))))

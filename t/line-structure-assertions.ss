;;; -*- Gerbil -*-
;;; Parser-specific AST assertions over the bounded Rust AOT product.

(import (only-in :std/test check)
        (only-in :gerbil-parser/src/compiler/rust-syntax
                 rust-module-form-item rust-static-form-value
                 rust-struct-form? rust-struct-form-name rust-struct-form-fields
                 rust-field-name rust-field-value
                 rust-array-form? rust-array-form-values
                 rust-some-form? rust-some-form-value
                 rust-string-form-value rust-identifier-form-value
                 rust-number-form-value))
(export check-structural-aot)

(def (aot-root syntax)
  (let (root (rust-static-form-value (rust-module-form-item syntax)))
    (unless (and (rust-struct-form? root)
                 (eq? (rust-struct-form-name root) 'LineStructureSpec))
      (error "expected structural parser AOT root" root))
    root))

(def (aot-field struct name)
  (let loop ((fields (rust-struct-form-fields struct)))
    (cond
     ((null? fields) (error "missing structural parser AOT field" name))
     ((eq? (rust-field-name (car fields)) name)
      (rust-field-value (car fields)))
     (else (loop (cdr fields))))))

(def (aot-required value name)
  (unless (rust-some-form? value)
    (error "missing optional structural parser AOT rule" name))
  (rust-some-form-value value))

(def (aot-first-rule root name)
  (let (array (aot-field root name))
    (unless (and (rust-array-form? array)
                 (pair? (rust-array-form-values array)))
      (error "missing structural parser AOT rule" name))
    (car (rust-array-form-values array))))

(def (aot-rule-name root name)
  (rust-struct-form-name (aot-required (aot-field root name) name)))

(def (aot-block-rule-name root name)
  (rust-struct-form-name
   (aot-required (aot-field (aot-first-rule root 'blocks) name) name)))

(def (aot-key-line-summaries root)
  (map (lambda (rule)
         (list (rust-string-form-value (aot-field rule 'prefix))
               (map rust-string-form-value
                    (rust-array-form-values (aot-field rule 'keys)))
               (rust-identifier-form-value (aot-field rule 'context))
               (rust-identifier-form-value (aot-field rule 'mode))))
       (rust-array-form-values (aot-field root 'key_lines))))

(defrules check-structural-aot
  (identity fields block-opening block-contents list-markers
   heading-fields body-line block-header inline-link paragraph-node key-lines)
  ((_ syntax (identity))
   (let* ((root (aot-root syntax))
          (grammar (rust-string-form-value (aot-field root 'grammar_digest)))
          (parser (rust-string-form-value (aot-field root 'parser_digest))))
     (check (substring grammar 0 7) => "sha256:")
     (check (substring parser 0 7) => "sha256:")))
  ((_ syntax (fields expected))
   (check (map rust-field-name (rust-struct-form-fields (aot-root syntax)))
          => expected))
  ((_ syntax (block-opening expected))
   (check (rust-string-form-value
           (aot-field (aot-first-rule (aot-root syntax) 'blocks) 'opening))
          => expected))
  ((_ syntax (block-contents expected))
   (check (rust-identifier-form-value
           (aot-field (aot-first-rule (aot-root syntax) 'blocks) 'contents))
          => expected))
  ((_ syntax (list-markers markers tab-width))
   (let (rule (aot-required (aot-field (aot-root syntax) 'list) 'list))
     (check (rust-struct-form-name rule) => 'ListLineRule)
     (check (rust-string-form-value (aot-field rule 'unordered_markers))
            => markers)
     (check (rust-number-form-value (aot-field rule 'tab_width))
            => tab-width)))
  ((_ syntax (heading-fields))
   (check (aot-rule-name (aot-field (aot-root syntax) 'heading) 'fields)
          => 'HeadingFieldsRule))
  ((_ syntax (body-line))
   (check (aot-block-rule-name (aot-root syntax) 'body_line)
          => 'KeyValueLineRule))
  ((_ syntax (block-header))
   (check (aot-block-rule-name (aot-root syntax) 'header)
          => 'BlockHeaderRule))
  ((_ syntax (inline-link))
   (check (aot-rule-name (aot-root syntax) 'inline_link)
          => 'InlineLinkRule))
  ((_ syntax (paragraph-node))
   (check (rust-some-form? (aot-field (aot-root syntax) 'paragraph_node))
          => #t))
  ((_ syntax (key-lines expected))
   (check (aot-key-line-summaries (aot-root syntax)) => expected))
  ((_ syntax clause more ...)
   (begin (check-structural-aot syntax clause)
          (check-structural-aot syntax more) ...)))

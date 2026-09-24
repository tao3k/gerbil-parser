;;; -*- Gerbil -*-
;;; Structural checks for generated Rust functions, without string snapshots.

(import (only-in :std/test check)
        (only-in :std/misc/ports read-all-as-string)
        (only-in :gerbil-parser/src/compiler/rust-syntax
                 rust-render
                 rust-function-form? rust-function-form-name
                 rust-function-form-parameters rust-function-form-result
                 rust-function-form-body rust-block-form?
                 rust-block-form-statements rust-block-form-result
                 rust-let-form? rust-let-form-name rust-let-form-value
                 rust-method-form? rust-method-form-method
                 rust-identifier-form? rust-identifier-form-value))
(export check-rust-aot-function check-rust-aot-artifact)

(defsyntax (check-rust-aot-function stx)
  (syntax-case stx ()
    ((_ function name parameters result local method)
     (syntax
      (let* ((value function)
             (body (rust-function-form-body value))
             (binding (car (rust-block-form-statements body)))
             (tail (rust-block-form-result body)))
        (check (rust-function-form? value) => #t)
        (check (rust-function-form-name value) => name)
        (check (rust-function-form-parameters value) => parameters)
        (check (rust-function-form-result value) => result)
        (check (rust-block-form? body) => #t)
        (check (length (rust-block-form-statements body)) => 1)
        (check (rust-let-form? binding) => #t)
        (check (rust-let-form-name binding) => local)
        (check (rust-method-form? (rust-let-form-value binding)) => #t)
        (check (rust-method-form-method (rust-let-form-value binding))
               => method)
        (check (rust-identifier-form? tail) => #t)
        (check (rust-identifier-form-value tail)
               => (symbol->string local)))))))

(defsyntax (check-rust-aot-artifact stx)
  (syntax-case stx ()
    ((_ function path)
     (syntax
      (check (call-with-input-file path read-all-as-string)
             => (rust-render function))))))

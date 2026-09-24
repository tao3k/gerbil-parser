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
                 rust-method-form? rust-method-form-method
                 rust-let-form? rust-let-form-value
                 rust-first-word-form? rust-string-in-form?
                 rust-any-form?
                 rust-if-form? rust-if-form-condition rust-if-form-alternate))
(export check-rust-aot-function check-rust-aot-conditional
        check-rust-aot-any check-rust-aot-artifact)

(defsyntax (check-rust-aot-any stx)
  (syntax-case stx ()
    ((_ function name parameters result)
     (syntax
      (let* ((value function)
             (body (rust-function-form-body value)))
        (check (rust-function-form? value) => #t)
        (check (rust-function-form-name value) => name)
        (check (rust-function-form-parameters value) => parameters)
        (check (rust-function-form-result value) => result)
        (check (rust-any-form? (rust-block-form-result body)) => #t))))))

(defsyntax (check-rust-aot-function stx)
  (syntax-case stx ()
    ((_ function name parameters result method)
     (syntax
      (let* ((value function)
             (body (rust-function-form-body value))
             (tail (rust-block-form-result body)))
        (check (rust-function-form? value) => #t)
        (check (rust-function-form-name value) => name)
        (check (rust-function-form-parameters value) => parameters)
        (check (rust-function-form-result value) => result)
        (check (rust-block-form? body) => #t)
        (check (rust-block-form-statements body) => '())
        (check (rust-method-form? tail) => #t)
        (check (rust-method-form-method tail) => method))))))

(defsyntax (check-rust-aot-artifact stx)
  (syntax-case stx ()
    ((_ function path)
     (syntax
      (check (call-with-input-file path read-all-as-string)
             => (rust-render function))))))

(defsyntax (check-rust-aot-conditional stx)
  (syntax-case stx ()
    ((_ function name parameters result)
     (syntax
      (let* ((value function)
             (body (rust-function-form-body value))
             (binding (car (rust-block-form-statements body)))
             (branch (rust-block-form-result body)))
        (check (rust-function-form? value) => #t)
        (check (rust-function-form-name value) => name)
        (check (rust-function-form-parameters value) => parameters)
        (check (rust-function-form-result value) => result)
        (check (length (rust-block-form-statements body)) => 1)
        (check (rust-let-form? binding) => #t)
        (check (rust-first-word-form? (rust-let-form-value binding)) => #t)
        (check (rust-if-form? branch) => #t)
        (check (rust-string-in-form? (rust-if-form-condition branch)) => #t)
        (check (rust-if-form? (rust-if-form-alternate branch)) => #t))))))

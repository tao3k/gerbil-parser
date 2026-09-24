;;; -*- Gerbil -*-
;;; Structural checks for generated Rust functions, without string snapshots.

(import (only-in :std/test check)
        (only-in :std/encoding/json JSONReadOptions string->json)
        (only-in :std/misc/ports read-all-as-string)
        (only-in :gerbil-parser/src/compiler/rust-syntax
                 rust-render rust-function-ir-json
                 rust-function-form? rust-function-form-name
                 rust-function-form-parameters rust-function-form-result
                 rust-function-form-body rust-block-form?
                 rust-block-form-statements rust-block-form-result
                 rust-method-form? rust-method-form-method
                 rust-let-form? rust-let-form-value
                 rust-first-word-form? rust-string-in-form?
        rust-any-form?
        rust-if-form? rust-if-form-condition rust-if-form-alternate
        rust-line-event-function-form?
        rust-line-event-function-form-name
        rust-line-event-function-form-root
        rust-line-event-function-form-body
        rust-event-if-form? rust-event-if-form-consequent
        rust-event-if-form-alternate rust-event-node-form?
        rust-event-node-form-children rust-event-token-form?))
(export check-rust-aot-function check-rust-aot-conditional
        check-rust-aot-any check-rust-aot-artifact
        check-rust-function-ir
        check-rust-event-strategy)

(def (json-ir-equivalent? left right)
  (cond
   ((and (hash-table? left) (hash-table? right))
    (let (keys (hash-keys left))
      (and (= (length keys) (length (hash-keys right)))
           (let loop ((rest keys))
             (or (null? rest)
                 (and (member (car rest) (hash-keys right))
                      (json-ir-equivalent?
                       (hash-get left (car rest))
                       (hash-get right (car rest)))
                      (loop (cdr rest))))))))
   ((and (vector? left) (vector? right))
    (and (= (vector-length left) (vector-length right))
         (let loop ((index 0))
           (or (= index (vector-length left))
               (and (json-ir-equivalent?
                     (vector-ref left index)
                     (vector-ref right index))
                    (loop (+ index 1)))))))
   ((and (list? left) (list? right))
    (and (= (length left) (length right))
         (let loop ((items left) (expected right))
           (or (null? items)
               (and (json-ir-equivalent? (car items) (car expected))
                    (loop (cdr items) (cdr expected)))))))
   (else (equal? left right))))

(defsyntax (check-rust-function-ir stx)
  (syntax-case stx ()
    ((_ function fixture)
     (syntax
      (let (options (JSONReadOptions object-as-hash: #t))
        (check
         (json-ir-equivalent?
          (string->json (rust-function-ir-json function) options)
          (call-with-input-file fixture
            (lambda (port)
              (string->json (read-all-as-string port) options))))
         => #t))))))

(defsyntax (check-rust-event-strategy stx)
  (syntax-case stx ()
    ((_ strategy name root)
     (syntax
      (let* ((value strategy)
             (body (rust-line-event-function-form-body value)))
        (check (rust-line-event-function-form? value) => #t)
        (check (rust-line-event-function-form-name value) => name)
        (check (rust-line-event-function-form-root value) => root)
        (check (rust-event-if-form? body) => #t)
        (check (rust-event-node-form?
                (rust-event-if-form-consequent body)) => #t)
        (check (rust-event-node-form?
                (rust-event-if-form-alternate body)) => #t)
        (check (rust-event-token-form?
                (car (rust-event-node-form-children
                      (rust-event-if-form-consequent body))))
               => #t))))))

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

;;; -*- Gerbil -*-
;;; Structural checks for generated Rust functions, without string snapshots.

(import (only-in :gerbil-parser/language-test-support parser-check parser-check-result)
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
      (parser-check ()(scope ((options (JSONReadOptions object-as-hash: #t)))
        (equal
         (json-ir-equivalent?
          (string->json (rust-function-ir-json function) options)
          (call-with-input-file fixture
            (lambda (port)
              (string->json (read-all-as-string port) options))))
          #t)))))))

(defsyntax (check-rust-event-strategy stx)
  (syntax-case stx ()
    ((_ strategy name root)
     (syntax
      (parser-check ()(sequential ((value strategy)
             (body (rust-line-event-function-form-body value)))
        (equal (rust-line-event-function-form? value)  #t)
        (equal (rust-line-event-function-form-name value)  name)
        (equal (rust-line-event-function-form-root value)  root)
        (equal (rust-event-if-form? body)  #t)
        (equal (rust-event-node-form?
                (rust-event-if-form-consequent body))  #t)
        (equal (rust-event-node-form?
                (rust-event-if-form-alternate body))  #t)
        (equal (rust-event-token-form?
                (car (rust-event-node-form-children
                      (rust-event-if-form-consequent body))))
                #t)))))))

(defsyntax (check-rust-aot-any stx)
  (syntax-case stx ()
    ((_ function name parameters result)
     (syntax
      (parser-check ()(sequential ((value function)
             (body (rust-function-form-body value)))
        (equal (rust-function-form? value)  #t)
        (equal (rust-function-form-name value)  name)
        (equal (rust-function-form-parameters value)  parameters)
        (equal (rust-function-form-result value)  result)
        (equal (rust-any-form? (rust-block-form-result body))  #t)))))))

(defsyntax (check-rust-aot-function stx)
  (syntax-case stx ()
    ((_ function name parameters result method)
     (syntax
      (parser-check ()(sequential ((value function)
             (body (rust-function-form-body value))
             (tail (rust-block-form-result body)))
        (equal (rust-function-form? value)  #t)
        (equal (rust-function-form-name value)  name)
        (equal (rust-function-form-parameters value)  parameters)
        (equal (rust-function-form-result value)  result)
        (equal (rust-block-form? body)  #t)
        (equal (rust-block-form-statements body)  '())
        (equal (rust-method-form? tail)  #t)
        (equal (rust-method-form-method tail)  method)))))))

(defsyntax (check-rust-aot-artifact stx)
  (syntax-case stx ()
    ((_ function path)
     (syntax
      (parser-check ()(equal (call-with-input-file path read-all-as-string)
              (rust-render function)))))))

(defsyntax (check-rust-aot-conditional stx)
  (syntax-case stx ()
    ((_ function name parameters result)
     (syntax
      (parser-check ()(sequential ((value function)
             (body (rust-function-form-body value))
             (binding (car (rust-block-form-statements body)))
             (branch (rust-block-form-result body)))
        (equal (rust-function-form? value)  #t)
        (equal (rust-function-form-name value)  name)
        (equal (rust-function-form-parameters value)  parameters)
        (equal (rust-function-form-result value)  result)
        (equal (length (rust-block-form-statements body))  1)
        (equal (rust-let-form? binding)  #t)
        (equal (rust-first-word-form? (rust-let-form-value binding))  #t)
        (equal (rust-if-form? branch)  #t)
        (equal (rust-string-in-form? (rust-if-form-condition branch))  #t)
        (equal (rust-if-form? (rust-if-form-alternate branch))  #t)))))))

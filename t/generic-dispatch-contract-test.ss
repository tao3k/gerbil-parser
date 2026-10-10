;;; Core CLOS preparation and contextual conflict contracts.
;;; These executable design controls do not install a parser backend.
(import :std/test
        (only-in :core/poo-clos/interface .defgeneric .defmethod)
        (only-in :gerbil-parser/src/compiler/contextual-dispatch
                 compile-contextual-dispatch contextual-dispatch-ref)
        (only-in :gerbil-parser/src/modules/parser/contextual-objects
                 make-contextual-role make-contextual-method))
(export generic-dispatch-contract-test)

(def selections 0)
(.defgeneric test-select-semantic-plan (backend capability))
(.defmethod (test-select-semantic-plan backend capability)
  (error "unsupported test semantic plan" backend capability))
(.defmethod (test-select-semantic-plan (backend (eql 'recognition)) capability)
  (set! selections (+ selections 1))
  (lambda (value) (list 'recognition value)))
(.defmethod (test-select-semantic-plan (backend (eql 'event))
                                     (capability (eql 'plain)))
  (set! selections (+ selections 1))
  (lambda (value) (list 'event value)))

(.defgeneric test-context-selection (mode position)
  (argument-precedence-order mode position))
(.defmethod (test-context-selection (mode (eql 'shell)) position) 'mode-choice)
(.defmethod (test-context-selection mode (position (eql 'command))) 'position-choice)

(def (contextual-intersection first-result second-result)
  (compile-contextual-dispatch
   (list (make-contextual-role 'example
           (list (make-contextual-method 'by-mode 'shell 'any 'any first-result)
                 (make-contextual-method 'by-position 'any 'command 'any second-result))))
   '(shell) '(command) '(word)))

(def generic-dispatch-contract-test
  (test-suite "generic preparation and contextual contracts"
    (test-case "normal values select a fixed kernel once at preparation"
      (set! selections 0)
      (let (kernel (test-select-semantic-plan 'event 'plain))
        (for-each (lambda (value) (check (kernel value) => (list 'event value)))
                  '(a b c d))
        (check selections => 1))
      (check ((test-select-semantic-plan 'recognition 'layout) 'input)
             => '(recognition input))
      (check-exception (test-select-semantic-plan 'event 'layout)
                       (lambda (condition)
                         (equal? (error-message condition)
                                 "unsupported test semantic plan"))))
    (test-case "argument precedence cannot silently replace contextual conflict admission"
      (check (test-context-selection 'shell 'command) => 'mode-choice)
      (check-exception (contextual-intersection 'mode-choice 'position-choice)
                       (lambda (condition)
                         (equal? (error-message condition) "ambiguous contextual dispatch"))))
    (test-case "equal contextual results retain both declaration origins"
      (let (ir (contextual-intersection 'same-result 'same-result))
        (check (contextual-dispatch-ref ir 'cells)
               => '((shell command word
                           (same-result ((example by-mode) (example by-position))))))))))

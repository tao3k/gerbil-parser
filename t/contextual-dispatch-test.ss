;;; -*- Gerbil -*-
;;; Compiler decisions for independently varying lexical and syntax axes.

(import (only-in :std/test check test-case test-suite)
        (only-in :gerbil-parser/src/modules/parser/contextual-objects
                 make-contextual-method make-contextual-role)
        (only-in :gerbil-parser/src/compiler/contextual-dispatch
                 compile-contextual-dispatch contextual-dispatch-ref))
(export contextual-dispatch-test)

(def (method name mode position form result)
  (make-contextual-method name mode position form result))

(def (cell ir mode position form)
  (find (lambda (row)
          (equal? (take row 3) (list mode position form)))
        (contextual-dispatch-ref ir 'cells)))

(def contextual-dispatch-test
  (test-suite "contextual compiler dispatch"
    (test-case "Bash mode and syntax position resolve together"
      (let* ((base
              (make-contextual-role
               'word-base
               (list (method 'bare 'any 'any 'bare 'ordinary-word)
                     (method 'quoted 'any 'any 'quoted 'quoted-word))))
             (bash
              (make-contextual-role
               'bash-command
               (list (method 'reserved 'command 'command-start
                             'bare 'reserved-word))))
             (ir (compile-contextual-dispatch
                  (list base bash)
                  '(command word) '(command-start argument) '(bare quoted))))
        (check (list-ref (cell ir 'command 'command-start 'bare) 3)
               => '(reserved-word ((bash-command reserved))))
        (check (list-ref (cell ir 'command 'argument 'bare) 3)
               => '(ordinary-word ((word-base bare))))
        (check (list-ref (cell ir 'word 'argument 'quoted) 3)
               => '(quoted-word ((word-base quoted))))))
    (test-case "the same compiler resolves an HCL template context"
      (let* ((role
              (make-contextual-role
               'hcl-template
               (list (method 'literal 'any 'any 'text 'template-text)
                     (method 'interpolation 'template 'expression
                             'dollar-open 'interpolation-open))))
             (ir (compile-contextual-dispatch
                  (list role)
                  '(template expression) '(text expression)
                  '(text dollar-open))))
        (check (list-ref (cell ir 'template 'expression 'dollar-open) 3)
               => '(interpolation-open ((hcl-template interpolation))))
        (check (list-ref (cell ir 'template 'text 'text) 3)
               => '(template-text ((hcl-template literal))))))
    (test-case "incomparable methods with different results fail closed"
      (let ((by-mode
             (make-contextual-role
              'by-mode (list (method 'm 'command 'any 'bare 'word))))
            (by-position
             (make-contextual-role
              'by-position
              (list (method 'p 'any 'command-start 'bare 'keyword)))))
        (check
         (with-catch
          (lambda (condition) (error-message condition))
          (lambda ()
            (compile-contextual-dispatch
             (list by-mode by-position)
             '(command) '(command-start) '(bare))
            #f))
         => "ambiguous contextual dispatch")))
    (test-case "a same-result tie retains both method origins"
      (let* ((by-mode
              (make-contextual-role
               'by-mode (list (method 'm 'command 'any 'bare 'word))))
             (by-position
              (make-contextual-role
               'by-position
               (list (method 'p 'any 'command-start 'bare 'word))))
             (ir (compile-contextual-dispatch
                  (list by-mode by-position)
                  '(command) '(command-start) '(bare))))
        (check (list-ref (cell ir 'command 'command-start 'bare) 3)
               => '(word ((by-mode m) (by-position p))))))
    (test-case "method result and role identity bind the digest"
      (let* ((a (make-contextual-role
                 'one (list (method 'm 'any 'any 'bare 'word))))
             (b (make-contextual-role
                 'one (list (method 'm 'any 'any 'bare 'keyword))))
             (c (make-contextual-role
                 'two (list (method 'm 'any 'any 'bare 'word))))
             (compile
              (lambda (role)
                (compile-contextual-dispatch
                 (list role) '(command) '(command-start) '(bare)))))
        (check (string=?
                (contextual-dispatch-ref (compile a) 'digest)
                (contextual-dispatch-ref (compile a) 'digest))
               => #t)
        (check (string=?
                (contextual-dispatch-ref (compile a) 'digest)
                (contextual-dispatch-ref (compile b) 'digest))
               => #f)
        (check (string=?
                (contextual-dispatch-ref (compile a) 'digest)
                (contextual-dispatch-ref (compile c) 'digest))
               => #f)))))

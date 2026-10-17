#!/usr/bin/env gxi
;;; -*- Gerbil -*-
;;; Composition-only controls do not load parser generation/runtime modules.
(import "list-roles"
        (only-in "../../../src/modules/parser/objects" make-grammar)
        (only-in "../../../src/compiler/normalize" compile-grammar))
(def checked 0)
(def (list-check label actual expected)
  (unless (equal? actual expected) (error "list collision mismatch" label actual expected))
  (set! checked (+ checked 1))
  (display "LIST-COLLISION-CASE-OK: ") (display label) (newline) (force-output))
;;; Distinguish entry collisions from generated-helper collisions.
(def (collision-receipt first-owner first-entry second-owner second-entry)
  (with-catch (lambda (condition)
                (list (error-message condition) (error-irritants condition)))
    (lambda ()
      (compile-grammar
       (make-grammar 'collision '() '()
         (list
          (cons 'append (make-nonempty-list-role first-owner first-entry
                          '(reference name) '(literal ",") 'argument))
          (cons 'append (make-nonempty-list-role second-owner second-entry
                          '(reference name) '(literal ",") 'element))))))))
(list-check 'public-entry-collision
  (collision-receipt 'first 'shared 'second 'shared)
  '("grammar append target already exists" (rules shared)))
(list-check 'generated-helper-collision
  (collision-receipt 'shared-owner 'arguments 'shared-owner 'elements)
  '("grammar append target already exists" (rules shared-owner/tail)))

(display "LIST-COLLISIONS-OK: ") (display checked)
(display " checks passed") (newline) (force-output)
(exit 0)

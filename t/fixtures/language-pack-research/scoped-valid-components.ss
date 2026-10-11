#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import "located-component" "scoped-author-vocabulary"
        (only-in "../../../src/modules/parser/objects"
                 grammar-role? grammar-role-name grammar-role-ref))
(def component-value 'host-component-value)
(scoped-arguments scoped-component call-arguments arguments
  (reference name) (literal ",") argument)
(deflocated-list direct-component call-arguments arguments
  (reference name) (literal ",") (field argument))
(def checked 0)
(def (probe label actual expected)
  (unless (equal? actual expected) (error "scoped valid component mismatch" label actual expected))
  (set! checked (+ checked 1))
  (display "SCOPED-VALID-CASE-OK: ") (display label) (newline) (force-output))
(def scoped-role (cdr (assq 'role scoped-component)))
(def direct-role (cdr (assq 'role direct-component)))
(probe 'hygienic-local-binding component-value 'host-component-value)
(probe 'checked-role (grammar-role? scoped-role) #t)
(probe 'same-owner (grammar-role-name scoped-role) (grammar-role-name direct-role))
(for-each
 (lambda (section)
   (probe section (grammar-role-ref scoped-role section) (grammar-role-ref direct-role section)))
 '(syntax-kinds terminals lexical-rules rules extras keywords parser-entrypoints recoveries flow))
(display "SCOPED-VALID-COMPONENTS-OK: ") (display checked)
(display " checks passed") (newline) (force-output)
(exit 0)

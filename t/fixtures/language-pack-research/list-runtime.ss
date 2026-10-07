#!/usr/bin/env gxi
;;; -*- Gerbil -*-
(import (only-in :gerbil-parser/t/fixtures/tla-sany-differential/exit-child-process test-child-process-exit!)
        "list-parser" "list-roles"
        (only-in "../../../src/modules/parser/objects" make-grammar)
        (only-in "../../../src/compiler/normalize" compile-grammar grammar-ir-ref)
        (only-in "../../../src/runtime/parser" parse-source)
        (only-in "../../../src/compiler/parser-ir" parser-ir-ref)
        (only-in "../../../src/compiler/bound-ir" bound-grammar-ir-binding)
        (only-in "../../../src/runtime/artifact"
                 parse-artifact-success? parse-artifact-events parse-artifact-roundtrip))
(def checked 0)
(def (list-check label actual expected)
  (unless (equal? actual expected) (error "list component mismatch" label actual expected))
  (set! checked (+ checked 1))
  (display "LIST-CASE-OK: ") (display label) (newline) (force-output))
(def (ref value key) (cdr (assq key value)))
(list-check 'composition-operations
  (map (lambda (step) (ref step 'operation)) (ref list-composition-receipt 'steps))
  '(merge append append))
(list-check 'composition-owners
  (map (lambda (step) (ref step 'role)) (ref list-composition-receipt 'steps))
  '(list-study-base-role call-arguments array-elements))
(list-check 'deterministic-composition
  (compile-grammar (make-list-study-grammar)) list-composed-ir)
(list-check 'helper-rule-identities
  (map car (filter (lambda (row) (memq (car row) '(call-arguments/tail array-elements/tail)))
                   (grammar-ir-ref list-composed-ir 'rules)))
  '(call-arguments/tail array-elements/tail))
(for-each
 (lambda (section)
   (list-check (list 'reified-section section)
     (grammar-ir-ref list-study-grammar section) (grammar-ir-ref list-composed-ir section)))
 '(syntax-kinds terminals lexical-rules rules extras keywords parser-entrypoints recoveries flow))
(list-check 'duplicate-append-diagnostic
  (with-catch (lambda (condition) (error-message condition))
    (lambda ()
      (compile-grammar
       (make-grammar 'duplicate '() '()
         (let (role (make-nonempty-list-role 'call-arguments 'arguments
                       '(reference name) '(literal ",") 'argument))
           (list (cons 'append role) (cons 'append role)))))))
  "grammar append target already exists")
(for-each
 (lambda (case)
   (let* ((source (car case)) (field-name (cadr case)) (count (caddr case))
          (candidate (parse-list-composed source))
          (control (parse-source list-control-parser source)))
     (list-check (list 'accepted source) (parse-artifact-success? candidate) #t)
     (list-check (list 'control-accepted source) (parse-artifact-success? control) #t)
     (list-check (list 'roundtrip source) (parse-artifact-roundtrip candidate) source)
     (list-check (list 'complete-event-parity source)
       (parse-artifact-events candidate) (parse-artifact-events control))
     (list-check (list 'per-item-fields source)
       (length (filter (lambda (event)
                         (and (eq? (vector-ref event 0) 'start-field)
                              (eq? (vector-ref event 1) field-name)))
                       (parse-artifact-events candidate))) count)))
 '(("f()" argument 0) ("f(a)" argument 1) ("f(a,b,c)" argument 3)
   ("f( a , b )" argument 2) ("[]" element 0) ("[a]" element 1)
   ("[a,b,c]" element 3) ("[ a ,\n b ]" element 2)))
(for-each
 (lambda (source)
   (list-check (list 'rejected source)
     (parse-artifact-success? (parse-list-composed source)) #f))
 '("f(,)" "f(a,)" "f(a b)" "f(a" "[,]" "[a,]" "[a b]" "[a"))
;;; Both policy variants consume the exact canonical product via checked entry.
(list-check 'published-composed-grammar-is-complete-canonical-input list-study-grammar list-composed-ir)
(list-check 'published-parser-retains-composition
  (parser-ir-ref list-study-parser-ir 'compositionDigest) (ref list-composition-receipt 'compositionDigest))
(list-check 'single-policy-canonical-input list-single-grammar list-single-composed-ir)
(list-check 'single-policy-parser-composition
  (parser-ir-ref list-single-parser-ir 'compositionDigest) (ref list-single-composition-receipt 'compositionDigest))
(list-check 'single-policy-selected-bound-owner
  (ref (ref (bound-grammar-ir-binding list-single-bound-grammar-ir 'rule 'arguments) 'source) 'componentOwner)
  'single-argument-extension)
(list-check 'single-policy-removed-helper-binding
  (bound-grammar-ir-binding list-single-bound-grammar-ir 'rule 'call-arguments/tail) #f)
(for-each
 (lambda (source)
   (let ((candidate (parse-list-single source))
         (control (parse-list-composed source)))
     (list-check (list 'single-policy-accepted source) (parse-artifact-success? candidate) #t)
     (list-check (list 'single-policy-event-parity source)
       (parse-artifact-events candidate) (parse-artifact-events control))
     (list-check (list 'single-policy-roundtrip source) (parse-artifact-roundtrip candidate) source)))
 '("f()" "f(a)" "[a,b,c]"))
(list-check 'single-policy-rejects-second-argument
  (parse-artifact-success? (parse-list-single "f(a,b)")) #f)
(list-check 'ordinary-policy-keeps-multiple-arguments
  (parse-artifact-success? (parse-list-composed "f(a,b)")) #t)

;;; Native upstream slot inheritance must preserve the accepted language policy.
(for-each
 (lambda (source)
   (for-each
    (lambda (entries)
      (let ((candidate ((car entries) source)) (control ((cadr entries) source)))
        (list-check (list 'native-status source)
          (parse-artifact-success? candidate) (parse-artifact-success? control))
        (list-check (list 'native-complete-events source)
          (parse-artifact-events candidate) (parse-artifact-events control))
        (list-check (list 'native-roundtrip source) (parse-artifact-roundtrip candidate) source)))
    (list (list parse-native-list parse-list-composed)
          (list parse-native-single parse-list-single))))
 '("f()" "f(a)" "f(a,b,c)" "[a,b,c]" "f(a,)" "[a b]"))
(list-check 'native-append-helper-declaration-owner
  (ref (ref (bound-grammar-ir-binding native-list-bound-grammar-ir 'rule 'arguments) 'source)
       'declarationOwner) 'call-arguments)
(list-check 'native-inherited-base-not-generated
  (ref (ref (bound-grammar-ir-binding native-list-bound-grammar-ir 'rule 'source-file) 'source)
       'generated?) #f)
(list-check 'native-override-declaration-owner
  (ref (ref (bound-grammar-ir-binding native-single-bound-grammar-ir 'rule 'arguments) 'source)
       'declarationOwner) 'single-argument-extension)
(list-check 'native-removed-helper-absent
  (bound-grammar-ir-binding native-single-bound-grammar-ir 'rule 'call-arguments/tail) #f)

(display "LIST-RUNTIME-OK: ") (display checked)
(display " checks passed") (newline) (force-output)
(test-child-process-exit! 0)

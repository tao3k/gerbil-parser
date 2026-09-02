;;; -*- Gerbil -*-
;;; Compile-time inclusion of versioned native-syntax fixtures.

(import (for-syntax :std/misc/ports)
        ../runtime/identity)
(export defsyntax-fixture
        syntax-fixture?
        syntax-fixture-id
        syntax-fixture-language
        syntax-fixture-version
        syntax-fixture-contract
        syntax-fixture-source-digest
        syntax-fixture-source
        syntax-fixture-expected-status
        syntax-fixture-root-kind
        syntax-fixture-required-kinds)

(defstruct syntax-fixture
  (id language version contract source-digest source
      expected-status root-kind required-kinds)
  transparent: #t)

;; The source file is resolved relative to the declaration and embedded while
;; the module is expanded. Runtime parsing therefore has no filesystem or
;; working-directory dependency. The declared path is deliberately absent from
;; the fixture value and from every content identity.
(defsyntax (defsyntax-fixture stx)
  (syntax-case stx (identity source expect)
    ((_ binding
        (identity fixture-id language version contract)
        (source path)
        (expect expected-status root-kind (required-kind ...)))
     (and (identifier? #'binding)
          (stx-string? #'fixture-id)
          (stx-string? #'language)
          (stx-string? #'version)
          (stx-string? #'path)
          (memq (stx-e #'expected-status) '(accepted rejected))
          (if (eq? (stx-e #'expected-status) 'accepted)
            (identifier? #'root-kind)
            (eq? (stx-e #'root-kind) #f)))
     (let* ((resolved (gx#core-resolve-path #'path (stx-source stx)))
            (content (call-with-input-file resolved read-all-as-string)))
       (with-syntax ((fixture-content content))
         #'(def binding
             (make-syntax-fixture
              fixture-id
              language
              version
              contract
              (sha256-text fixture-content)
              fixture-content
              'expected-status
              'root-kind
              '(required-kind ...))))))
    (_ (raise-syntax-error #f "invalid native-syntax fixture declaration" stx))))

;;; -*- Gerbil -*-
;;; Compile-time inclusion of versioned native-syntax fixtures.

(import (for-syntax :std/misc/ports)
        :gerbil-parser/src/runtime/identity)
(export defsyntax-fixture
        defsyntax-corpus
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

;; A corpus declaration is an expansion-time manifest. Every path remains
;; explicit and reviewable, while the resulting runtime value contains only
;; immutable, content-addressed fixtures.
(defsyntax (defsyntax-corpus stx)
  (syntax-case stx (identity accepted rejected)
    ((_ binding
        (identity language-value version-value contract-value)
        (accepted
         (fixture-id accepted-binding path root-kind
                     (required-kind ...)) ...)
        (rejected
         (rejected-id rejected-binding rejected-path) ...))
     #'(begin
         (defsyntax-fixture accepted-binding
           (identity fixture-id language-value version-value contract-value)
           (source path)
           (expect accepted root-kind (required-kind ...))) ...
         (defsyntax-fixture rejected-binding
           (identity rejected-id language-value version-value contract-value)
           (source rejected-path)
           (expect rejected #f ())) ...
         (def binding
           (list accepted-binding ... rejected-binding ...))))
    (_ (raise-syntax-error #f "invalid native-syntax corpus declaration" stx))))

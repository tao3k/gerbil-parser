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

;; syntax-fixture
;;   : (forall (k) (-> String String String String String String Symbol k (List Symbol) SyntaxFixture))
;;   : (-> FixtureId Language Version Contract Digest Source Status RootKind (List Symbol) SyntaxFixture)
;;   | type SyntaxFixture = immutable native-syntax fixture evidence
;;   | doc m%
;;       Stores source identity, expected admission state, and CST obligations.
;;     %
;; : (-> FixtureId LanguageId VersionId ContractId Digest SourceText Status RootKind (List Symbol) SyntaxFixture)
(defstruct syntax-fixture
  (id language version contract source-digest source
      expected-status root-kind required-kinds)
  transparent: #t)

;;; Expansion resolves the source relative to its declaration. Runtime parsing
;;; therefore has no filesystem dependency, and content identity excludes paths.
;; defsyntax-fixture
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       Embeds one digest-bound syntax fixture at expansion time.
;;
;;       # Examples
;;
;;       ```scheme
;;       (defsyntax-fixture sample
;;         (identity "sample" "lang" "v1" "contract.v1")
;;         (source "sample.txt")
;;         (expect accepted Root (Child)))
;;       ;; => immutable syntax-fixture binding
;;       ```
;;       Result: the binding contains source bytes and their SHA-256 identity.
;;     %
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

;;; A corpus is an expansion-time manifest: paths stay reviewable in source,
;;; while the runtime list contains only immutable content-addressed fixtures.
;; defsyntax-corpus
;;   : (-> Syntax Syntax)
;;   | doc m%
;;       Expands accepted and rejected fixture rows into one ordered corpus.
;;
;;       # Examples
;;
;;       ```scheme
;;       (defsyntax-corpus samples
;;         (identity "lang" "v1" "contract.v1")
;;         (accepted ("ok" ok "ok.txt" Root ()))
;;         (rejected ("bad" bad "bad.txt")))
;;       ;; => ordered list of immutable syntax-fixture values
;;       ```
;;       Result: accepted rows precede rejected rows exactly as declared.
;;     %
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

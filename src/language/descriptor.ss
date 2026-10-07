;;; -*- Gerbil -*-
;;; Runtime-small immutable identity and generated-artifact descriptor.

(export +language-grammar-schema+
        language-grammar?
        make-language-grammar language-grammar-with-identity
        language-grammar-language
        language-grammar-version
        language-grammar-contract
        language-grammar-grammar
        language-grammar-ir
        language-grammar-machine
        language-grammar-observability
        language-grammar-with-observability
        language-grammar-with-parser-policy language-grammar-parser-policy
        language-parser-policy-identity language-parser-policy-branch-budget
        language-parser-policy-cst-validator require-portable-language-policy!)

(def +language-grammar-schema+ "gerbil-parser.language-grammar.v1")

(defstruct language-grammar
  (schema language version contract grammar ir machine observability)
  transparent: #t)

(defstruct language-parser-policy (identity branch-budget cst-validator)
  transparent: #t)

;;; Preserve the ordinary descriptor layout and its eight-argument constructor.
;;; A policy is admitted once and cannot be supplied as an unchecked loader slot.
(defstruct (policy-language-grammar language-grammar) (policy) transparent: #t)

(def (language-grammar-parser-policy grammar)
  (and (policy-language-grammar? grammar) (policy-language-grammar-policy grammar)))

(def (copy-language-grammar grammar observability policy)
  (let (fields (list (language-grammar-schema grammar)
                    (language-grammar-language grammar)
                    (language-grammar-version grammar)
                    (language-grammar-contract grammar)
                    (language-grammar-grammar grammar)
                    (language-grammar-ir grammar)
                    (language-grammar-machine grammar) observability))
    (if policy
      (apply make-policy-language-grammar (append fields (list policy)))
      (apply make-language-grammar fields))))

(def (language-grammar-with-parser-policy grammar identity branch-budget cst-validator)
  (unless (and (language-grammar? grammar)
               (string? identity) (positive? (string-length identity))
               (fixnum? branch-budget) (positive? branch-budget)
               (procedure? cst-validator))
    (error "invalid language parser policy" identity branch-budget))
  (copy-language-grammar
   grammar (language-grammar-observability grammar)
   (make-language-parser-policy identity branch-budget cst-validator)))

;;; Scheme CST callbacks have no portable lowering yet. Reject before emission.
(def (require-portable-language-policy! grammar)
  (let (policy (language-grammar-parser-policy grammar))
    (when policy
      (error "language parser policy is unsupported by standalone Rust AOT"
             (language-parser-policy-identity policy))))
  grammar)

;;; Observability changes preserve the recognition and validation policy.
(def (language-grammar-with-observability grammar observability)
  (copy-language-grammar grammar observability (language-grammar-parser-policy grammar)))

;;; Bind release identity after syntax compilation; preserve machine and policy.
(def (language-grammar-with-identity definition language version contract)
  (let ((policy (language-grammar-parser-policy definition))
        (fields (list (language-grammar-schema definition)
                      (string-copy language) (string-copy version) (string-copy contract)
                      (language-grammar-grammar definition) (language-grammar-ir definition)
                      (language-grammar-machine definition) (language-grammar-observability definition))))
    (if policy (apply make-policy-language-grammar (append fields (list policy)))
        (apply make-language-grammar fields))))

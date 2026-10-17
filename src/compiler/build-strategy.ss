;;; -*- Gerbil -*-
;;; Common POO strategy binding/admission; providers belong to engine extensions.
(import (only-in :clan/poo/object .o .cc .ref .slot? object?)
        (only-in :clan/poo/mop define-type validate)
        (only-in :core/types PooFlowContract. poo-flow-classification-evidence)
        (only-in ../language/descriptor language-grammar? language-grammar-ir language-grammar-machine)
        (only-in ./machine parser-machine-grammar-digest parser-machine-ir))
(export BuildStrategy. BuildStrategyContract make-bound-build-strategy emit-build-strategy
        build-strategy-common-shape? declare-build-strategy-provider)

;;; Opaque engine registration keeps a recipe override from substituting an
;;; unchecked writer. A provider owns one output format, admission and emitter.
(defstruct build-strategy-provider (name format admit emit))
(def (declare-build-strategy-provider name format admit emit)
  (unless (and (symbol? name) (symbol? format) (procedure? admit) (procedure? emit))
    (error "invalid engine build strategy provider" name format))
  (make-build-strategy-provider name format admit emit))

(def (build-strategy-common-shape? candidate)
  (and (object? candidate)
       (andmap (lambda (slot) (.slot? candidate slot)) '(descriptor digest metadata provider kind format))
       (language-grammar? (.ref candidate 'descriptor))
       (build-strategy-provider? (.ref candidate 'provider))
       (eq? (.ref candidate 'kind) (build-strategy-provider-name (.ref candidate 'provider)))
       (eq? (.ref candidate 'format) (build-strategy-provider-format (.ref candidate 'provider)))
       (equal? (.ref candidate 'digest)
               (parser-machine-grammar-digest (language-grammar-machine (.ref candidate 'descriptor))))
       (eq? (language-grammar-ir (.ref candidate 'descriptor))
            (parser-machine-ir (language-grammar-machine (.ref candidate 'descriptor))))
       (object? (.ref candidate 'metadata))))

(def (build-strategy-shape? candidate)
  (with-catch (lambda (_) #f)
    (lambda ()
      (and (build-strategy-common-shape? candidate)
           ((build-strategy-provider-admit (.ref candidate 'provider)) candidate)))))

(define-type (BuildStrategyContract @ PooFlowContract.)
  identity: 'gerbil-parser/build-strategy
  .classify: (lambda (candidate context)
               (let (accepted? (build-strategy-shape? candidate))
                 (poo-flow-classification-evidence
                  'gerbil-parser/build-strategy candidate accepted?
                  (if accepted? '() '((expected gerbil-parser/build-strategy))) context))))

(def BuildStrategy.
  (.o (:: self)
      descriptor: #f provider: #f
      (digest (parser-machine-grammar-digest (language-grammar-machine (.ref self 'descriptor))))
      metadata: (.o)
      (kind (build-strategy-provider-name (.ref self 'provider)))
      (format (build-strategy-provider-format (.ref self 'provider)))
      (.emit (lambda (port) (emit-build-strategy self port)))))

(def (make-bound-build-strategy descriptor prototype)
  (let (candidate (.cc prototype 'descriptor descriptor))
    (validate BuildStrategyContract candidate)
    candidate))

;;; The common dispatcher has no strategy-kind or language-name branches.
;;; Writers must materialize their output before touching the caller's port.
(def (emit-build-strategy strategy port)
  (validate BuildStrategyContract strategy)
  ((build-strategy-provider-emit (.ref strategy 'provider)) strategy port))

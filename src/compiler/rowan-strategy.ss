;;; -*- Gerbil -*-
;;; Existing canonical Rust/Rowan producer through the common POO build protocol.
(import (only-in :clan/poo/object .o .ref)
        (only-in :clan/poo/mop define-type validate)
        (only-in :core/types PooFlowContract. poo-flow-classification-evidence)
        (only-in ./build-strategy BuildStrategy. build-strategy-common-shape?
                 declare-build-strategy-provider make-bound-build-strategy)
        (only-in ../language/descriptor language-grammar-parser-policy)
        (only-in ./rust-rowan language-rust-rowan-module-source))
(export RustRowanStrategy. RustRowanStrategyContract make-rust-rowan-strategy)

(def (rowan-strategy-shape? candidate)
  (and (build-strategy-common-shape? candidate)
       (eq? (.ref candidate 'provider) +rust-rowan-provider+)
       (not (language-grammar-parser-policy (.ref candidate 'descriptor)))))
(define-type (RustRowanStrategyContract @ PooFlowContract.)
  identity: 'gerbil-parser/rust-rowan-strategy
  .classify: (lambda (candidate context)
               (let (accepted? (rowan-strategy-shape? candidate))
                 (poo-flow-classification-evidence
                  'gerbil-parser/rust-rowan-strategy candidate accepted?
                  (if accepted? '() '((expected gerbil-parser/rust-rowan-strategy))) context))))

(def (emit-rowan-strategy strategy port)
  ;; Existing producer retains portable Scanner IR and parser policy gates.
  (let (source (language-rust-rowan-module-source (.ref strategy 'descriptor)))
    (write-string source port)))
(def +rust-rowan-provider+
  (declare-build-strategy-provider 'rust-rowan 'rust rowan-strategy-shape? emit-rowan-strategy))
(def RustRowanStrategy.
  (.o (:: self BuildStrategy.) provider: +rust-rowan-provider+))
(def (make-rust-rowan-strategy descriptor (prototype RustRowanStrategy.))
  (let (strategy (make-bound-build-strategy descriptor prototype))
    (validate RustRowanStrategyContract strategy)
    strategy))

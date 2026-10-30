;;; -*- Gerbil -*-
;;; Existing canonical Rust producer through the common POO build protocol.
(import (only-in :clan/poo/object .o .ref)
        (only-in :clan/poo/mop define-type validate)
        (only-in :yuanxu/object-types/contracts yuanxu-predicate-contract)
        (only-in ./build-strategy BuildStrategy. build-strategy-common-shape?
                 declare-build-strategy-provider make-bound-build-strategy)
        (only-in ../language/descriptor language-grammar-parser-policy)
        (only-in ./rust-runtime language-rust-runtime-module-source))
(export RustRuntimeStrategy. RustRuntimeStrategyContract make-rust-runtime-strategy)

(def (runtime-strategy-shape? candidate)
  (and (build-strategy-common-shape? candidate)
       (eq? (.ref candidate 'provider) +rust-runtime-provider+)
       (not (language-grammar-parser-policy (.ref candidate 'descriptor)))))
(define-type RustRuntimeStrategyContract
  (yuanxu-predicate-contract 'gerbil-parser/rust-runtime-strategy runtime-strategy-shape?))

(def (emit-runtime-strategy strategy port)
  ;; Existing producer retains portable Scanner IR and parser policy gates.
  (let (source (language-rust-runtime-module-source (.ref strategy 'descriptor)))
    (write-string source port)))
(def +rust-runtime-provider+
  (declare-build-strategy-provider 'rust-runtime 'rust runtime-strategy-shape? emit-runtime-strategy))
(def RustRuntimeStrategy.
  (.o (:: self BuildStrategy.) provider: +rust-runtime-provider+))
(def (make-rust-runtime-strategy descriptor (prototype RustRuntimeStrategy.))
  (let (strategy (make-bound-build-strategy descriptor prototype))
    (validate RustRuntimeStrategyContract strategy)
    strategy))

;;; Public build strategy extension surface for downstream language repositories.
(import (only-in ./src/compiler/build-strategy BuildStrategy. BuildStrategyContract make-bound-build-strategy emit-build-strategy)
        (only-in ./src/compiler/rowan-strategy RustRowanStrategy. RustRowanStrategyContract make-rust-rowan-strategy)
        (only-in ./src/compiler/fused-reduction FusedReductionStrategy. FusedReductionStrategyContract
                 make-fused-reduction-strategy fused-reduction-module emit-fused-reduction-module)
        (only-in ./src/language/entry declare-language-build-strategy emit-language-build-strategy))
(export BuildStrategy. BuildStrategyContract make-bound-build-strategy emit-build-strategy
        RustRowanStrategy. RustRowanStrategyContract make-rust-rowan-strategy
        FusedReductionStrategy. FusedReductionStrategyContract make-fused-reduction-strategy
        fused-reduction-module emit-fused-reduction-module
        declare-language-fused-reductions declare-language-build-strategy emit-language-build-strategy)

;;; Concrete convenience declarations belong to the public build facade;
;;; Loader admission itself imports only the common protocol.
(def (declare-language-fused-reductions descriptor (prototype FusedReductionStrategy.))
  (declare-language-build-strategy 'fused-reductions (make-fused-reduction-strategy descriptor prototype)))

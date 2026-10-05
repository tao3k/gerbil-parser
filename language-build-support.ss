;;; Public build strategy extension surface for downstream language repositories.
(import (only-in ./src/compiler/recursive-source RecursiveSourceStrategy. make-recursive-source-strategy)
        (only-in ./src/compiler/build-strategy BuildStrategy. BuildStrategyContract make-bound-build-strategy emit-build-strategy)
        (only-in ./src/compiler/rowan-strategy RustRowanStrategy. RustRowanStrategyContract make-rust-rowan-strategy)
        (only-in ./src/compiler/fused-reduction FusedReductionStrategy. FusedReductionStrategyContract
                 make-fused-reduction-strategy)
        (only-in ./src/language/entry declare-language-build-strategy emit-language-build-strategy))
(export RecursiveSourceStrategy. make-recursive-source-strategy
        BuildStrategy. BuildStrategyContract make-bound-build-strategy emit-build-strategy
        RustRowanStrategy. RustRowanStrategyContract make-rust-rowan-strategy
        FusedReductionStrategy. FusedReductionStrategyContract make-fused-reduction-strategy
        declare-language-build-strategy emit-language-build-strategy)

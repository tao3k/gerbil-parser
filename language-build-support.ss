;;; Public build strategy extension surface for downstream language repositories.
(import (only-in ./src/compiler/recursive-source RecursiveSourceStrategy. make-recursive-source-strategy)
        (only-in ./src/compiler/build-strategy BuildStrategy. BuildStrategyContract make-bound-build-strategy emit-build-strategy)
        (only-in ./src/compiler/runtime-strategy RustRuntimeStrategy. RustRuntimeStrategyContract make-rust-runtime-strategy)
        (only-in ./src/compiler/fused-reduction FusedReductionStrategy. FusedReductionStrategyContract
                 make-fused-reduction-strategy)
        (only-in ./language-support/development declare-language-build-strategy emit-language-build-strategy))
(export RecursiveSourceStrategy. make-recursive-source-strategy
        BuildStrategy. BuildStrategyContract make-bound-build-strategy emit-build-strategy
        RustRuntimeStrategy. RustRuntimeStrategyContract make-rust-runtime-strategy
        FusedReductionStrategy. FusedReductionStrategyContract make-fused-reduction-strategy
        declare-language-build-strategy emit-language-build-strategy)

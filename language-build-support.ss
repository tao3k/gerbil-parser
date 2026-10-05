;;; Public build strategy extension surface for downstream language repositories.
(import (only-in ./src/compiler/fused-reduction FusedReductionStrategy. FusedReductionStrategyContract
                 make-fused-reduction-strategy fused-reduction-module emit-fused-reduction-module)
        (only-in ./src/language/entry declare-language-fused-reductions emit-language-build-strategy))
(export FusedReductionStrategy. FusedReductionStrategyContract make-fused-reduction-strategy
        fused-reduction-module emit-fused-reduction-module
        declare-language-fused-reductions emit-language-build-strategy)

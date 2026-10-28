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

;;; Explicit compiler services are separate from request and value protocols.
(import (only-in ./event-fold-program-support prepare-event-fold-program)
        (only-in ./src/compiler/event-fold-scheme-modules event-fold-scheme-module-sources)
        (only-in ./src/compiler/event-fold-scheme-build compile-event-fold-scheme-units)
        (only-in ./src/build-support/compiler-profile initialize-compiler-build! run-build-tool)
        (only-in ./src/runtime/machine parser-machine-grammar-digest))
(export prepare-event-fold-program event-fold-scheme-module-sources
        compile-event-fold-scheme-units initialize-compiler-build! run-build-tool parser-machine-grammar-digest)

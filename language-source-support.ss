;;; Constrained public source recipes; provider registration stays in the engine.
(import (only-in ./src/language/source-strategy SourceStrategy. SourceStrategyContract)
        (only-in ./src/runtime/source-engines ShellSourceStrategy. LineSourceStrategy.)
        (only-in ./src/language/source deflanguage-source deflanguage-source-receipt declare-source-language))
(export SourceStrategy. SourceStrategyContract ShellSourceStrategy. LineSourceStrategy.
        deflanguage-source deflanguage-source-receipt declare-source-language)

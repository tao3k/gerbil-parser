;;; -*- Gerbil -*-
;;; Stable lr-parser facade; owner modules retain the original execution forms.
(import (only-in ./reduction-plan RecognitionSemantics. EventSemantics.
                 select-lr-action-factory select-lr-action-factory$poo-clos-generic)
        (only-in "./lr-parser/state.ss"
                 lr-recognition-view? lr-recognition-view-base lr-recognition-view-delta lr-recognition-fragment-offset current-lr-recognition-observer current-lr-transfer-yield-observer lr-recognition-fragment? lr-recognition-fragment-production-id lr-recognition-fragment-runtime lr-recognition-fragment-executor lr-recognition-fragment-entry-state lr-recognition-fragment-exit-state lr-recognition-fragment-children lr-recognition-fragment-lookahead lr-recognition-fragment-value lr-recognition-fragment-start lr-recognition-fragment-end lr-recognition-fragment-token-count current-lr-branch-budget lr-checkpoint? lr-lexical-mode? lr-lexical-mode-id lr-lexical-mode-terminals lr-runtime-lexical-mode-catalog lr-runtime-layout? lr-runtime-direct-step lr-runtime-event-step current-lr-event-program-enabled? lr-checkpoint-deterministic-actions lr-checkpoint-deterministic-shifts lr-failure-frontier? lr-failure-frontier-state lr-failure-frontier-expected-terminals)
        (only-in "./lr-parser/prepare.ss"
                 lr-runtime-with-action-selector lr-prepare install-lr-runtime-direct-step! install-lr-runtime-event-step! lr-runtime-event-program? lr-runtime-for-current-semantic-backend)
        (only-in "./lr-parser/recognition.ss"
                 lr-recognition-relocate lr-recognition-project)
        (only-in "./lr-parser/checkpoint-base.ss"
                 lr-checkpoint-state lr-initial-checkpoint require-lr-checkpoint-runtime! lr-checkpoint-lexical-mode lr-checkpoint-interned-lexical-mode-count lr-checkpoint-remaining-token-count lr-failure-frontier-remaining-tokens lr-rejection-condition?)
        (only-in "./lr-parser/batch.ss"
                 lr-parse/receipt lr-parse/prepared/receipt)
        (only-in "./lr-parser/checkpoint.ss"
                 lr-checkpoint-advance lr-checkpoint-advance-shifts lr-checkpoint-feed)
        (only-in "./lr-parser/transfer.ss"
                 lr-checkpoint-before-shift lr-checkpoint-fragment-compatible? lr-checkpoint-inject-fragment lr-runtime-fragment-reuse-safe? lr-recognition-fragment-exit-mode lr-checkpoint-resume lr-checkpoint-frontier lr-checkpoint-drive lr-checkpoint-drive/contextual lr-checkpoint-prefix-snapshot lr-prefix-snapshot-rebind lr-checkpoint-rebind-suffix lr-checkpoint-resume-suffix lr-failure-frontier-resume)
        (only-in "./lr-parser/entry.ss"
                 lr-parse lr-parse/prepared))
(export RecognitionSemantics. EventSemantics. select-lr-action-factory
        select-lr-action-factory$poo-clos-generic
        lr-runtime-with-action-selector
        lr-checkpoint-state lr-recognition-view? lr-recognition-view-base lr-recognition-view-delta
        lr-recognition-relocate lr-checkpoint-before-shift
        lr-checkpoint-fragment-compatible? lr-checkpoint-inject-fragment
        lr-runtime-fragment-reuse-safe?
        lr-recognition-fragment-offset lr-recognition-fragment-exit-mode
        current-lr-recognition-observer current-lr-transfer-yield-observer
        lr-recognition-fragment? lr-recognition-fragment-production-id
        lr-recognition-fragment-runtime lr-recognition-fragment-executor
        lr-recognition-fragment-entry-state
        lr-recognition-fragment-exit-state lr-recognition-fragment-children
        lr-recognition-fragment-lookahead lr-recognition-fragment-value
        lr-recognition-fragment-start lr-recognition-fragment-end
        lr-recognition-fragment-token-count lr-recognition-project
        current-lr-branch-budget lr-parse
        lr-parse/receipt
        lr-parse/prepared/receipt
        lr-prepare
        lr-parse/prepared
        lr-checkpoint? lr-initial-checkpoint require-lr-checkpoint-runtime! lr-checkpoint-advance
        lr-checkpoint-advance-shifts lr-checkpoint-resume lr-checkpoint-frontier
        lr-checkpoint-feed lr-checkpoint-drive
        lr-checkpoint-drive/contextual
        lr-checkpoint-lexical-mode
        lr-checkpoint-prefix-snapshot
        lr-prefix-snapshot-rebind
        lr-checkpoint-rebind-suffix
        lr-checkpoint-resume-suffix
        lr-lexical-mode?
        lr-lexical-mode-id
        lr-lexical-mode-terminals
        lr-runtime-lexical-mode-catalog lr-runtime-layout? lr-runtime-direct-step lr-runtime-event-step
        install-lr-runtime-direct-step! install-lr-runtime-event-step!
        current-lr-event-program-enabled? lr-runtime-event-program?
        lr-runtime-for-current-semantic-backend
        lr-checkpoint-interned-lexical-mode-count
        lr-checkpoint-deterministic-actions
        lr-checkpoint-deterministic-shifts
        lr-checkpoint-remaining-token-count
        lr-failure-frontier?
        lr-failure-frontier-state
        lr-failure-frontier-expected-terminals
        lr-failure-frontier-remaining-tokens
        lr-failure-frontier-resume
        lr-rejection-condition?)

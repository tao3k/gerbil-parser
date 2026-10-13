;;; -*- Gerbil -*-
;;; Stable incremental facade; owner modules retain the original execution forms.
(import (only-in ./probe
                 current-lr-lexical-plan-reuse-enabled?)
        (only-in ./reuse
                 current-lr-probe-reuse-enabled?)
        (only-in "./incremental/state.ss"
                 current-lr-source-index-enabled? incremental-session-source-index incremental-session-source-modes current-lr-fragment-reuse-enabled? +edit-schema+ +incremental-receipt-schema+ edit? edit-start-byte edit-delete-byte-length edit-inserted-text incremental-session? incremental-session-artifact incremental-session-recognition-root)
        (only-in "./incremental/session.ss"
                 make-incremental-session incremental-session-project-artifact incremental-session-publication-comparison)
        (only-in "./incremental/support.ss"
                 make-edit apply-edit)
        (only-in "./incremental/driver.ss"
                 parse-source/incremental parse-incremental-session))
(export current-lr-lexical-plan-reuse-enabled? current-lr-source-index-enabled? incremental-session-source-index incremental-session-source-modes
        current-lr-probe-reuse-enabled? current-lr-fragment-reuse-enabled?
        +edit-schema+
        +incremental-receipt-schema+
        make-edit
        edit?
        edit-start-byte
        edit-delete-byte-length
        edit-inserted-text
        apply-edit
        parse-source/incremental
        make-incremental-session
        incremental-session?
        incremental-session-artifact
        incremental-session-recognition-root
        incremental-session-project-artifact
        incremental-session-publication-comparison
        parse-incremental-session)

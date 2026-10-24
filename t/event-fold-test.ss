(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; A single stateful Scheme algorithm is executable and AOT-lowerable.

(import (only-in :std/test check check-exception test-case test-suite)
        (only-in :std/encoding/json JSONReadOptions string->json)
        (only-in "event-strategy-fixture.ss" event-lines-language-grammar)
        (only-in "event-fold-fixture.ss" parse-fold-lines parse_fold_lines
                 parse-outline-lines parse_outline_lines)
        (only-in :gerbil-parser/rust-runtime-event-support
                 event-fold-ir-json run-event-fold)
        (only-in :gerbil-parser/src/compiler/event-fold-runtime
                 prepare-event-fold-program run-event-fold-program)
        (only-in :gerbil-parser/src/compiler/event-strategy-aot
                 fold-source-lines source-line-events)
        (only-in :gerbil-parser/src/compiler/event-fold-state-frame
                 prepare-fold-state-layout instantiate-fold-state-frame
                 fold-frame-ref fold-frame-update! fold-frame-bound?
                 fold-frame-bind! fold-frame-unbind!)
        (only-in :gerbil-parser/src/modules/parser/interface
                 source-offset-after source-pattern-end source-pattern-at?
                 source-ascii-ci-pattern-at?))


(include "fixtures/event-fold-state.ssi")
(include "fixtures/event-fold-emission.ssi")

(deflanguage-parser-tests event-fold-test "Scheme stateful event fold AOT"
  (engine)
    (suite "state and preparation" event-fold-state-tests)
    (suite "emission and lowering" event-fold-emission-tests))

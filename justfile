set shell := ["sh", "-eu", "-c"]

# Public native declaration lowering; test-all discovers this suite once.
test-line-events output load_path:
    mkdir -p "{{output}}"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/modules/parser/line-structure-objects.ss src/modules/parser/source-event-scope-types.ss src/modules/parser/source-event-scope-objects.ss src/modules/parser/source-event-scope-funs.ss src/modules/parser/line-event-funs.ss src/modules/parser/list-event-funs.ss src/modules/parser/inline-link-event-funs.ss src/modules/parser/source-boundary-types.ss src/modules/parser/source-boundary-objects.ss src/modules/parser/source-boundary-funs.ss src/modules/parser/interface.ss t/line-event-test.ss t/inline-link-event-test.ss t/source-boundary-test.ss t/source-event-scope-test.ss t/list-event-test.ss
    GAMBOPT=max-heap=1G,debug=q GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil test -v 5 t/line-event-test.ss t/inline-link-event-test.ss t/source-boundary-test.ss t/source-event-scope-test.ss t/list-event-test.ss 2>&1 | tee "{{output}}/line-events-test.log"
    rg --quiet '^HARNESS-OK' "{{output}}/line-events-test.log"
    rg --quiet '^OK$' "{{output}}/line-events-test.log"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{output}}/line-events-test.log"

test-source-priority:
    GAMBOPT=max-heap=1G,debug=q gerbil test -v 3 t/source-priority-test.ss

build-source-priority output load_path:
    mkdir -p "{{output}}"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/modules/parser/source-pattern-funs.ss src/modules/parser/funcs.ss src/modules/parser/interface.ss

build-process-fixture: build-qualification
    mkdir -p .data/process-controls
    ${CC:-cc} -std=c11 -Wall -Wextra -Werror t/fixtures/qualification/process-fixture.c -o .data/process-controls/fixture

# Real initialization/load phases; never substitutes for strict controls.
diagnose-process-startup: build-process-fixture
    gerbil env gxi t/fixtures/qualification/startup-controls.ss "$PWD/.data/process-controls/fixture"

test-process-contracts: build-process-fixture
    gerbil env gxi t/fixtures/qualification/process-controls.ss "$PWD/.data/process-controls/fixture"

build-qualification:
    gerbil env gxi build-qualification.ss compile

# Focused native suites; test-all discovers the same suites once.
test-qualification-suites: build-qualification
    GAMBOPT=max-heap=1G,debug=q gerbil test -v 3 t/native-plans-test.ss t/bootstrap-test.ss

# Exercise the downstream admission caller after support-module relocation.
test-process-admission: build-qualification
    GAMBOPT=max-heap=1G,debug=q gerbil env gxi scripts/tests/process-qualification-test.ss

test-compiler-process:
    GAMBOPT=max-heap=1G,debug=q gerbil env gxi scripts/tests/compiler-process-test.ss

# Cache admission controls stay Scheme and share the CI owner.
test-preparation:
    GAMBOPT=max-heap=1G,debug=q gerbil env gxi scripts/tests/preparation-test.ss

# Build the shared helper through its compiled module owner before loading source suites.
test-workers output=".data/worker-controls": build-qualification
    gerbil compile -O t/fixtures/tla-sany-differential/worker-control.ss
    gerbil env gxi test-processes.ss workers "{{output}}"

# Compile the suite's own fixtures, not only its import declarations.
build-event-fold-native output load_path:
    mkdir -p "{{output}}"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/runtime/source-lines.ss src/runtime/event-fold-lines.ss src/compiler/event-strategy-aot.ss src/compiler/event-fold-program.ss src/compiler/event-fold-runtime.ss src/compiler/event-fold-ir.ss src/compiler/event-fold-scheme-context.ss src/compiler/event-fold-scheme-expressions.ss src/compiler/event-fold-scheme-statements.ss src/compiler/event-fold-scheme.ss t/event-strategy-fixture.ss
    just generate-event-fold-native "{{output}}" "{{load_path}}"

generate-event-fold-native output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil env gxi t/generate-event-fold-native.ss "{{output}}/event-fold-native-generated.ss"
    ! rg --quiet 'fold-statements|fold-predicate|fold-offset |fold-uint |run-event-fold|eval ' "{{output}}/event-fold-native-generated.ss"

build-event-fold-modules output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/compiler/event-fold-scheme-modules.ss src/compiler/event-fold-scheme-build.ss

link-event-fold-native output load_path:
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O -exe -o "{{output}}/event-fold-native" "{{output}}/event-fold-native-generated.ss"

test-event-fold-native output load_path:
    GAMBOPT=max-heap=1G,debug=q GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" "{{output}}/event-fold-native" > "{{output}}/event-fold-native.log" 2>&1
    rg '^CASE-OK |^OK$' "{{output}}/event-fold-native.log"
    test "$(rg --count '^CASE-OK ' '{{output}}/event-fold-native.log')" = 96
    rg --quiet '^OK$' "{{output}}/event-fold-native.log"
    ! rg --quiet 'ERROR|FAILED|FAILURE' "{{output}}/event-fold-native.log"

build-event-fold-compiled output load_path:
    mkdir -p "{{output}}"
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/runtime/source-lines.ss src/runtime/event-fold-lines.ss src/compiler/event-strategy-aot.ss src/compiler/event-fold-state-frame.ss src/compiler/event-fold-program.ss src/compiler/event-fold-runtime.ss src/compiler/event-fold-aot.ss rust-runtime-event-support.ss
    GERBIL_PATH="{{output}}" GERBIL_LOADPATH="{{output}}/lib:{{load_path}}" gerbil compile -O src/compiler/event-fold-ir.ss src/compiler/event-fold-scheme-context.ss src/compiler/event-fold-scheme-expressions.ss src/compiler/event-fold-scheme-statements.ss src/compiler/event-fold-scheme.ss src/compiler/event-fold-scheme-modules.ss t/event-strategy-fixture.ss t/event-fold-fixture.ss t/event-fold-test.ss t/event-fold-scheme-test.ss

test-event-fold-compiled binary output: build-qualification
    gerbil env gxi qualify.ss compiled-event-fold "{{binary}}" "{{output}}"

# Fast lexical algorithm loop; keep the POO Flow Case heap fence pre-import.
test-lexer:
    GAMBOPT=max-heap=1G,debug=q gerbil test -v 3 t/ranked-regular-scanner-test.ss

# LR mode admission is checked separately from the inner scanner transition.
test-lexical-mode:
    GAMBOPT=max-heap=1G,debug=q gerbil test -v 3 t/grammar-composition-contract-test.ss t/grammar-composition-execution-test.ss t/grammar-composition-lowering-test.ss

# Full qualification after the focused algorithm loop.
test-all: build-qualification
    GAMBOPT=max-heap=1G,debug=q gerbil test -v 3 t/... languages/...

# All contracts execute from current source; every suite has a hard deadline.
test-performance-before-compile: build-qualification
    gerbil env gxi qualify.ss performance

# Reports each complete language batch sample without a shell timing wrapper.
benchmark-versioned-matched label="current" samples="20":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/versioned-languages/matched-batch.ss {{label}} {{samples}}

# Compare long deterministic parses between separate v0.19 package revisions.
benchmark-arithmetic-matched terms="3200" samples="30":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/arithmetic-scale/matched-stream.ss {{terms}} {{samples}}

# Parsed input has one addition term per line; the parser is already built.
benchmark-arithmetic-lines lines="1024" samples="30":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/arithmetic-scale/matched-stream.ss {{lines}} {{samples}} lines

# Prepared-token stages show where CPU work goes; they are not streaming totals.
benchmark-arithmetic-stages terms="3200":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/arithmetic-scale/benchmark.ss {{terms}}

# Complete LR construction at a scale where partition costs are visible.
benchmark-lr-grammar-construction contexts="256" samples="3":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/lr1-partition/context-scale.ss {{contexts}} {{samples}}

# Complete immutable Examples syntax and lossless artifact receipt.
tla-sany-corpus corpus: build-qualification
    GAMBOPT=max-heap=1G,debug=q GERBIL_PARSER_LR_TRACE=1 gerbil env gxi qualify.ss run --timeout 600 --idle-timeout 5 --log .data/tla-source.log -- gerbil env gxi -e '(load "t/fixtures/tla-sany-differential/preload.ss") (preload-module "gerbil-parser/languages/tla-plus/sany-candidate")' t/fixtures/tla-sany-differential/corpus.ss {{quote(corpus)}}
# Current-source scanner execution; plan-batch includes one plan and requests bindings.
benchmark-contextual-scanner axes="16" samples="20" words="128" literals="0" phase="scan" requests="1": build-qualification
    GERBIL_LOADPATH=.. GERBIL_PATH=$PWD/.gerbil GAMBOPT=max-heap=1G,debug=q gerbil env gxi qualify.ss run --timeout 120 --idle-timeout 5 --log /private/tmp/parser-contextual-scanner-scale.log --require SCANNER-SCALE-OK -- gxi t/benchmarks/contextual-scanner/matched-scale.ss {{axes}} {{samples}} {{words}} {{literals}} {{quote(phase)}} {{requests}}

# Complete contextual ParseArtifact pairs; plan creation is included per batch.
benchmark-contextual-parser requests="16" samples="5": build-qualification
    GERBIL_LOADPATH=.. GERBIL_PATH=$PWD/.gerbil GERBIL_PARSER_LR_TRACE=1 GAMBOPT=max-heap=1G,debug=q gerbil env gxi qualify.ss run --timeout 120 --idle-timeout 5 --log /private/tmp/parser-contextual-parser-scale.log --require CONTEXTUAL-PARSER-SCALE-OK -- gxi -e '(load "t/fixtures/tla-sany-differential/preload.ss") (preload-module "gerbil/tools/gxtest")' t/benchmarks/contextual-scanner/matched-parser.ss {{requests}} {{samples}}

# Complete deferred batches; each sample reports real token and byte coverage.
benchmark-contextual-deferred delimiters="512" samples="5": build-qualification
    GERBIL_LOADPATH=.. GERBIL_PATH=$PWD/.gerbil GAMBOPT=max-heap=1G,debug=q gerbil env gxi qualify.ss run --timeout 120 --idle-timeout 5 --log /private/tmp/parser-contextual-deferred-scale.log --require DEFERRED-SCALE-OK -- gxi t/benchmarks/contextual-scanner/deferred-scale.ss {{delimiters}} {{samples}}

# Host-native toolchain; compiler admission and real test progress stay separate.
test-local suite="rust" gerbil_path=".gerbil": build-qualification
    gerbil env gxi qualify.ss local --suite {{quote(suite)}} --gerbil-path {{quote(gerbil_path)}}
# Real TLA+ grammar through the default LALR route; complete LRSpec stability.
benchmark-lalr-construction samples="20":
    gerbil env gxi -:max-heap=2G,debug=q t/benchmarks/lr1-partition/lalr-construction.ss {{samples}}

# Complete output equality and inverse edits before CPU sampling.
benchmark-incremental-topology sizes="400 800 1600 3200":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/incremental-session/benchmark.ss topology {{sizes}}

# Independent siblings distinguish avoidable suffix replay from expression ancestry.
benchmark-incremental-siblings sizes="100 200 400 800":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/incremental-session/benchmark.ss topology-hcl {{sizes}}

# Capture gate reevaluates productions and compares both forward/inverse artifacts.
benchmark-incremental-capture sizes="2 400 800 1600 3200":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/incremental-session/benchmark.ss topology-capture {{sizes}}

benchmark-incremental-siblings-capture sizes="2 100 200 400 800":
    gerbil env gxi -:max-heap=1G,debug=q t/benchmarks/incremental-session/benchmark.ss topology-hcl-capture {{sizes}}

# Stage controls and uninstrumented public entry; 40 x 100, native-only.
benchmark-gql gerbil_path=".gerbil": build-qualification
    gerbil env gxi qualify.ss local --suite gql-profile --gerbil-path {{quote(gerbil_path)}}

# Optional application-owned actor library; 1/4 clients, 1/4 actors.
benchmark-gql-actors gerbil_path=".gerbil": build-qualification
    gerbil env gxi qualify.ss local --suite gql-actors --gerbil-path {{quote(gerbil_path)}}

# Real C ABI edits, full payload parity, natural GC; phase tracing is separate.
benchmark-source-edits samples="20" lines="128": build-qualification
    gerbil env gxi qualify.ss run --timeout 90 --idle-timeout 5 --log /private/tmp/parser-source-edit-cost.log --require SOURCE-EDIT-BENCHMARK-OK -- gxi -e '(load "t/fixtures/tla-sany-differential/preload.ss") (call-with-compiled-interface-trace (lambda () (eval (quote (import :gerbil-parser/t/benchmarks/source-edits/benchmark))) (eval (quote (main "{{samples}}" "{{lines}}")))))'

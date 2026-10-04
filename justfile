# Fast lexical algorithm loop; keep the POO Flow Case heap fence pre-import.
test-lexer:
    GAMBOPT=max-heap=1G,debug=q gerbil test -v 3 t/ranked-regular-scanner-test.ss

# LR mode admission is checked separately from the inner scanner transition.
test-lexical-mode:
    GAMBOPT=max-heap=1G,debug=q gerbil test -v 3 t/grammar-composition-test.ss

# Full qualification after the focused algorithm loop.
test-all:
    GAMBOPT=max-heap=1G,debug=q gerbil test -v 3 t/... languages/...

# All contracts execute from current source; every suite has a hard deadline.
test-performance-before-compile:
    python3 scripts/performance-preflight.py

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
tla-sany-corpus corpus:
    GAMBOPT=max-heap=1G,debug=q GERBIL_PARSER_LR_TRACE=1 gerbil env gxi t/fixtures/tla-sany-differential/watch.ss gerbil env gxi -e '(load "t/fixtures/tla-sany-differential/preload.ss") (preload-module "gerbil-parser/languages/tla-plus/sany-candidate")' t/fixtures/tla-sany-differential/corpus.ss {{quote(corpus)}}
# Current-source scanner execution; plan-batch includes one plan and requests bindings.
benchmark-contextual-scanner axes="16" samples="20" words="128" literals="0" phase="scan" requests="1":
    GERBIL_LOADPATH=.. GERBIL_PATH=$PWD/.gerbil GAMBOPT=max-heap=1G,debug=q python3 scripts/run-bounded.py --timeout 120 --idle-timeout 5 --log /private/tmp/parser-contextual-scanner-scale.log --require SCANNER-SCALE-OK -- gxi t/benchmarks/contextual-scanner/matched-scale.ss {{axes}} {{samples}} {{words}} {{literals}} {{quote(phase)}} {{requests}}

# Complete contextual ParseArtifact pairs; plan creation is included per batch.
benchmark-contextual-parser requests="16" samples="5":
    GERBIL_LOADPATH=.. GERBIL_PATH=$PWD/.gerbil GERBIL_PARSER_LR_TRACE=1 GAMBOPT=max-heap=1G,debug=q python3 scripts/run-bounded.py --timeout 120 --idle-timeout 5 --log /private/tmp/parser-contextual-parser-scale.log --require CONTEXTUAL-PARSER-SCALE-OK -- gxi -e '(load "t/fixtures/tla-sany-differential/preload.ss") (preload-module "gerbil/tools/gxtest")' t/benchmarks/contextual-scanner/matched-parser.ss {{requests}} {{samples}}

# Complete deferred batches; each sample reports real token and byte coverage.
benchmark-contextual-deferred delimiters="512" samples="5":
    GERBIL_LOADPATH=.. GERBIL_PATH=$PWD/.gerbil GAMBOPT=max-heap=1G,debug=q python3 scripts/run-bounded.py --timeout 120 --idle-timeout 5 --log /private/tmp/parser-contextual-deferred-scale.log --require DEFERRED-SCALE-OK -- gxi t/benchmarks/contextual-scanner/deferred-scale.ss {{delimiters}} {{samples}}

# Host-native toolchain; compiler admission and real test progress stay separate.
test-native-local suite="rust" gerbil_path=".gerbil":
    python3 scripts/test-native-local.py --suite {{quote(suite)}} --gerbil-path {{quote(gerbil_path)}}
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
benchmark-gql gerbil_path=".gerbil":
    python3 scripts/test-native-local.py --suite gql-profile --gerbil-path {{quote(gerbil_path)}}

# Optional application-owned actor library; 1/4 clients, 1/4 actors.
benchmark-gql-actors gerbil_path=".gerbil":
    python3 scripts/test-native-local.py --suite gql-actors --gerbil-path {{quote(gerbil_path)}}

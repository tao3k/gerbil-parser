# ISO GQL performance

Run from the repository root after the native package build:

```sh
just test-native-local gql .gerbil
just benchmark-gql .gerbil
```

The language owns `runtime-benchmark-test.ss`, the unchanged ASP runtime
contract at `runtime/benchmark.ss`, and semantic diagnostic tests at
`benchmark-profile-test.ss`. CI runs them through native-suite with the rest
of the language tests. Both native CI jobs also execute the 40 x 100 component
and actor diagnostics, preserving sample/summary receipts in the job logs.
Benchmark support is compiled explicitly, outside the
production library catalog.

## Read the receipts

`runtime/matched-stages.ss` reports 40 samples of 100 calls by default:

| Stage | Work measured | Boundary |
| --- | --- | --- |
| global-lexing | Full lexical catalog | A control, not the directed scanner path |
| prepared-lr | LR over significant tokens captured from the public parser | Token admission is identical; no source scanning |
| artifact-publication | Source identity, lossless events and publication | Reuses the admitted recognition root |
| full-source | Actual public language entry | Includes streaming scanner, LR and publication |

Every prepared artifact is checked against the full public result. Each
component warms up once and performs one initial GC, followed by natural GC
during the samples. Receipts include wall and CPU P50/P95 **per 100-call
batch**, wall P50/P95 per parse, bytes allocated per parse, and each sample's
GC count/time. `wallP50Sample`, `wallP95Sample` and `maxWallSample` retain
CPU, GC and allocation counters from the **same wall-ranked observation**.
Independent wall/CPU percentiles may select different observations and must
not be subtracted to attribute tail latency. Each sample also reports signed
`wall-minus-cpu-ms`; this can include elapsed waiting and measurement noise
(and can be negative), so it does not identify the responsible scheduler or
OS mechanism. GC time is already included in wall time and is not an extra
additive stage. Setup, output and semantic comparisons are outside the timed
batch. These component controls **must not be summed or used as exact shares
of the streaming full-source time**. They do identify which components warrant
further profiling without silently switching the production parse algorithm.

The existing admission contract remains 150ms wall P95 per 100 calls. A batch
P95 of 137ms is approximately 1.37ms per parse; it is not a single-query P95.
Neither CPU percentiles nor an average across a batch replace that gate or
measure the distribution of individual query latency.

For unexplained full-source outliers, compile `runtime/native-timing.ss`
with `gxc -V` (Darwin needs `-ld-options
"-Wl,-undefined,dynamic_lookup"`) and run its `main` through the native preload
helper. It records paired monotonic/wall clocks, CPU, GC, allocation, page
faults and voluntary/involuntary context switches for the same 40 x 100 work.
Wall minus CPU is elapsed time without process CPU consumption, not proof of
any specific scheduler cause; GC wall is not an independent additive phase.

## Coverage and limitations

`parser-test.ss` owns the pinned official corpus, malformed inputs and lossless
CST conformance. `query-syntax-test.ss` owns the language query surface.
`benchmark-profile-test.ss` checks exact stage equivalence with Unicode and
trivia, and rejects invalid measurement sizes and rejected source inputs.
The 40 x 100 performance workload remains the representative query from
`grammar.ss`; it does not claim a percentile across the whole official corpus.

## Local measurement, 2026-10-04

Before rebasing onto the concurrent runtime changes in `6757550`, the first
native 40 x 100 diagnostic run on `638e544` produced these batch medians:

| Component control | Wall P50 | Wall P95 | CPU P50 | Bytes/parse |
| --- | ---: | ---: | ---: | ---: |
| global lexing | 1.195ms | 2.323ms | 1.157ms | 52,356 |
| prepared LR | 8.553ms | 18.916ms | 8.400ms | 136,180 |
| publication | 1.640ms | 2.820ms | 1.641ms | 44,708 |
| public full source | 12.136ms | 54.503ms | 11.983ms | about 228,647 |

The full-source P95 sample included one GC taking 42.449ms of its 54.503ms
wall time. This identifies elapsed time inside collection in that sample;
it does not establish a Gambit defect or identify the objects being collected.
The isolated LR control has the largest CPU/allocation cost among the three
prepared controls, making recognition allocation the next profiling target.
It does not establish an additive LR share of the streamed public entry.
The maximum full-source sample was 95.279ms, with 58.364ms CPU and one
41.271ms GC: it also includes elapsed time without CPU consumption. These
are historical local receipts, not universal latency guarantees or an explanation
of the different historical 137ms/593ms outliers.

## Paired tail observations on `74f104c`

The Ubuntu native CI run [37174656471](https://github.com/tao3k/gerbil-parser/actions/runs/37174656471)
completed the unchanged admission gate at 81.257ms wall P95. Its separate
40 x 100 full-source diagnostic selected these actual observations:

| Wall rank | Sample | Wall | CPU | GC wall | GC count |
| --- | ---: | ---: | ---: | ---: | ---: |
| P50 | 14 | 24.873ms | 24.878ms | 0ms | 0 |
| P95 | 19 | 79.411ms | 79.387ms | 54.006ms | 1 |
| Maximum | 9 | 80.609ms | 80.595ms | 55.386ms | 1 |

The P95 sample spends about 68% of its elapsed time in GC. This is a paired
observation, not the difference between independent percentiles. The ordinary
no-GC P50 batch allocates 22,887,376 bytes (about 228,874 per parse). Allocation
in the prepared LR control therefore remains a concrete investigation target;
these measurements do not locate an individual allocation site or establish
that changing the heap policy improves parsing.

On the same source revision, the local diagnostic's wall P95 observation was
sample 29: 212.683ms wall, 58.673ms CPU, one GC taking 199.528ms wall. Its
maximum, sample 32, had 439.664ms wall, 29.318ms CPU and **no GC**. The latter
rules out GC as the cause of that particular maximum and demonstrates elapsed
time without comparable CPU consumption. It does not identify the waiting
mechanism. The unchanged local admission gate failed at 214.611ms P95; the
Linux pass does not replace that failure. These historical samples are tied
to `74f104c`, not measurements of subsequent diagnostic changes.

## Standard library review: prepared index dispatch

[SRFI-1's reference implementation guidance](https://srfi.schemers.org/srfi-1/srfi-1.html#Rationale)
prefers common-case fast paths, constant-space iteration and avoiding temporary
structures. The installed Gerbil v0.19 compiler's `generate-runtime-begin%`
and `generate-runtime-define-values%` similarly use explicit pair matching,
tail recursion and reverse accumulators. Existing recognition materialization
already uses Gerbil's `:std/list/list-builder`, retaining its implementation
rather than duplicating a list builder.

The concrete change from this review is narrower: prepared LR action and goto
indexes contain only proper association lists or their prepared tables.
Lookup now dispatches with `pair?`/`null?`, instead of traversing the complete
list with `list?` before `assoc`. The association scan, equality behavior,
first-duplicate rule and original entry identity remain unchanged. This removes
one redundant traversal on list-backed queries; it does not reduce recognition
allocation or claim to solve the observed GC/waiting tail. `t/lr-index-test.ss`
covers empty, singleton, threshold and wide rows, misses, equal string keys,
and duplicate entry identity in both index representations. End-to-end cost
must still be judged from native prepared-LR and full-source samples.

Native comparison must retain the same compiler configuration. Gerbil
`:std/make` defaults to `optimize: #t`; a plain `gxc` rebuild of a production
module does not match that configuration. Rebuild production modules with the
package graph or matching `gxc -O` settings before comparing allocation/CPU
receipts. Diagnostic support compilation alone is not production qualification.
The rebased change passed 11 native cases across the index, GQL profile and
recognition-sequence/GLR modules after compiling the affected GQL runtime
modules with optimization enabled. The separate complete local package build
was stopped by the unchanged 30-second output-idle fence after its import
closure projection; these focused passes do not establish a full package pass.

## Empty operand-action allocation experiment

The optimized LR module still constructed a callback capturing the byte offset
and fragment constructor before calling `foldl` on empty operand actions.
Both semantic backends now return the original value for an empty action list;
nonempty actions keep the original fold and field/alias semantics.

The Scheme/ASP runtime owners emit matched observations directly into job logs.
Use `runtime/matched-stages.ss` with the existing benchmark contracts to compare
prepared LR and full-source costs. Result snapshots are not stored in the
language package. Generated Scheme places the null check before constructing
the fold callback; allocation savings do not imply a wall/CPU or GC guarantee.

`lr-empty-action-test.ss` checks nullable, identity and decorated operands,
rejection equality across materialized/event backends, and edits through an
empty source against independent production replay. Together with the GQL
profile, event-program and layout modules, 27 native cases passed. The original
150ms runtime admission gate is unchanged; these diagnostics do not turn the
known local wall-time failure into admission success.

## Optional actor library

The package remains a library. Importing `parser-actor-support` starts no worker,
listener or daemon. The application explicitly creates and closes local actors:

```scheme
(import :gerbil-parser/languages/gql/iso-39075-2024/parser
        :gerbil-parser/parser-actor-support)
(let (worker (spawn-parser-actor gql-iso-parser))
  (try
   (parser-actor-parse worker "RETURN 1" 5)
   (finally (parser-actor-stop! worker))))
```

This uses the installed Gerbil `:std/actor` LocalActor, envelope continuations,
request expiry and shutdown protocol, with a spawned green thread. Each actor
serializes its parse requests; applications can own multiple actors. Generated
drivers must be installed before sharing the prepared machine, which must not
be mutated while actors use it. Each synchronous submission snapshots the
caller's mutable source string. Results use the same complete ParseArtifact
as the direct API, including rejected artifacts.

A timeout bounds the caller's wait; Gerbil discards expired queued envelopes.
It does not preempt an already running parser. Shutdown waits behind earlier
work and joins the owned thread. This API does not implement a global pool,
mailbox capacity limit, remote actor server or hard cancellation. Applications
own admission and resource limits. Green thread concurrency is not a promise
of multicore speedup or lower individual parse CPU cost.

```sh
just benchmark-gql-actors .gerbil
```

`runtime/actors.ss` reports 40 batches of 100 complete requests for 1 client /
1 actor, 4 clients / 1 actor, and 4 clients / 4 actors. Actors are created before
measurement and joined afterwards. Timed work includes client spawning,
message/reply scheduling, source snapshots and parsing. Clients share one
prepared language machine; the last artifact from every client is checked
against the direct public parser. Compare these batch costs with full-source
controls; batch-normalized P95 is not an individual queued-request percentile.
`actor-test.ss` verifies concurrent reply isolation, Unicode/rejected artifact
identity, invalid-request recovery and actual shutdown.

In the first local actor run, batch P50/P95 was 12.573/58.333ms (1 client,
1 actor), 13.216/58.192ms (4 clients, 1 actor), and 15.263/63.209ms (4 clients,
4 actors). These runs do not establish a speedup over direct parsing.

The process allocation counter showed large isolated jumps during thread
switching (one batch reported 921,816,080 bytes while normal batches reported
about 23.6MB). Their cause is unqualified. Actor receipts preserve those raw
process deltas and label their scope `process-with-thread-switches`; they report
`allocatedBytesPerParse` as unavailable rather than assigning the jumps to
requests or presenting a misleading median as per-request memory cost.

## Rebased runtime measurement

After rebasing onto `1cdab80` and rebuilding with ASP v0.1.2.2, the unchanged
local runtime gate failed at 214.611ms batch P95. The diagnostic completed
40 samples and reported full-source wall P50/P95 19.782/212.683ms versus CPU
P50/P95 14.614/58.673ms. One no-GC sample took 439.664ms wall but 29.318ms CPU;
a later GC sample took 327.971ms wall, 66.641ms CPU and 287.830ms GC wall.
These are paired measurements of substantial non-CPU delay, not an identified
OS cause. The failed gate remains failed; these observations do not authorize
raising its ceiling. The earlier clean CPU/allocation controls remain useful
historical comparisons, with the runtime revision explicitly recorded.

## Multi-operand reduction callbacks

The generic materialized and event reducers now consume operand and semantic
value lists through private tail-recursive helpers. Offset and constructor are
explicit parameters, so each concatenating reduction avoids a captured fold
callback. Operand order, nullable reductions and `foldl2` termination behavior
are preserved; token admission and artifact publication keep their existing
owners. Generated direct reducers and selective GLR admission are unchanged.

Use the existing 40 x 100 component diagnostics to compare allocated bytes and
paired CPU/GC observations. `lr-empty-action-test.ss` also asserts exact field
and token order for three operands, trivia and a nullable middle operand, in
addition to backend equality and incremental production replay. An allocation
change alone does not satisfy the 150ms wall P95 contract.

## Decorated operand actions and derivation counts

Field and alias action chains now use explicit-parameter tail recursion in both
semantic backends. This preserves `foldl1` action order and terminal identity
while removing the callback that captured offset and constructor for every
nonempty action list. The nested field/alias regression in
`t/lr-empty-action-test.ss` checks independent node and field order, trivia,
lossless publication, and equality between the materialized and event paths.

The existing matched-stage command also emits `GQL-REDUCTION-COUNTS` before any
measured batch. For the representative query its selected derivation contains
288 reductions, 75 concatenating reductions, 192 identity operands and 132
decorated operands. `profile-gql-reductions` verifies that enabling the observer
leaves the public artifact identical; rejected inputs produce no counts report.
Counts describe the selected derivation, not discarded GLR work or actor
requests. The observer, tree traversal, validation and logging are untimed.

Compare no-GC allocation observations across baseline, candidate, reversal and
candidate repeat using the same native compiler, input, heap and sample counts.
Keep wall-ranked CPU/GC observations paired. Reduced allocation does not by
itself prove a latency improvement or satisfy the unchanged 150ms wall gate.

## Investigating GC with Gambit's own counters

GC wall time alone cannot distinguish collector CPU work from elapsed time
without CPU consumption, or identify which application objects survived.
`matched-stages.ss` therefore reports `gc-cpu-ms` alongside cumulative GC wall
and count deltas in each paired observation. After the existing initial GC it
also emits `GQL-GC-BASELINE`, including heap, live, movable and still bytes.
Each summary retains that baseline as `gcBaseline`.

`latest-gc` is `#f` when the batch did not collect. Otherwise it reports the
**latest collection in the whole VM**, including its CPU/wall time, heap size,
allocation counter, and live/movable/still bytes. If several collections occur
in one batch, this snapshot describes only the last; the GC time/count deltas
still cover all of them. Heap and live bytes are snapshots, not differences,
and cannot be attributed solely to GQL. Loaded modules, parser tables,
prepared inputs and reference artifacts also belong to this VM. In particular,
compiled permanent objects are not equivalent to dynamically allocated live
objects, so module size alone does not measure collection work.

The slot meanings were checked against the installed Gambit revision's
[process-statistics implementation](https://github.com/gambit/gambit/blob/dcd677cd3e40860bdd27dfbdbf5e3ce46ab03813/lib/_kernel.scm#L3999)
and its [memory implementation](https://github.com/gambit/gambit/blob/dcd677cd3e40860bdd27dfbdbf5e3ce46ab03813/lib/mem.c).
These diagnostics leave the 1 GiB heap cap, natural timed collections, and
existing admission deadlines unchanged. Compare allocation sites and live
heap under identical input/compiler/process conditions before drawing a
runtime conclusion. Validate exact artifacts when changing representation.

Gerbil's official guide recommends
[GCC for compiled code](https://gerbil.scheme.org/guide/getting-started.html)
and describes [full-program optimization](https://gerbil.scheme.org/guide/intro.html)
as a distinct executable build option. Record the actual compiler and build
mode for each comparison: optimized separately compiled modules (`gxc -O`)
do not by themselves establish that an executable was built with FPO. Neither
compiler advice nor these counters establish a speedup without a matched run.

A fresh-process startup control on the same optimized module products, input,
1 GiB cap and 40 x 100 batches reproduced these full-source baseline live bytes
in two pairs: 201,709,952 with compiled-interface admission, and 117,666,952
with runtime-module loading alone. The 84,043,000-byte difference is startup
footprint in this harness, not per-parse allocation. The runtime-only control
loads the generated native runtime wrappers and calls the same compiled entry;
it omits interface admission rather than substituting a different parser.
This identifies a concrete measurement-path contribution. It does not identify
the remaining live objects, measure an FPO executable, qualify latency under
host contention, or establish a Gambit defect. Functional source tests still
need their macro interfaces; those and runtime-only deployment are distinct
measurement contexts.


## Counting UTF-8 bytes without encoding temporary buffers

The scanner and incremental runtime use Gerbil's `:std/string/utf8`
`string-utf8-length` for eight length-only calls. The installed standard
implementation delegates to Gambit's `##string->utf8-length`; the
[official UTF-8 documentation](https://gerbil.scheme.org/reference/std/text/utf8.html)
also describes its optional start/end interval. Checkpoint validation counts
that interval directly instead of first copying the source prefix. Hashing,
byte slicing and boundary validation still use actual UTF-8 buffers where
those bytes are needed.

A controlled native comparison against revision
`ceab4bee9e6a14f9bc85053750b1b6d06466889d` on the representative 138-byte GQL query
(51 tokens, 37 significant), with 40 batches of 100 calls per stage and a
1 GiB heap cap, produced the following median bytes per parse among batches
with no collection:

| Implementation | Global lexing | Prepared LR | Artifact publication | Full source |
| --- | ---: | ---: | ---: | ---: |
| Original | 52,356 | 111,140 | 44,324 | 203,340.32 |
| Direct length | 50,724 | 111,140 | 44,324 | 201,708.32 |
| Original restored | 52,356 | 111,140 | 44,324 | 203,340.32 |
| Direct length repeated | 50,724 | 111,140 | 44,324 | 201,708.32 |

Each row retains 40, 39, 40 and 37 no-GC observations respectively. The four
runs used the same Gerbil 0.19 / Gambit
`dcd677cd3e40860bdd27dfbdbf5e3ce46ab03813` toolchain, optimized native module
build, input and harness; compiler optimization settings were unchanged.
The 1,632-byte reduction is about 3.1% of lexical allocation and 0.8% of
full-source allocation on this input. Prepared LR and publication serve as
unchanged stage controls. This result measures allocation, not retained heap
or a latency improvement; host contention prevented a wall-time conclusion,
and the 150ms admission gate remains unchanged.

After rebasing onto `df43581dba835a2b044056a39e6b7c96ab83ea50`, a fresh
original/candidate pair with the same 40 x 100 configuration reproduced every
allocation median in the first two rows, including both unchanged controls.
This pair preserves the newer scanner index and native publication changes.

Regression coverage exercises 1–4-byte scalars, NUL and combining marks,
Unicode checkpoint restoration, contextual scanning, Bash, incremental edits
and matched GQL artifacts. Reproduce the comparison with the matched-stage
command above, rebuilding each implementation before its fresh-process run.


## Borrowing the immutable stack cell for unary LR reductions

The private `pop-reduction` helper used to allocate a one-element source-value
list when the production width was one. Both consumers (`reduce-value` and
`reduce-value/events`) stop at the production's RHS width, so this case can
borrow the immutable semantic stack's first cell. The remaining semantic and
state suffixes are unchanged. Zero-width reductions retain their empty value
list; wider reductions still construct the required source-order list.

Against `8535fb4ba5478e6786b06aec7d4a4efec796c888`, the same representative
138-byte input, toolchain, heap and 40 x 100 configuration used above produced
these median bytes per parse in no-GC batches:

| Implementation | Global lexing | Prepared LR | Artifact publication | Full source |
| --- | ---: | ---: | ---: | ---: |
| Original | 50,724 | 111,140 | 44,324 | 201,708.32 |
| Borrow unary cell | 50,724 | 100,916 | 44,324 | 191,484.32 |
| Original restored | 50,724 | 111,140 | 44,324 | 201,708.32 |
| Borrow unary cell repeated | 50,724 | 100,916 | 44,324 | 191,484.32 |

Each row retains 40, 39, 40 and 37 no-GC observations respectively. This is
10,224 fewer bytes per parse: about 9.2% of prepared LR allocation and 5.1%
of full-source allocation. Lexing and publication are unchanged controls.
The native preprocessed C products confirm removal of the pair allocation
in the unary branch; compiler settings and the three-value return protocol
remain unchanged. The measured byte saving is specific to this input, execution
path and toolchain, not a portable per-pair size or a GC/latency guarantee.

`GQL-REDUCTION-COUNTS` now also partitions the selected derivation into
`emptyReductions`, `unaryReductions` and `multiOperandReductions`. Their sum
is `reductions`. These untimed counts describe committed syntax, not every
executed reduction in failed/speculative branches; do not multiply them by
an assumed object size to infer runtime allocation.

`t/lr-empty-action-test.ss` exercises nested unary field/alias reductions
above retained left siblings, both semantic backends, Unicode, source order
and incremental edits against independent full replay. Existing empty and
multi-operand cases remain covered. Host contention still prevents a latency
conclusion; the existing admission deadlines remain unchanged.

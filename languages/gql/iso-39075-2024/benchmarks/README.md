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
wall time. This directly identifies GC as the dominant cost in that sample.
The isolated LR control has the largest CPU/allocation cost among the three
prepared controls, making recognition allocation the next profiling target.
It does not establish an additive LR share of the streamed public entry.
The maximum full-source sample was 95.279ms, with 58.364ms CPU and one
41.271ms GC: it also includes elapsed time without CPU consumption. These
are current local receipts, not universal latency guarantees or an explanation
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

# Real Fold cross-statement qualification

Status: whole-engine preflight incomplete; retain local buffer/wire candidate,
reject slower scan/publication extensions. No promotion, push or full matrix.

## Source checkpoint requested on 2026-10-13

The user requested committing the current task changes before continuing.
This checkpoint retains source, controls, distinct Lean/Quint obligations and
negative benchmark records; it is not throughput acceptance. Generated SDKs,
binaries and temporary logs remain local and are not staged. The existing
GitHub PR #13 head is 23b1b493135881b3e4c68b68a8e2ba376c32a721;
the checkpoint starts from 16f539901aeaf231ad65f0236a6c2b60283d5f4e.
Upstream module-admission work must be preserved during alignment. The
1000-request 10 ms gate remains open; no full matrix or Automation restart.

Checkpoint 450c27e44845ad9fa477b491fff2bf936fcd1128 is rebased on that
GitHub head. The sole Justfile conflict retained both independent recipes.
Post-rebase checks passed: 13 run controls, 8 tape/buffer controls, 6 single-use
call controls, and 24 retained actual Org AOT controls. The latter reruns the
qualified retained binary; it does not qualify a newly regenerated upstream
module-loading closure. The fresh model collector passed Lean and Quint:
Lean faa7c2ec17f4844c986094f4f84a244115a857f14967a12d792a9f03cf4a1050;
Quint dd6abc197b27fa3bd83d3f1ad13551cef82c32556b9c6996334c6c94c2b44ba3.
Model-only collection leaves Scheme/performance joins Missing and execution
unauthorized, as expected. Initial cache permission and missing Ascent library
environment failures were setup failures, not passing model runs.

Continuation closes the observed diagnostic-link workflow gap: attribution,
stall and allocation link recipes now use the existing static object-lock
precheck, as prepared preflight already does. The existing lock rejection and
evidence-preservation test passes. This is fail-fast detection, not an atomic
cross-process lock or a throughput improvement. Links must still run only
after their owned compilation has completed; no SDK locks are removed.

## Whole-span recognition/state/consumer fusion trial

An architectural span candidate replaces the generated per-byte Scheme loop
with one synchronous borrowed-buffer leaf. Each source byte is read once for
stop classification and all isolated reset/increment consumers. The leaf
produces trailing lengths; Scheme applies carry to the unbounded initial value,
then executes the original ordered last-byte residual and event consumption.
No callback, allocation or pointer retention occurs inside the leaf. The
compiler's existing effect/dependency admission is unchanged; this is NOT
permission to skip arbitrary triggers or cross consumer/helper/frame effects.

First product `.real-span-leaf` used the checked general traversal entry and
one local scratch vector per loop, including empty counter sets. Unit controls
(13 cases), 24 actual complete tapes and source-bound qualification passed:
Lean fdd4e8de2eca6233347a1f42dffce665b56322280e395c520b99bf4f1f56caf8;
Quint d5e43e5f69f3defcadad16553e81f3fa9c44997c8b2329a31ff84f1b607dcd17;
Scheme/AOT 37e36e26535bb1250d58f9491262b34858e5e725f2c107f7c84fd093728950b3.
Lean proves the finite reset/carry summary; Quint separately models synchronous
borrow, epoch validity, commit and residual order. Neither proves C storage or
compiler eligibility; real AOT controls bind those implementation obligations.

Actual SHA a421efa46b073d4bab4ba10accf1fd151ebfe62d437bfef62e9e251ec9e3ad77;
prepared SHA ba135fea6395b333e3fe81a6c2ce5baff025d2e7c2c35385f4f109891c421fc8.
The corpus and retained publication control remain byte-identical.
Four matched ABBA lanes/product, complete 1000 changing tapes each, gate exit70:
candidate wall37.815/67.181/37.052/36.544 ms; CPU37.711/37.216/36.916/36.533 ms;
control wall36.636/50.843/36.016/35.916 ms; CPU36.615/37.608/35.974/35.878 ms.
Candidate wall median37.4335 ms, CPU37.066 ms, RSS26664960 bytes;
control wall36.326 ms, CPU36.2945 ms, RSS26574848 bytes.
Candidate P95 52-56 us, P99 66-121 us, throughput14885.16-27364.27 requests/s;
control P95 53-55 us, P99 71-252 us. All lanes have four GCs. The candidate
regresses CPU/RSS and is NOT admitted. Its67.181 ms lane has29.965 ms beyond
process CPU; that difference alone cannot locate the pause or classify GC.

`.span-leaf-stalls-1.log` is diagnostic only: wall40516 us, CPU40496 us,
complete comparisons1112 us. A captured447 us call has412.226 us GC wall;
the800 us first call has802 us CPU and no GC. This run does NOT reproduce
or retrospectively attribute the earlier46.798/67.181 ms slow batches.

Initial test failure was an unbound library name in the isolated test eval,
fixed by explicit lexical injection (no production eval introduced). Initial
prepared link correctly rejected an active object lock; initial real-control
launch preceded binary completion and failed before parsing. All failures
remain in `.span-leaf-*.log`; their finished retries, not failures, qualify.

Second product `.real-span-entry` removes repeated direct-call admission from
compiler-proven intervals and removes scratch allocation for zero-counter
spans. The checked general library entry remains. Installed std/ffi vector
body macros and Gambit's existing U64 storage macros are used; no kernel/SDK
changes. Thirteen unit and 24 complete actual controls pass. Source-bound
collector session80053 and prepared linker session41191 are active at this
receipt point; do not duplicate them or edit bound sources before completion.
No second-product performance has yet been admitted. No full20-batch matrix,
baseline promotion, commit, push, Automation, downstream or SMP admission.

### Completion and withdrawal of both foreign span candidates

All sessions listed above have ended. Second-product source-bound chain passed:
Lean89ac43249a0ef1d7b0c5ae3faba367544c500a34068480119b389468dd705d12;
Quinta128d4353df5aa5b1ed122bf052edd123cf292b574963440b97cd9f7d8791d35;
Scheme/AOTcf573f72b61fc8114947fd928dba778f56e272ac41768ad5ba8ab2f80dbf4b0c.
Actuald966ac143a841a6dfd7453cba462541df62710cfee91a50075bc180bfd4ab96d;
prepared0149b06a61627cde14cbaf86f98a4f2238cc5bec23a34375b2ff3a7145b25a48.
Four matched complete1000-tape ABBA lanes/product, all gate exit70:
candidate walls37.148/36.514/36.934/77.678 ms; CPU37.074/36.464/36.823/36.559 ms;
control walls36.987/35.617/35.986/36.252 ms; CPU36.118/35.555/35.899/36.103 ms.
Candidate medians wall37.041 ms, CPU36.691 ms, RSS26648576 bytes;
control wall36.119 ms, CPU36.001 ms, RSS26566656 bytes. Candidate P9552-56 us,
P9989-116 us, throughput12873.66-27386.76 requests/s. All eight lanes have4 GCs.
The77.678 ms lane has41.119 ms beyond process CPU; this is NOT a root-cause
classification or permission to discard the lane. No throughput admission.

Both new scanner/compiler-runtime entries and their production imports were
removed with narrow reverse patches. Existing single-use call fusion and owned
tape publication remain; source byte-spans returns to its original implementation.
Retained scalar controls now include a counter above2^64 and nonzero carry.
The temporary duplicate Lean counter refinement was replaced by a genuine
representation obligation built on the existing Fold model. Its first empty
case proof failed and remains negative evidence in.span-encoding-models.log;
the corrected proof has no sorry/custom axioms and passes.

Diagnostic-only instrumentation extends the existing request-owned count session,
not a new runtime/benchmark framework. It emits grouped source visits, isolated
counter byte visits, spans/buckets and fallback byte transitions; all hooks are
absent when diagnostics is disabled. Existing two cost tests include weighted
increments and rejection of negative increments. Compiler/zero controls pass.
Fresh.real-span-census actual24 controls and the independent1000 complete-tape
count census pass; output events1017619 is WIRE BYTES, not an event count.
Diagnostic CPU253.95 ms is instrumented and NOT a performance observation.
Observed source visits326949, summary byte visits323278, spans45996,
fallback byte transitions71640. Buckets1/2-4/5-16/>16 =6335/11332/25995/2334.
Mean7.108 bytes/span;38.410% spans<=4,5.074%>16. Visits can overlap across
owners and are not unique coverage of the2856770 raw input bytes. Static
.span-census-static-invariants.log has285 loop occurrences,33 admitted;
all reported required inline guard cells may be written within their loop.
Hoisting them unchanged would be unsound. The dependency-write boundaries,
not a blanket faster byte scanner, are the next execution-region obligation.
Installed SSAX look-for-str.scm was consulted for sequential recognition and
partial-match overlap handling; its port API was NOT substituted for byte Fold.

The final retained-source model/AOT join passes:
Leanfaa7c2ec17f4844c986094f4f84a244115a857f14967a12d792a9f03cf4a1050;
Quintc1552051fa4f127d2df8054daf9403df42ed22bb3bc80dbb692f902ffdf1348a;
Scheme/AOT20e0922dee1af8ab72d6c4d9a35d13c990e33cf1bf8ec741b151bd97fe720882.
Matched-performance remains Missing; executionAuthorized=false. No current
artifact meets1000/10ms. No full3x20 matrix, promotion, commit, push, kernel/SDK
change, Automation restart or Org/FFI/Tokio/Python/SMP speed claim.
Diagnostic build/link sessions12400/34662, census52823 and collector20633 all
ended; no active matrix exists. An early census control launch failed before
binary completion, then the completed-binary retry passed. Preserve both logs.

## 2026-10-13 reset/increment run summary rejection and live stall attribution

### Generated call-region fusion: real implementation, preflight still open

#### Follow-up: exclusive event-tape publication (work in progress)

Production fold-tape-publish! now follows installed std/io/bio/api.ss
get-memory-output-u8vector: seal owner, clear its byte slot, shrink the privately
allocated u8vector to used length, and transfer it rather than subu8vector-copy
the complete tape. No input/header alias or foreign pointer escapes; all
sealed writers still reject before accessing the cleared slot. This removes
one publication allocation/copy, not the owner or capacity-growth allocations.
Shrinking logical length is NOT proof of immediate physical RSS reclamation.
Existing eight buffer/tape controls include post-growth, zero-size, explicit
GC, retained bytes and sealed reentry checks; they pass. Actual fresh AOT has
24 complete tape controls passing. An initial wrong controls pathname failed
before execution; .owned-publication-real-controls.log preserves that error,
and the valid retry is .owned-publication-real-controls-retry.log.

Lean owned_publication_prefix models observable prefix equality, not allocation
or primitive correctness. Quint TapeOwner separately owns seal/transfer/reentry
lifecycle, with four tests, finite one-step closure and bounded histories.
First Lean simplification attempt failed; corrected List.take_take proof
builds successfully without sorry. No failed attempt is admitted as passing.
Source-bound qualification runs as session45192; sequential prepared/stalls/
allocation diagnostic links run as session93341. Do not duplicate these jobs
or modify bound code during qualification. New product .real-owned-publication;
staged immutable evidence .owned-publication-real-evidence. No timing/large-load
or allocation improvement qualified yet; prior published baseline unchanged.

Publication-stage completion: sessions45192/93341/11489/60824 ended. Model/AOT
join passed: Lean372f63ab693316b01db28f9cfb788288410a7c98c25b3367275331e7c6303b6c;
Quintda0863009441f32fa1920ba2c43c01dd85a60c14954c9a987d068a9e4186c357;
Scheme/AOT60124fda8e7ce187dc63ce8aa5ef45dd894ecd5962e4ae1c54c373df9973e557.
Actual org-fold SHA1b753e93b00f4f08214d5430b1a0d68a98418f4b5c6c101283bf3e86053479dd;
prepared953ae85f308f8471a4d9f7ee09c1a2ec185d9966c31ee0a446d9b3e0283325f0.
These identities qualify that stage, NOT subsequent single-read source edits.

Allocation subset bytes/request by residue0/1/2 is14608/23456/28528 versus
15904/24976/31888; denominators332/332/331 versus332/331/332. Reduction is
1296/1520/3360 bytes per observed no-GC request, not whole-process allocation.
Logs .owned-publication-allocation.log/.owned-publication-control-allocation.log.
Eight interleaved1000-request lanes per product retain every result: walls
candidate38.524/35.534/35.642/36.220/35.549/36.259/35.755/35.268 ms;
control38.058/36.191/39.826/36.583/38.761/73.321/36.322/36.057 ms.
Median35.6985 versus37.3205 ms; CPU35.6645 versus36.279 ms. Candidate P95
50-56 us; P9960-133 us; throughput25957.84-28354.32 requests/s. All complete
1000 tapes pass; GC4 versus5; all unchanged10ms gates fail. First eight lanes
have sysctl-denied RSS and time exit1, not complete resource qualification;
later eight escalated lanes have parser gate exit70 and RSS. RSS medians
26583040 versus26558464 bytes (candidate slightly higher), not an RSS saving.
External compiler activity makes peak throughput and tail admission conditional.
Retain candidate1 wall/P99 regression and control6 wall73.321 ms/P99411 us.
No full20, publication/push or claimed stable architecture-wide speed admission.

Current AOT stalls diagnostic: wall133.837 ms versusCPU46.434 ms with probes.
Slowest call monotonic8157 us, CPU319 us, zeroGC and3 involuntary switches;
other multi-ms rows also show switches withoutGC. Concurrent external compiler
jobs were observed. This supports non-CPU waiting for those calls, NOT a
retrospective explanation of the historic46.798 ms batch. Profile sample
.owned-publication-stack.sample has top-frame counts part14=66, fold-tape=54,
part17=24, source-lines=11. Module samples are NOT exclusive function costs,
dynamic scan counts or parser-only percentages; complete tape comparison remains
in the profile loop. .owned-publication-stack-profile.log confirms complete tapes.

Follow-up single-read production lowering now binds each admitted next byte
once for membership recognition and all counter consumers. No read at limit,
no cache and no intervening consumer/source writes. The existing counter control
also checks one generated next-byte source load for two summaries. Current
tests/AOT/prepared link run as session92630; no large matrix active. Do not
duplicate or promote that product before actual AOT/model and matched checks.
NUL/empty/exact-end scalar-reference cases now pass inside the13-case run
suite; all24actual single-read tapes pass. Immutable copied AOT is staged at
.single-read-real-evidence. The second source-bound chain is active separately;
do not confuse earlier publication identities with this source generation.

Single-read final result: source-bound Leanb64a4528fe3f7b07ca91b892bf421911a41ae3b997136af1e64e81b633dc1d30,
Quintbcb115adc394796bcc7572785b0bb569041f6132ef2bcf6dc78b6b73fc68285e,
Scheme/AOT04c51ba78cbca8cd7bc254871d9da59ce67dbf791154f93923e2f50fee4b1ead
pass, but performance fails. Candidate walls37.200/36.401/36.751/36.410 ms;
publication-only controls38.752/36.636/35.166/35.982 ms. CPU medians36.5015
versus36.2355 ms; P50 consistently30 versus29 us. Candidate P9551-53 us,
P9970-83 us, throughput26881.72-27471.77 requests/s, RSS26574848-26607616B;
control RSS26460160-26574848B. All8lanes full1000tapes pass, GC4, gateexit70.
Actual SHA b5a93216c1b16246d711aa7873606ec8d18fda704a7aebdb44228cc244bdc76b;
prepared70be3ec5f238bfab48d51ec0365592a8b574385710438a7f3c87dd159e410b6a.
The new let/guard/source-read lowering was withdrawn with a narrow reverse
patch; independent scalar NUL/empty/end tests are retained. No unrelated dirty
edits reverted. Publication-only runtime remains an unpromoted candidate.
Current final-source model/AOT requalification session27278 uses a fresh
.publication-retained-evidence-controls root and the unchanged real publication
artifact. Do not restart it or claim old source identities cover this rollback.
Next architectural candidate must collapse an admitted entire pure source span,
not introduce another per-byte binding/guard. Counter suffix/initial-state
semantics and ordered residual/event effects must remain explicit obligations.

Final retained-source qualification session27278 completed exit0 under the
unchanged240s budget: Leana20290bd26c1a78d545c08eef2b13d7ae2a0dfef9f9bc6bb2043c5b027a5a2f9;
Quint0f934b1cbb01db6962f6220da494ef6bfe30fc9a45a35b04c4731e868a03608c;
Scheme/AOT1de91e7e76708a2b2702379a39cee39deffc2530baa0e86bbaf74a0570dca8b5.
Review still lacks matched-performance and executionAuthorized=false. All owned
compiler/model/benchmark/OS-sampling processes ended. No matrix active, no push,
baseline promotion or Automation restart. Live GitHub PR13 head verified still
16f539901aeaf231ad65f0236a6c2b60283d5f4e (open). Published prepared baseline
a94f3fb148469e3f3f04bffc89bec90be66d65efcdd32664a8787e844ffa17c0 and
immutable corpus e9ccee6c366bdc07f733c525634ed9efed2eb1d2932ef93b54f5ba4cd820df40
remain unchanged; git diff --check passes. Runtime publication benefit remains
diagnostic/unpromoted and historical46.798ms root cause is NOT established.


Head remains 16f539901aeaf231ad65f0236a6c2b60283d5f4e; fixed installed SDK
1cfb032, unchanged corpus/input indices/independent full tape oracle.
New event-fold-call-plan.ss counts whole-program private references and fuses
only single-use expression calls with scalar-reference arguments and bounded
expanded bodies. Shared consumers, helper frames and effectful argument calls
retain their boundary. Alpha conversion preserves let/let*/named-loop scope;
final reachability protects children after rejected speculative expansions.
The pass is integrated before AOT module partitioning, on production tapes
only. It introduces no runtime cache, duplicate consumer or event format.

An initial real-generation failure exposed Gerbil single-binding let syntax.
Retained .single-use-aot-build.log and incomplete .real-single-use; fixed the
converter and added this syntax to the scope control. Six directed controls
and 24 complete real AOT tapes now pass on fresh .real-single-use-v2.
Initial compile/Lean syntax failures remain recorded; none counted as passes.

Current static census: 3310->1300 definitions; 7381->5371 private calls;
27210->18920 private argument slots; 70->25 event owner assignments.
Counts are syntactic, not dynamic scans, allocations or CPU shares.

Source-bound complete qualification passed within unchanged 240 s budget:
Lean 140046f1d390704266b2a069740736f03b580015139290e3422a6fec04166696;
Quint c628def5c3b8c03d1ba0ca971957b34ab4e213f843f896061993795abf5271a3;
Scheme/AOT 822ef6711f124d8a2ce6e5ebc39a4801e74b464484433fb6c5bc0dc65825be14.
Lean splice_call_preserves_order proves ordered region composition under its
model, not the alpha converter. Quint fused-call history owns extent
invalidation. No custom axioms or speed-proof claim. Model review still has
matched-performance missing and executionAuthorized=false.

Actual AOT SHA256 685fcbf700d04bf684b91c7f811501d003a0d1f7e0b7e122227ad220e4721687;
prepared e39d5ad4448d123f4047413649b2e41d0ffd75d1e548490803c76124d7893d29.

| Lane | Wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| control1 | 44.609 | 64 | 80 | 22417.00 | 44.095 | 28803072 |
| candidate1 | 37.430 | 55 | 76 | 26716.54 | 37.369 | 26525696 |
| candidate2 | 46.798 | 51 | 156 | 21368.43 | 36.389 | 26558464 |
| control2 | 43.357 | 64 | 82 | 23064.33 | 43.322 | 28786688 |
| candidate3 | 36.032 | 54 | 63 | 27753.11 | 36.022 | 26525696 |
| control3 | 43.264 | 63 | 93 | 23113.91 | 43.222 | 28786688 |
| control4 | 44.095 | 65 | 116 | 22678.31 | 43.908 | 28803072 |
| candidate4 | 36.307 | 53 | 69 | 27542.90 | 36.289 | 26542080 |
| candidate5 | 37.025 | 55 | 101 | 27008.78 | 36.852 | 26558464 |
| control5 | 44.349 | 65 | 95 | 22548.42 | 44.182 | 28835840 |
| control6 | 43.352 | 61 | 96 | 23066.99 | 43.295 | 28803072 |
| candidate6 | 36.212 | 53 | 83 | 27615.16 | 36.145 | 26574848 |

ABBA/BAAB followed by an additional BAAB diagnostic: six lanes per product,
1000 changing requests each, all full comparisons pass, all gate exits70.
These are preflight diagnostics, NOT twenty-batch acceptance or large loads.
All six candidate CPU times improve against controls; six-lane median CPU
36.339 vs43.615 ms. Wall medians36.666 vs43.726 ms retain the 46.798 ms
candidate2 spike (CPU36.389 ms; P99156 us). No retrospective attribution of
that ~10.4 ms non-CPU gap. Additional reverse controls do not replace it.
Five collections each; no GC-count improvement or allocation-total claim.
RSS medians26550272 vs28803072 bytes include retained inputs/oracles/code.
Call percentiles and comparison-inclusive batch wall remain distinct; this
is not public Org::parse, FFI graph projection, Rust queues, Python or SMP.
Another session's parser-quality evidence process was observed during earlier
qualification; owned compiler jobs had ended before timing. Do not claim an
isolated-host capacity limit from these diagnostics.

Retain this as an unpromoted implementation candidate: broad CPU/RSS benefit
observed, but the hard10ms preflight fails and wall/tail regression is retained.
Published baseline hash unchanged; no push, commit or full20 matrix. Do not
label scan-count or allocation reduction established from call/RSS counts.
Serial allocation diagnostics against both existing AOT closures completed
with complete-tape agreement; all owned build/model/benchmark jobs have ended.
The no-GC-request subset has identical observed bytes per request:
fixture residues0/1/2:15904/24976/31888 bytes for candidate and control.
Candidate denominators332/331/332; control332/332/332. Collections by residue
are2/2/1 versus2/1/1; CPU sums8.171/11.158/17.941 ms versus
9.935/12.897/21.082 ms. These diagnostics include observation overhead and
exclude requests crossing GC; they do not establish total or parser-exclusive
allocation, lower GC pressure, or performance acceptance. Logs:
.single-use-allocation.log and .single-use-control-allocation.log.
The current benefit is call-structure/CPU/code-RSS, not demonstrated allocation
reduction; direct event allocation remains an explicit open cost.
No kernel/SDK change, acceptance relaxation or Automation restart.

Next coherent step is lexical dominance of immutable recognition facts across
dependent consumers, now inside fused regions, plus direct-consumer allocation
cost. Do not reintroduce an all-idle guard or maintained request cache.


### Whole-engine model and rejected local-transition trial

The optimization unit is now the complete parser engine, not Fold-local guards.
formal/ParserEngine/Execution.lean models ordered dependent consumers sharing
immutable recognition facts and invariant-preserving ordered transitions.
The existing FoldPlan.qnt owns lifecycle/invalidation, not a second semantics
proof. Lean build passes (40 jobs); FoldPlan has eleven passing cases and the
existing bounded Quint checks pass. Compiler refinement and whole-request
dynamic cost attribution remain open; these are not speed or GC proofs.

Local completion-support trial: 13 run controls and 24 complete real AOT tapes
passed. Main mode guards and 33/285 admitted loops did not change. Generated
definitions remain 3310; private call sites 7374->7371; owner assignments
69->70. Static counts cannot establish execution cost reduction.

Source-bound trial receipts (historical after withdrawal):
Lean 7551302d363bbdc86c8870059c405fa97673cd025dedcd4de392f7da5a25b50b;
Quint bf9a111d595868549c3f5e69a9e9d54d2dba844978c650f0dd7f41ead4483434;
Scheme/AOT e564c5d3ef7be0fcbc3fe17c9c6bd1ca7dac2a10a2e3ce31a4aec37c65b54c06.
Trial AOT 1ad8c00c117674f708eac86424e084daedca6024065a667491119dc092e66963;
prepared 71ba046e73d315b1d974a8bc9321cec631ef3360da56a780bfa38253cdbcd223.
All retained under .real-local-transition and .local-transition-*.
No timeout widening, baseline promotion, commit or push.

| Lane | Wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| control1 | 43.803 | 59 | 121 | 22829.49 | 43.286 | 28819456 |
| candidate1 | 43.344 | 63 | 119 | 23071.24 | 43.179 | 28803072 |
| candidate2 | 46.836 | 67 | 129 | 21351.10 | 44.568 | 28868608 |
| control2 | 43.244 | 62 | 97 | 23124.60 | 43.189 | 28819456 |
| candidate3 | 42.116 | 63 | 84 | 23743.95 | 42.046 | 28803072 |
| control3 | 55.164 | 60 | 85 | 18127.76 | 42.611 | 28835840 |
| control4 | 43.562 | 64 | 95 | 22955.79 | 43.503 | 28819456 |
| candidate4 | 42.716 | 63 | 89 | 23410.43 | 42.642 | 28803072 |

Serial ABBA/BAAB: all eight lanes completed 1000 full independent comparisons,
all exits 70, five GCs each. Wall medians 43.030 ms candidate vs 43.683 ms
control do not qualify a consistent gain: candidate2 regressed; control3 has
55.164 ms wall vs 42.611 ms CPU. The non-CPU gap is observed, not attributed
to a specific scheduler/GC cause. No 10 ms admission or full twenty-batch matrix.
Call P95/P99 and comparison-inclusive batch wall are distinct; RSS includes
retained input/oracle/code, not Rust capacity. No FFI/public Org/Python/SMP claim.

Withdrawn only this turn's local-support admission/lowering policy. The prior
selective-table implementation and immutable baseline remain unchanged. The
source-only invariant checker and directed negative controls remain tooling.
Restoration session69320 completed exit0: fresh compilation, existing source
controls and all 13 run controls have final OK. All owned jobs ended.
Baseline prepared SHA256 remains
a94f3fb148469e3f3f04bffc89bec90be66d65efcdd32664a8787e844ffa17c0;
corpus SHA256 remains
e9ccee6c366bdc07f733c525634ed9efed2eb1d2932ef93b54f5ba4cd820df40.
No kernel/SDK/Automation changes. Trial source-bound receipts remain historical,
not qualification of the restored lowering. git diff --check passes.

Next architecture target: jointly plan source recognition facts and consumer
dependency frontiers across frame/helper boundaries, preserving original
state/event effect order. Then reduce generated call/return boundaries and
publication work, not another all-idle guard or maintained runtime cache.


### Continuation: dependency-selected interval execution

Final qualification on head 16f539901aeaf231ad65f0236a6c2b60283d5f4e:
the fresh aligned retry passed the unchanged 240 s budget. Eleven run controls
and 24 complete real AOT tape controls pass. Source-bound receipts are Lean
527fbd50fe56b58580c7588f9198029f6e16515b46db69c041e4acbc5e421f6f,
Quint bfbed6809deed73f6d12dc61aab981aa37b2e8590319097269fb60c096a84a70,
and Scheme/AOT a1b6649ca18b0347a653625dfa8a68e2673c672d5ad4f890c697a3bf1c70d29f.
The earlier timeout below remains a failure, not converted to a pass.

The interval-selected narrow/wide candidate has AOT SHA256
884f7c9f7ca657393c0667fa8978289af7b3c5f3a5d2db1ed481cd1d8a6b0edc
and prepared-binary SHA256
98f8ca51ecb0fd7d31628b1340beb708b83212aed7a80cbfae06b0579981f9ec.
The unchanged baseline prepared binary is
a94f3fb148469e3f3f04bffc89bec90be66d65efcdd32664a8787e844ffa17c0;
input/oracle SHA256 is
e9ccee6c366bdc07f733c525634ed9efed2eb1d2932ef93b54f5ba4cd820df40.
All timing lanes use the same installed 1cfb032 SDK, not another session's
2591dcd build. No SDK rebuild, kernel change or Automation restart occurred.

| Lane | Wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Control 1 | 57.987 | 154 | 275 | 17245.24 | 54.903 | 28770304 |
| Candidate 1 | 43.931 | 62 | 109 | 22762.97 | 43.781 | 28770304 |
| Candidate 2 | 43.262 | 63 | 89 | 23114.97 | 43.220 | 28770304 |
| Control 2 | 42.219 | 59 | 87 | 23686.02 | 42.112 | 28753920 |
| Candidate 3 | 41.011 | 58 | 63 | 24383.70 | 41.015 | 28803072 |
| Control 3 | 42.779 | 62 | 73 | 23375.96 | 42.733 | 28753920 |
| Control 4 | 42.382 | 59 | 65 | 23594.92 | 42.388 | 28803072 |
| Candidate 4 | 41.871 | 58 | 91 | 23882.88 | 41.856 | 28786688 |

Sequential ABBA/BAAB lanes each compare all 1000 changing inputs against the
independent complete tape oracle. All comparisons pass; all preflight exits
are 70 and each lane has five collections. The first control spike is retained.
Four-lane wall medians are 42.5665 ms candidate and 42.5805 ms control: no stable
speed improvement is established. No promotion, commit, push or full twenty-
batch matrix. The 1000-request <=10 ms gate remains failed. All owned processes
have ended. This candidate replaces the slower unconditional wide policy but
remains unqualified; published baseline artifacts are unchanged.

P95/P99 are compiled Fold-call durations; wall includes call timing/sample
recording and full semantic comparison. RSS is whole-process retained inputs,
oracles and native code, not Rust reserved capacity. This is not public
Org::parse, Python, Tokio or FFI projection acceptance. Counter deltas across GC
are not allocation totals. Unsupported SMP remains open/skipped, never passed.
The next algorithmic gap is dependent active consumer transitions: derive
local completion/transition support and safe zero-preserving reset witnesses,
instead of requiring whole-mode inactivity. Do not repeat the rejected wide
policy or infer speed from static guard counts; no maintained runtime cache.

The new candidate admits ordinary-byte intervals from the complete consumer
body. A consumer selector is stable only when its read support is disjoint
from every interval write and exact counter target, it has no observable
effects, and it does not depend on the changing cursor/current byte. Leaf
actions are total state-independent overwrites; isolated unconditional exact
counters retain their existing summary. Source/order boundaries and event
actions reject interval admission. Conditional counters are not moved into
unconditional accumulators: their ordinary reset remains under its selector.

For consumers that still require an idle premise, the planner removes one
support at a time only with a complete admitted transfer witness for the new
body. This is not equality with the all-idle body: independent active consumers
may execute their ordered overwrite once at the last ordinary byte. Required
premises must remain disjoint from all common writes. There is one scanner,
not a source pass per consumer, no maintained runtime mode/scan cache, and the
first triggering byte remains unconsumed for the original complete body.

Lean selective_run_last owns conditional-transfer equivalence under untouched
context. Quint RunPlan adds activeConsumersCanCommitTest to the existing
commit/residual lifecycle; ConsumerScope retains dependency invalidation.
These models do not prove the Scheme extractor or physical performance.
Standalone Lean and Quint pass. Actual AOT and performance remain pending;
fresh closure .selective-mixed-controls is being validated. No matrix or
baseline promotion is authorized by these structural results.

Mixed-state AOT 1b5cb5227e65fb936587590b7bf2c6cf490152b0b42619514ad73c9489d5f69b
and prepared binary 0622b17e8c2d30dd38e4c7371b69d995a709f39b11eed8b7476bcad32b679293
pass the 24 real tape controls. Source-bound receipts: Lean
bb2c67a7794241927125a54534a6ff6cd255360e1417e337d77561f77043e3f5;
Quint c6cb7c5a31bf197a8aee3424a82d5992a636f791ff5ed28fa2c37ae2cc6b47b4;
Scheme/AOT 3de53a6e9f803f298a105512a82125b64f5aed36a29231a5dec962db016858b4.
These are historical after subsequent classification-selection edits.

| Lane | Wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Control 1 | 45.051 | 60 | 109 | 22197.07 | 44.604 | 28770304 |
| Candidate 1 | 47.277 | 63 | 103 | 21151.93 | 47.161 | 28819456 |
| Candidate 2 | 48.004 | 65 | 123 | 20831.60 | 47.555 | 28868608 |
| Control 2 | 56.601 | 109 | 312 | 17667.53 | 54.177 | 28803072 |
| Candidate 3 | 49.267 | 63 | 211 | 20297.56 | 48.168 | 28819456 |
| Control 3 | 58.144 | 113 | 458 | 17198.68 | 53.361 | 28803072 |
| Control 4 | 44.004 | 60 | 80 | 22725.21 | 43.990 | 28770304 |
| Candidate 4 | 46.399 | 62 | 114 | 21552.19 | 46.256 | 28868608 |

All 1000 comparisons per lane pass; all preflights fail (70), five collections
each. Mixed-state plans reduce guards to 10/8/7 but broaden trigger alphabets
from 17/16 to 29/21. Stable endpoints show regression despite removing guard
premises; noisy controls are retained, not used to claim improvement. This
unconditional wide-alphabet policy is not promoted. The follow-on candidate
retains narrow and wide precompiled tables, selecting once at interval entry
from disjoint unchanged consumer support. Both tables share one scanner and
the same admitted wide transfer; no text/mode cache or additional source pass.
The selection support must be disjoint from every common write/counter target;
the narrow alphabet must be a proper subset of the wide alphabet. It is an
unqualified candidate, pending fresh native controls, AOT and matched timing.

GitHub PR #13 head advanced to 16f539901aeaf231ad65f0236a6c2b60283d5f4e.
The checkout fast-forwarded to that exact commit, not the local origin's later
unpublished head. Dirty edits were preserved; collect_models.py was backed up
as .pre-align-collect-models.py and its additional Fold inputs restored while
retaining upstream recognition/token inputs. The upstream diff does not alter
the Fold compiler, its directly imported runtime modules, or preflight code;
generated AOT imports do not use the changed LR recognition/token/reduction
path. Existing immutable Fold artifacts are therefore reused, but model/AOT
source binding is rerun fresh at .selective-tables-aligned-evidence-controls.
The pre-alignment source-bound receipts are not the new head's qualification.
The first aligned chain failed its unchanged 240 s conformance budget while
linking event-fold-controls, after all six source controls and eleven run
controls passed. Timeout receipt:
6d5fcbd7735835b5326dbd92c130449f57941a6a48a435dadd9ce8fbdccf1551.
Another session's parallel parser-quality build was active at timeout. This
supports possible compiler contention, not proof of a sole cause. The timeout
is retained as a failure; budget and acceptance are not relaxed. Owned timed-out
processes exited. A fresh complete retry must wait for compiler contention to
end; never benchmark under that build or count this receipt as passed.

### Continuation: consumer-local selector support, not an all-idle guard

The new private implementation adds fold-consumer-state-body to the existing
zero-state analysis and invokes it at actual for-line-bytes lowering. Its
initial fact set is empty. An executed unsigned selector's false-positive
branch, true zero-equality branch, or a literal zero write introduces a fact
for that consumer only. Unrelated writes preserve it; support writes, opaque
effects, helpers/bounds and lexical joins invalidate it conservatively. Branch
merges intersect facts. Each byte starts afresh; no runtime fact table/cache or
new all-machine guard is emitted. Ordinary-run admission remains separate.

Seven native analysis controls pass, including independent consumers, dependent
writes and effect barriers. All 24 independent actual Org complete tapes pass
in .consumer-state-native-controls.log. Static (NOT executed) coverage reports
285 loops, 4 refined bodies, and 714 -> 672 selector occurrences. This limited
coverage is not claimed to remove the entire heavily exercised state chain.
The installed SDK, original IR/corpus and immutable baseline remain unchanged.

Lean adds unsigned-selector and independent-support refinement; Quint's new
ConsumerScope checks support-local invalidation, barriers and next-byte scope
across five tests and 1000 seeded bounded traces. These are different duties,
not duplicated semantics or a full compiler/liveness proof. Both focused
checks pass. The full source-bound chain is currently running against staged
.consumer-state-real-evidence and the actual .real-consumer-state product;
no performance promotion, complete matrix, commit or push is authorized by
these model results alone. Logs/products have unique .consumer-state-* names.
The full chain then exits 0; Scheme/actual AOT receipt identity is
5b86d3642b33a6ba9150acc59c923d3f75b9ce5f8434df6e2756e4c4d256e15b.
That packet becomes historical upon subsequent source changes.

| Lane | Wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| control 1 | 43.883 | 62 | 95 | 22787.87 | 43.289 | 28786688 |
| candidate 1 | 43.509 | 64 | 86 | 22983.75 | 43.418 | 28803072 |
| candidate 2 | 43.223 | 64 | 95 | 23135.83 | 43.168 | 28770304 |
| control 2 | 42.596 | 61 | 86 | 23476.38 | 42.571 | 28819456 |
| candidate 3 | 58.390 | 154 | 276 | 17126.22 | 57.711 | 28819456 |
| control 3 | 42.553 | 62 | 88 | 23500.11 | 42.512 | 28819456 |
| control 4 | 43.424 | 62 | 95 | 23028.74 | 43.333 | 28819456 |
| candidate 4 | 43.858 | 64 | 106 | 22800.86 | 43.772 | 28770304 |

All eight strictly serial/awaited lanes compare 1000 complete tapes and fail
the unchanged gate (exit 70), with five GCs and equal raw allocation counters
(NOT allocated bytes). No major gain; one reverse lane regresses. Do not promote
this zero-only integration. Its actual product SHA is
acd8d77f47bfbf1bcc39b9c42c9e44eb6d959a7606b7eecb5eaef064e491689b;
prepared SHA 696f8a81db749a1a9504be888cddf418ce56a62137154ffea1e90972c09e854d.

The follow-on consumer-selector implementation tracks executed pure selector
truth, including positive/Boolean/composite selectors, against each predicate's
declared read support. Writes invalidate only intersecting support; unknown
effects discard all knowledge. A branch merge keeps common facts and loop/frame
handoffs are not recursively specialized under stale facts. No runtime table,
new admission guard or source-input cache is emitted. New native controls cover
positive/Boolean facts, independent/dependent writes, barriers, composite event
order and lexical/iteration scope. Fresh closure .consumer-support-controls and
actual product .real-consumer-support are the next unqualified owners.
Five new consumer-support native controls pass with exit 0. All 24 actual Org
complete tapes also pass. Static (not executed) selector count is 714 -> 669
over the same four refined bodies; generated helper definitions are 3308,
not an expansive per-byte specialization. Actual product SHA
da30c8c2c8be5b316b7885f71ab396e23f664a310864984ba5075b46bfbf0968.
The original IR remains d37b4b160c9d956a549e86486e0b590b0cbdbf2a273ccb16f683c100d3158c38.
Lean's disjoint-write theorem explicitly assumes sound extracted predicate
support; it does NOT prove the Scheme extractor from a source hash. Quint
checks eligibility scope/invalidation histories, not unsigned mathematics.
The source-bound chain is running at the unchanged 240-second limit; current
packet may not be reported passed before it exits. No production promotion.

The full chain subsequently exits 0: Lean e149b7b1521236f473ea82bb38bd763ed3cfc30147da284b7a993b6a3d144dea,
Quint c7c1effc4c6c5a2e01e136c4f14790026d92815473adbc821edfc0be013e1044,
Scheme/AOT 85f8bb55417535afbd9299474d9888f00e713c29fcdae7db8a77309c0c679748.
That staged .consumer-support-real-evidence packet is now historical because
the following support-minimization source is different. Strict serial results:

| Lane | Wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| control 1 | 42.220 | 59 | 72 | 23685.46 | 41.699 | 28786688 |
| candidate 1 | 42.123 | 59 | 106 | 23740.00 | 42.066 | 28868608 |
| candidate 2 | 41.967 | 60 | 87 | 23828.25 | 41.897 | 28852224 |
| control 2 | 41.218 | 59 | 86 | 24261.25 | 41.197 | 28835840 |
| candidate 3 | 41.664 | 58 | 65 | 24001.54 | 41.645 | 28835840 |
| control 3 | 42.205 | 61 | 93 | 23693.88 | 42.158 | 28803072 |
| control 4 | 41.906 | 61 | 93 | 23862.93 | 41.836 | 28786688 |
| candidate 4 | 40.876 | 58 | 72 | 24464.23 | 40.841 | 28868608 |

All lanes compare 1000 complete independent tapes, have five GCs and fail the
10 ms gate (exit 70). These essentially tied results do not qualify major
state-chain removal or a baseline promotion. Candidate prepared product SHA
1de980282f1c661e129e22b3560dba688ff17b754d8fa2b031085b8cc37de368.

The follow-on guard-support minimizer drops an external zero premise ONLY
when rerunning the conservative cofactor without it produces the identical
ordered IR body. Explicit guard witnesses are not minimized recursively. Eight
native zero/support controls pass, including elimination of a payload guard
inside an unreachable consumer. Unnecessary actual guard cells are:
timestamp-diary-opens, footnote-label-count and export-backend-count. Inline
ordinary admission drops from 16/12 to 13/10 state guards. This is a structural
guard-dependency finding, not a measured admission-hit or speed claim.

The earlier exact reset/increment summary is now tested with this minimal
guard support, not reinstated unchanged. Seven run controls pass; summaries
cannot be read/written by other common actions or guards, conditionally updated
or multiply declared. Local accumulators preserve exact counter state and
publish before the unconsumed triggering residual; no event reorder or runtime
cache is introduced. Static trigger alphabets become 17/16, with the unrelated
payload guards removed. Fresh closure .consumer-supported-runs-controls and
actual .real-consumer-supported-runs have completed correctness qualification,
but failed performance preflight. Eight zero-state, seven run, five consumer
and twenty-four actual AOT controls pass. Source-bound model receipts are Lean
ac8aac991e0171e8741486c3384930cf89a75c7638d3c409c0816c677641612a,
Quint d424baf94d853dcb4941eb6d0d27e84b3e1502f0514ef8139774d8d81adf46a6,
Scheme/AOT ddf93974b19a44d1cddf0c2961f6fcaff056dd33ee077b4d3b2fc6f1ae1880df.
These receipts bind that candidate, not subsequent edits.

Matched A/B/B/A and B/A/A/B, 1000 changing requests each:

| Lane | Wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Control 1 | 44.423 | 61 | 92 | 22510.86 | 43.796 | 28852224 |
| Candidate 1 | 44.692 | 64 | 171 | 22375.37 | 44.490 | 28934144 |
| Candidate 2 | 41.504 | 60 | 75 | 24094.06 | 41.489 | 28934144 |
| Control 2 | 42.465 | 61 | 94 | 23548.80 | 42.413 | 28819456 |
| Candidate 3 | 41.620 | 60 | 99 | 24026.91 | 41.532 | 28884992 |
| Control 3 | 43.107 | 63 | 84 | 23198.09 | 43.058 | 28835840 |
| Control 4 | 42.048 | 61 | 76 | 23782.34 | 42.018 | 28803072 |
| Candidate 4 | 42.096 | 61 | 96 | 23755.23 | 42.022 | 28884992 |

All lanes compare every independent OEV1 tape and exit 70: the 10 ms gate
fails. Five GC collections per lane; the raw allocation-counter difference
is not allocated-byte evidence. No full twenty-batch matrix or promotion.
Actual artifact SHA256: 368784c6dd09926baf3da40a2e654bb18ada288ae5686a6a32256d856cceaf24.
Prepared artifact SHA256: f8676ebd8c95e6f332129f96549f888b16a510fd6a467478c5a6f03fc5928a51.


PR 13 remains b5e1895c8bc9a79dcb3d4e845130cdc113b7d2be (verified on GitHub).
No SDK/kernel changes, baseline overwrite, push or automation restart.

The next unqualified experiment is .real-consumer-scalar, compiled from
.consumer-scalar-controls. Scalar operator trees remain directly in their
consumer block instead of receiving an artificial helper boundary. Unknown
calls, scans, allocation and statement bodies remain outlined; evaluation
order is unchanged. Five calling-convention controls pass. Generated helper
definitions decrease from 3308 to 3256; this is a static call-boundary count,
not proof that primitive evaluations or end-to-end latency decreased. Actual
AOT linking, twenty-four complete-tape controls and the full source-bound
model chain completed. Lean: 758705c205df2f9d6fb8d4b237a584b7eedb1ac442bf4525cc7dc29c01d72492;
Quint: 099be0856ab57f4449fa89fe95c0809ed1fe3c59b9edde1701628d2522ee9541;
Scheme/AOT: 4855cc0e4c5a1fec3ab32387de00188a5d501cf4714081fefcd982895c78563d.
Actual binary: e6571cbfbbba3fa685faca9cb143434d23ed8e2e97ecc3dbe886afe475d7a376;
prepared: a79a198dc038973e18fd48110dad1d00a15e0945c7164413635bc242064c123d.

| Lane | Wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Control 1 | 67.527 | 154 | 415 | 14808.89 | 55.885 | 28819456 |
| Candidate 1 | 90.921 | 140 | 376 | 10998.56 | 49.338 | 30916608 |
| Candidate 2 | 128.127 | 140 | 283 | 7804.76 | 52.344 | 30883840 |
| Control 2 | 43.036 | 63 | 77 | 23236.36 | 42.945 | 28753920 |
| Candidate 3 | 39.846 | 57 | 87 | 25096.62 | 39.780 | 30900224 |
| Control 3 | 43.475 | 61 | 101 | 23001.73 | 43.386 | 28786688 |
| Control 4 | 43.518 | 60 | 73 | 22979.00 | 43.528 | 28803072 |
| Candidate 4 | 41.378 | 57 | 99 | 24167.43 | 40.794 | 30900224 |

Every lane completed all 1000 full semantic comparisons and failed the wall
gate (exit 70). All had five collections. Mixed CPU/wall behavior does not
attribute the 90/128 ms stalls to encoding, GC or scheduler internals. Nor is
the raw allocation-counter decrease an allocated-byte claim. No best-lane
promotion is justified. The scalar classifier, its test and case-count edit
were withdrawn after this negative matched result; existing dependency-derived
calling conventions remain unchanged. All artifacts/logs are retained. This
model packet is historical after rollback, not qualification of current files.
Post-withdrawal fresh closure .consumer-rollback-controls builds successfully;
eight zero-state and four private-call-environment controls pass with final OK.
All owned compilation, model and timing processes have ended. No matrix is
active. The baseline and immutable input corpus hashes remain unchanged.
Remaining open work is selective consumer execution across ordinary spans;
neither predicate cofactoring nor scalar helper elimination demonstrated the
required architecture-level throughput. Do not rerun these rejected candidates
unchanged or treat their static reductions as actual execution-count savings.

The candidate removes unconditional exact current-byte reset/increment machines
from ordinary-run trigger classification, accumulates their counters locally,
and publishes them before the unconsumed trigger residual. Other reads/writes,
conditional execution, duplicate targets and guard dependencies reject admission.
It is a transition summary, not a request cache or foreign scan. Static inline
trigger alphabets shrink from 52/53 to 16/17; executed coverage is not inferred.
Seven native admission controls and 24 independent complete actual Org AOT tapes
pass. Lean proves the reset/increment run-summary algebra; existing Quint checks
retain epoch, owner and residual/publication responsibilities. The full existing
source-bound chain passes at its unchanged 240-second Scheme limit:

- Lean: d7267559381ccb1991e8c82ef6ec2981954cf7adf9d8d682fe903461d77b10db
- Quint: 7af95f23d99a50f41cc8924fcd14eee3aa3aab7061c823e5d82b2b9eb43d92f2
- Scheme/actual AOT: 0ca97e6fbbc27638480a7d9f4d213a7086fe903bb91cbc08bfdff19e41d7222a

These observations describe the subsequently REJECTED source version, not the
restored current engine. Immutable sources and actual product are preserved in
.counter-summary-real-evidence; .counter-summary-full-evidence-retry.log records
matched-performance Missing and executionAuthorized=false. The first evidence
invocation failed before checks because an absolute receipt path was supplied;
the existing policy requires repository-relative receipt identifiers.

The first .counter-summary-abba-* launch did not explicitly await one yielded
process before the next request. Treat its timing as diagnostic-only; retain
its 321.481 ms candidate batch and do not claim strict isolation. The subsequent
.counter-summary-serial-* run explicitly awaits EVERY child before the next:

| Lane | 1000-request wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| control 1 | 49.831 | 70 | 157 | 20067.8 | 46.700 | 28819456 |
| candidate 1 | 44.380 | 61 | 89 | 22532.7 | 43.773 | 28852224 |
| candidate 2 | 43.538 | 62 | 98 | 22968.4 | 43.422 | 28852224 |
| control 2 | 46.177 | 63 | 185 | 21655.8 | 45.389 | 28786688 |
| candidate 3 | 43.169 | 62 | 87 | 23164.8 | 43.023 | 28868608 |
| control 3 | 43.416 | 63 | 101 | 23033.0 | 43.306 | 28819456 |
| control 4 | 44.172 | 64 | 95 | 22638.8 | 43.886 | 28819456 |
| candidate 4 | 43.881 | 63 | 110 | 22788.9 | 43.716 | 28852224 |

Every lane compares all 1000 independent complete tapes, has five GCs, and fails
the unchanged 10 ms preflight (exit 70). Reverse CPU times are essentially tied;
the candidate offers no substantial qualified gain and retains slightly more
RSS. The earlier disturbed timing was worse. The candidate admission, scanner,
test-count and plan-diagnostic edits were withdrawn, preserving the prior
condition lowering. .rejected-counter-summary-source.diff archives the attempted
source (also contains pre-existing context changes; NOT a wholesale rollback).
The pure Lean summary lemma remains only an algebraic contract for future work.
Existing baseline binary SHA a94f3fb148469e3f3f04bffc89bec90be66d65efcdd32664a8787e844ffa17c0
is unchanged. Rejected product SHA 891837346cadead2f3b5bd804aaefbc89ce36943e740bade904267b9b236c4be;
rejected prepared product SHA 842f37e58d5179f96fa89c497f78a7a5b1608dc59abf036384d85830086ca0bb.

Live BASELINE diagnosis, not throughput acceptance, now distinguishes realtime
and monotonic clocks, parser probes, complete tape comparison, GC CPU/wall and
voluntary/involuntary switches. The installed official std/os/time.ss uses
CLOCK_REALTIME; the independent POSIX monotonic probe does not alter that clock
or the SDK. Four .call-environment-stalls-*.log runs compare all 1000 tapes:

| Run | Batch wall ms | CPU ms | Parser monotonic ms | Tape comparison ms |
| --- | ---: | ---: | ---: | ---: |
| 1 | 71.491 | 62.702 | 68.028 | 1.268 |
| 2 | 47.720 | 47.386 | 44.909 | 1.039 |
| 3 | 111.849 | 71.842 | 108.193 | 1.401 |
| 4 | 48.670 | 47.593 | 45.910 | 1.014 |

Run 3 input 440 takes 4.908 ms realtime / 4.917 ms monotonic, only 0.438 ms CPU,
zero GC and two involuntary switches; input 675 takes 4.339/4.351 ms with 0.100 ms
CPU, zero GC and two involuntary switches. This reproduced class includes OS
descheduling, not GC or an isolated realtime-clock jump. It does NOT identify
the historical 142-148 ms batches retroactively, the scheduling competitor,
or all of the increased CPU cost. Probe overhead is included; no RSS/throughput
acceptance is inferred from this diagnostic. Diagnostic binary SHA
a0cee10298a820dffdb4344a863cbfa91e125c2046ad376679d4ef5e13eb216e.

The changing corpus remains e9ccee6c366bdc07f733c525634ed9efed2eb1d2932ef93b54f5ba4cd820df40.
All 20-batch matrices remain unopened: the 1000-request gate fails. SMP remains
OPEN/SKIP, not PASS. Rust queues, FFI and Python are outside these engine numbers.
Next: actual changing-input call census on the RESTORED baseline, rather than
inferring CPU dominance from the reduced static trigger alphabet. New count
products are diagnostic-only and may not replace the immutable timing baseline.

### Restored-baseline changing-input native call census

The .real-baseline-census diagnostic product retains exactly the original IR
file bytes (SHA d37b4b160c9d956a549e86486e0b590b0cbdbf2a273ccb16f683c100d3158c38).
Its 24 independent controls pass. The new prepared-counts attribution driver
then compares all 1000 changing inputs and 1017619 output tape bytes against the
existing independent oracle: .baseline-census-changing-counts-retry.log exits 0
and ends DIAGNOSTIC-OK. No speed/allocated-bytes claim: 302.614 ms diagnostic CPU
includes every instrumentation table update and four GCs. Native census binary
SHA ac22a196537ba6b56acbd49ea88b3e854df8ba8a846c4311f2ac28a9815ed8ff.

The first prepared-counts call attempted the production performance binder and
was correctly rejected (negative .baseline-census-changing-counts.log). That
protection is UNCHANGED. The corrected diagnostic-only interface binds the
corpus digest; generated IR and actual binary hashes are sealed separately in
this receipt. It does not obtain production admission or infer backend parity.

Expressions 54 and 565 each execute 95001 times; 13 expressions (including 120,
130, 555, 558, 560 and intermediate dispatch nodes) execute 94668 times each.
The latter include an ordered active-state selection chain over slots 15, 32,
50, 73 etc. Expression 42 computes a clamped two-byte successor 89606 times.
These are dynamic CALL counts, not per-expression exclusive CPU attribution.
Source encoding and event publication each execute 1000 times, and inline-span
is entered 5331 times. Thus static ordinary-run alphabet reduction alone does
not remove the heavily exercised residual selection path.

New guard-name diagnostics (.baseline-run-guard-cells.log) expose all 12/16
ordinary guard cells rather than only their count. Counts include inactive mode
payload counters; whether they cause fast-path rejection still needs executed
guard-hit evidence, not inference from names.

### Whole-residual byte-region candidate: rejected

The barrier-aware byte-region cofactor was implemented over the complete
residual, retaining ordered writes, else arms and original positive conditions.
Six run controls and 24 independent actual Org tapes passed. The source-bound
Lean/Quint/Scheme-AOT chain passed at the unchanged 240-second limit, recorded
in .residual-region-full-evidence.log and .residual-region-real-evidence.
That packet describes the subsequently rejected version, NOT current sources.
All compiler/evidence jobs and an unrelated SDK source experiment had ended
before the following strictly serial, awaited timing lanes:

| Lane | 1000-request wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| control 1 | 58.877 | 156 | 370 | 16984.56 | 56.124 | 28868608 |
| candidate 1 | 46.834 | 70 | 191 | 21352.01 | 45.378 | 30769152 |
| candidate 2 | 46.490 | 71 | 197 | 21510.00 | 45.636 | 30801920 |
| control 2 | 48.220 | 72 | 257 | 20738.28 | 46.625 | 28819456 |
| candidate 3 | 41.581 | 60 | 80 | 24049.45 | 41.558 | 30818304 |
| control 3 | 43.780 | 61 | 116 | 22841.48 | 43.714 | 28803072 |
| control 4 | 43.807 | 62 | 103 | 22827.40 | 43.678 | 28819456 |
| candidate 4 | 82.181 | 209 | 603 | 12168.26 | 62.133 | 30801920 |

All lanes compare 1000 complete independent tapes and fail the 10 ms gate
(exit 70). Candidate GC counts are four, versus five for controls, but this
does NOT offset the unstable wall/CPU or roughly 2 MB higher process RSS.
The compiler grows from 52 to 74 generated parts and 4713 helper definitions.
The stable reverse lane improves CPU only about five percent; the last lane
regresses. Withdraw this integration, preserving pre-existing condition
lowering and the immutable baseline. Archive .rejected-residual-region-source.diff
includes pre-existing context: it is NOT a wholesale rollback patch. Rejected
actual product SHA 04c0722e05e0f9d2008d4d5328d3ebaecbf4e5548d4a97c1bfa6e839b5d209ee;
prepared SHA 4a8713405500c9df15ffa453ebc528b8f2439079bac844019c4ec6154111b5a4.

### Idle trigger execution: rejected after actual AOT qualification

The next implementation separates state admission from source-byte admission:
a trigger byte need not imply an active machine. Keep existing ordinary-run
scanning; on a residual byte, check the complete body's zero-state guards and
execute its existing barrier-aware cofactor when eligible, otherwise execute
the original ordered body. Facts are invalidated at writes/opaque barriers
inside the byte and checked anew at the next iteration; no maintained cache.
This replaces the rejected whole-byte case expansion rather than stacking it.
Six native run controls pass, including distinct idle trigger and active-state
paths. .idle-trigger-controls owns compilation; .real-idle-trigger passes all
24 independent actual AOT complete-tape controls. Generated parts/definitions
are 53/3377, versus the control's 52/~3319, not the rejected 74/4713 expansion.
The complete source-bound chain exits 0 at the unchanged 240-second limit:

- Lean: c6a054e06c60245c6e79966f6e65ddaefd8f1ede70c9f572250a820844f4c8a8
- Quint: 20e5bcaf3d31f32b8802837803c7805254043c244059ca92a0744a98c3f742f8
- Scheme/actual AOT: 73c9e3d7421957adee17bd78ea8a840f67b1c125a98d14f32151d434ade0e7f3

.idle-trigger-full-evidence.log retains matched-performance Missing and
executionAuthorized=false. Staged source/product: .idle-trigger-real-evidence.
Actual product SHA 8b108a5aaab8b8f64537b46bd541c75df34a9966cadb2bad14cfb8215cca5aa5;
prepared SHA 0acd3c22f413771c029e29144576264736796930763df4b672345afcb33a5d6a.
Original IR and corpus hashes are unchanged. Before timing, another session's
new-SDK reference diagnostic was found consuming roughly 94 percent CPU;
wait for it to end without terminating or modifying its work. A subsequent
Ascent compiler batch also ended before timing. No performance qualification
follows from models. The strictly serial, explicitly awaited lanes are:

| Lane | 1000-request wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| control 1 | 44.102 | 60 | 129 | 22674.71 | 43.509 | 28803072 |
| candidate 1 | 75.223 | 157 | 428 | 13293.81 | 68.337 | 28901376 |
| candidate 2 | 42.067 | 64 | 92 | 23771.60 | 41.814 | 28884992 |
| control 2 | 45.846 | 66 | 145 | 21812.15 | 45.561 | 28819456 |
| candidate 3 | 45.751 | 73 | 182 | 21857.45 | 45.126 | 28901376 |
| control 3 | 45.547 | 61 | 154 | 21955.34 | 45.243 | 28868608 |
| control 4 | 46.747 | 68 | 136 | 21391.75 | 46.381 | 28819456 |
| candidate 4 | 43.806 | 65 | 152 | 22827.92 | 43.197 | 28884992 |

All lanes compare all 1000 complete independent tapes, report five GCs and
fail the unchanged preflight (exit 70). The raw allocation counter deltas are
identical, but are NOT allocated-byte totals under GC. No stable major gain:
candidate 1 regresses markedly, candidate 3 CPU is essentially tied, and
candidate RSS is slightly higher. A new external reference diagnostic appears
in the post-timing process snapshot; polling does not establish an isolated
host throughout the entire interval. Thus no architectural speedup or exact
cause of candidate 1's extra CPU is inferred. Reject the integration rather
than select its best singleton. Full matrices remain unopened.

The added idle-residual lowering, experiment-only test and test-count change
are withdrawn; pre-existing condition lowering stays intact. Five restored
native run controls pass with exit 0 in .idle-trigger-rollback-controls.
.rejected-idle-trigger-source.diff archives the experiment (also contains
pre-existing condition lowering; NOT a wholesale rollback). The model/product
packet is now historical rather than current source qualification. Immutable
baseline hash remains a94f3fb148469e3f3f04bffc89bec90be66d65efcdd32664a8787e844ffa17c0.

Next architectural obligation: establish per-consumer/state dependency support
and executed idle-admission coverage before another specializer. A monolithic
all-zero entry guard still couples independent payload counters and machines;
repeating that guard per byte does not prove removal of residual work. Do not
assume a counter is irrelevant from its name or introduce a maintained cache.
Lean's eligible-domain equivalence remains separate from Quint's invalidation,
ordered publication and ownership protocol. Real AOT and the 10 ms gate must
discharge the implementation/performance obligations, not just these models.

## 2026-10-13 bounded run leaf rejection and dependency-derived calling convention

GitHub PR 13 was verified at b5e1895c8bc9a79dcb3d4e845130cdc113b7d2be.
The installed pinned SDK and all existing baseline binaries remain unchanged.
No automation was restarted. Two engine candidates were implemented in order.

The first moved the already-admitted ordinary-run endpoint search into a
synchronous borrowed-buffer C leaf, with a single-byte specialization. Four
native controls include exhaustive byte/suffix differential checks; 24 real
Org AOT complete-tape controls pass. The candidate's standalone Lean and Quint
checks pass, but the combined source-bound Scheme gate times out at its original
240 seconds; that failed observation is retained, not counted as conformance.
The original memchr version failed dynamic linking, and was replaced by a
bounded C loop without changing the installed SDK or its link configuration.

Sequential A/B/B/A diagnostics under other active compiler workloads:

| Lane | 1000-request wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Existing anchored control 1 | 76.117 | 97 | 499 | 13137.670 | 52.118 | 28983296 |
| Borrowed run leaf 1 | 56.158 | 96 | 337 | 17806.902 | 53.835 | 29048832 |
| Borrowed run leaf 2 | 93.750 | 229 | 818 | 10666.667 | 62.464 | 29016064 |
| Existing anchored control 2 | 75.350 | 200 | 693 | 13271.400 | 59.364 | 28950528 |

All four compare all 1000 independent complete tapes, have five GCs and fail
the unchanged 10 ms gate (exit 70). CPU cost did not improve; RSS increased.
These disturbed diagnostics do not qualify throughput. The new runtime APIs,
compiler import/lowering and active scanner tests were rolled back. Negative
artifacts remain under .real-run-leaf, .run-leaf-*, and the test is archived as
.rejected-run-leaf-scan-test.ss. The pure first-trigger/partition lemmas remain
applicable to the restored Scheme scanner; they do not prove the rejected C.

The second candidate derives each generated helper's fixed arguments from its
normalized body, including transitive calls. Unused source/bounds/slots/owner
arguments are removed without request caches, speculative effects, code
inlining, or changes to event publication. Four focused native controls pass
for state-only bodies, quoted names, lexical cursor binding and list ownership;
Lean's EnvironmentExpr.project_required proves read-dependent projection for
its explicit expression fragment. Quint retains lifecycle responsibility.
The real generated product has 3319 definitions versus 3499 in its anchored
predecessor; static private argument slots decrease approximately 28 percent.
Static syntax counts are not dynamic instruction or allocation measurements.
Its real AOT binary 71412dddaf0a3920844c8743120d52266aff886f82ddefed022e2697e15a3cb6
passes 24 complete-tape controls. Full evidence-real-fold-controls passes within
the unchanged 240-second Scheme limit, with realAotControlsBound=true:
Lean d54d8d3b80bacc978181469c260854af00cc79d3c8d904fca0e21cc675e32be8;
Quint 7c9a6ae3b8eaf0e2fb8b2843081571d3230dbce5448dc23d8ad291f1efc22f35;
Scheme 8c0faa156f6020e026340dfe33eab4aa50643e9acd6d2cf890812290819a7ea1.
Native review 9993bfcece3a0f79cd0f16d160cf5957bd176ce404cfcbe58854ba79df1b8a91
still has matched-performance missing and executionAuthorized=false.
The prepared binary hash is a94f3fb148469e3f3f04bffc89bec90be66d65efcdd32664a8787e844ffa17c0.

After this task's compilation ended, matched A/B/B/A and reversed B/A/A/B
preflights retained the same corpus, full tapes, flags and five GCs:

| Lane | Wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Anchored control 1 | 45.327 | 64 | 100 | 22061.906 | 44.865 | 28950528 |
| Projected environment 1 | 43.471 | 60 | 95 | 23003.842 | 43.063 | 28803072 |
| Projected environment 2 | 43.217 | 62 | 107 | 23139.043 | 43.136 | 28835840 |
| Anchored control 2 | 44.048 | 63 | 93 | 22702.506 | 43.994 | 28934144 |
| Projected environment 3 | 142.624 | 164 | 1226 | 7011.443 | 73.236 | 28819456 |
| Anchored control 3 | 212.749 | 350 | 3135 | 4700.375 | 84.107 | 28950528 |
| Anchored control 4 | 70.518 | 120 | 476 | 14180.777 | 65.431 | 28950528 |
| Projected environment 4 | 148.276 | 237 | 1509 | 6744.180 | 70.871 | 28868608 |

Every lane passes 1000 complete semantic comparisons but exits 70 at the hard
10 ms gate. The first round has a modest wall/CPU decrease and lower peak RSS;
the reversed round is unstable for both products and does not reproduce a
qualified benefit. No concurrent compiler was found in the snapshot after
that round, so its pauses must NOT be attributed to compiler load by assumption.
Raw receipts remain .call-environment-abba-*.log; none are discarded.
The projected candidate's non-CPU remainder rises from 0.408/0.081 ms in the
first round to 69.388/77.405 ms in the second. CPU cost also rises: subtracting
CPU from wall alone cannot identify GC, scheduling or blocking mechanisms.
Keep this private candidate unpromoted; the installed/committed baseline stays
unchanged. No three-load twenty-batch matrix, performance acceptance or push.
Do not extrapolate these engine-only times to public Org::parse or FFI/Tokio.

## 2026-10-12 run specialization and spike attribution

Verified GitHub parser PR 13 still has head a7eb67a3c6c5398c7e591b88fafa2d2f4275fa94.
Attempted a contiguous foreign scan only for admitted overwrite-only ordinary
runs (87 generated real Org call sites), never arbitrary state/effect regions.
Nine focused controls and 24 complete actual AOT tapes passed. Sequential B/R/R/B:

| Product | Wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Buffer 9 | 49.054 | 65 | 70 | 20385.697 | 48.541 | 29048832 |
| Run leaf 1 | 48.578 | 65 | 75 | 20585.450 | 48.563 | 29081600 |
| Run leaf 2 | 48.326 | 66 | 72 | 20692.795 | 48.325 | 29081600 |
| Buffer 10 | 47.879 | 64 | 71 | 20885.983 | 47.879 | 29065216 |

No stable timing/RSS improvement; all failed the unchanged 10 ms gate.
Rolled back byte scanner API and compiler import/lowering; archive
.rejected-ordinary-leaf.patch. .real-buffer-runs is REJECTED, not a new baseline.
Restored eight active consumer controls; compiler closure rebuilt after rollback.
No full 20-batch matrix, README performance promotion or push.

Lean standalone Fold check passed with two new universal theorems: run residual
retention and contiguous partition. Quint full existing protocol suite passed
(7 publication, 8 shared-scan, 4 idle, 4 run controls); each of the three lifecycle
invariants ran 1000 seeded traces of 20 steps. These are bounded traces, not
exhaustive verification. Model collection now binds actual admission, lowering,
runtime scanner/writer and control source files. Aggregate Lake/ASCENT evidence
was not rerun: private checkout has no materialized LeanPoo dependency.

Three diagnostic-only complete-tape runs read installed header VM GC counters
and getrusage counters per call. Logs .stalls-runs-{1,2,3}.log retain top 20 rows.
Observed GC pauses 234-564 us account for most of the 314-696 us GC-bearing
requests; non-GC requests also show involuntary switches. Cold initial call and
extra probes mean these rows cannot replace matched performance receipts.
The historical 71.459 ms batch had 62.551 ms CPU, an 8.908 ms wall/CPU gap and
712 whole-process involuntary switches. Those process-wide counts include
initialization and do not identify timed-call switches. Its full root cause
remains UNPROVEN; do not attribute the whole spike to GC or scheduling.

Kernel/SDK unchanged; unsupported SMP still skipped; Automation remains off.

Source baseline: parser PR 13 a7eb67a3c6c5398c7e591b88fafa2d2f4275fa94;
Orgize PR 18 8f04aa246290b56b5605f4a38f25c55d0b416268.
Private checkout paths are experimental build locations, not dependency pins.
SDK: installed Gerbil 1cfb032 / Gambit ea114fc, unchanged.
ASP f5b7c009, POO 099b3815 and Core e85fd453 were privately rebuilt.
Complete parser source and current Org syntax declarations compiled successfully.

The candidate emitted 13 lexical shared-scan bindings and 28 frame-byte
bindings. Both control and candidate passed all 24 complete OEV1 tape cases.
Five planner and five lowering safety cases passed before rollback.
Independent changing-input preparation contains 1000 inputs and complete tapes.

Sequential unsandboxed paired preflights (identical binaries/options/input,
no task compiler active; order candidate/control/control/candidate):

| Product | Wall ms | P95 us | P99 us | Requests/s | Peak RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: |
| Candidate 2 | 60.211 | 86 | 99 | 16608.26 | 29507584 |
| Control 2 | 61.121 | 86 | 104 | 16360.99 | 29310976 |
| Control 3 | 58.712 | 83 | 95 | 17032.29 | 29278208 |
| Candidate 3 | 62.922 | 90 | 168 | 15892.69 | 29491200 |

All four failed the unchanged 1000-request <=10 ms gate. No 20-batch matrix
was launched. P95/P99 are per compiled parser call; wall includes complete
semantic comparison. Peak RSS includes executable, retained inputs/oracles,
and process initialization, not a parser-only allocation measurement.
Each batch reported five GCs and the same raw allocation delta 23014968;
the runtime did not admit that counter as allocated bytes. CPU approximately
matched timed wall. These are Fold/OEV1 results, not public Org, Tokio, Python,
or FFI end-to-end acceptance. Earlier sandboxed runs could not report RSS and
are retained separately, not pooled into these controls.

Rollback: removed candidate runtime lowering and the candidate generation
mode. Original runtime lowering restored; the rejected lowering patch and test
are retained as `.rejected-cross-lowering.patch` and
`.rejected-frame-guard-test.ss`. Existing `.real-cross` binaries are REJECTED
artifacts compiled before rollback, not the active source baseline.
Pure analysis, its division-error safety fix, compiler dependency separation
and private build/qualification support remain uncommitted and unpromoted.

Previous architectural target: fuse the full-range inline trigger precheck with
the guarded ordinary-run traversal. Actual Org currently scans for triggers
before traversing the same source range again. Any fusion must preserve the
no-trigger branch's lack of state writes, entry guard cells, exceptional-byte
ordering, final ordinary-prefix state, and complete tapes. Do not simply
delete the precheck or execute skipped state updates. No cross-request cache.

Receipt locations: `.control-preflight-{1,2,3}.log`,
`.candidate-preflight-{1,2,3}.log`, `.real-{control,cross}/org-fold-test.log`.
## Subsequent whole-engine experiments

Parser PR 13 head was reverified through GitHub as a7eb67a3c6c5398c7e591b88fafa2d2f4275fa94.
Scope remains the upstream parser engine; no downstream FFI/runtime changes.

Trigger/traversal cursor fusion reached six actual Org AOT sites and passed
five planner/lowering controls plus all 24 complete tape cases. Strict awaited
order control 6 / fused 3 / fused 4 / control 7 produced:

| Product | Wall ms | P95 us | P99 us | Requests/s | Peak RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: |
| Control 6 | 65.184 | 89 | 169 | 15341.188 | 29294592 |
| Fused 3 | 68.016 | 90 | 177 | 14702.423 | 30474240 |
| Fused 4 | 64.157 | 89 | 151 | 15586.764 | 30507008 |
| Control 7 | 62.530 | 86 | 137 | 15992.324 | 29278208 |

Rejected and rolled back compiler lowering, context flag, generator mode,
and active tests. Archive: .rejected-trigger-fusion.patch and
.rejected-trigger-fusion-test.ss. Fused binaries are rejected artifacts.
Earlier overlapping/yielded preflights 1/2 and controls 4/5 are diagnostic
only, excluded from this matched sequence.

Bounded one-use generated procedure contraction reduced private definitions
3588 -> 1830 and units 57 -> 29. Seven native controls covered argument-once,
sharing/escape, quotation, size budgets, lexical assignment, nested remaining
references and recursion. The actual AOT product passed all 24 complete tape
cases. No request cache, source snapshot, SDK or kernel changes.

Two strictly sequential C/K/K/C preflight groups were run; source corpus,
SDK, linker, process options and hard gate were unchanged:

| Product | Wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Control 8 | 72.001 | 93 | 354 | 13888.696 | 65.599 | 29310976 |
| Contracted 1 | 77.681 | 109 | 589 | 12873.161 | 59.773 | 27590656 |
| Contracted 2 | 60.711 | 107 | 211 | 16471.480 | 60.478 | 27590656 |
| Control 9 | 98.548 | 87 | 321 | 10147.339 | 65.739 | 29310976 |
| Control 10 | 62.873 | 89 | 132 | 15905.078 | 62.546 | 29310976 |
| Contracted 3 | 84.686 | 204 | 443 | 11808.327 | 75.512 | 27623424 |
| Contracted 4 | 54.205 | 79 | 114 | 18448.483 | 54.073 | 27557888 |
| Control 11 | 61.735 | 88 | 141 | 16198.267 | 61.608 | 29327360 |

Wall/CPU divergences and CPU variation preclude a stable performance claim.
Candidate GC counts 4 vs control 5 and lower RSS do not qualify throughput.
Every run failed the original <=10 ms / >=100000 requests/s gate (exit 70).
Do not pool batch P95/P99: these remain individual per-call distributions.
Allocation-counter deltas were not admitted as bytes by the runtime.

Contraction activation and lowering were rolled back. Archive:
.rejected-call-contraction.patch, .rejected-call-graph.ss and
.rejected-call-graph-test.ss. .real-contracted binaries are REJECTED artifacts,
not the accepted compiler. Baseline .real-control binary remains unchanged.
Restored compiler closure was rebuilt through build-fold-attribution (exit 0);
two cost controls and five state/effect controls passed with final OK.
Fresh tape-run generation in .real-restored matched every control
parse-org-compiled-events*.ss byte-for-byte (SOURCE-ROLLBACK-OK).
All task compilation/test/preflight processes have finished; no matrix active.

No full 60-batch matrix, public Org/FFI acceptance, README speed claim or push
was performed. Unsupported SMP remains skipped. Automation stays off.
Do not repeat either experiment as a fresh candidate. Next work must address
whole-engine execution/scan ownership and actual state dependence, not just
function counts, GC counts, or downstream queue concurrency.

## Continuous buffer / record consumer experiment

Parser PR 13 exact head was again verified unchanged through GitHub.
The retained local candidate changes only engine source traversal and tape
consumer: one synchronous std/ffi borrowed-buffer leaf per line boundary,
and one leaf per complete OEV1 start/token record. No retained pointer,
Scheme callback from C, host-alignment cast, request cache or kernel change.
Unsigned bounds, all wire byte orders, source visit order and seal checks
are retained. Eight native controls and all 24 actual tape controls passed.
The grammar digest and independent 1000-input/oracle corpus are unchanged.
The original .real-control executable remains immutable and accepted source.
The local .real-buffer candidate is NOT promoted or pushed.

First matched awaited C/B/B/C group:

| Product | Wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Control 12 | 67.740 | 97 | 301 | 14762.327 | 65.687 | 29310976 |
| Buffer 1 | 50.733 | 69 | 102 | 19711.036 | 50.679 | 29114368 |
| Buffer 2 | 50.243 | 71 | 100 | 19903.270 | 50.152 | 29097984 |
| Control 13 | 62.013 | 89 | 115 | 16125.651 | 61.867 | 29310976 |

Additional buffer controls (never suppress noisy/negative records):

| Product | Wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Buffer 3 | 50.085 | 67 | 105 | 19966.058 | 49.643 | 29097984 |
| Buffer 4 | 49.580 | 67 | 78 | 20169.423 | 49.566 | 29130752 |
| Buffer 5 | 54.252 | 75 | 123 | 18432.500 | 53.140 | 29114368 |
| Buffer 6 | 50.270 | 69 | 111 | 19892.580 | 50.162 | 29130752 |
| Buffer 7 | 71.459 | 176 | 383 | 13994.039 | 62.551 | 29097984 |
| Buffer 8 | 54.555 | 74 | 183 | 18330.126 | 54.271 | 29163520 |
| Control 14 | 64.159 | 88 | 133 | 15586.278 | 62.211 | 29278208 |

All preflights failed the unchanged <=10 ms / >=100000 requests/s gate.
No batch percentiles are pooled or called a per-request distribution.
Eight preflight processes are NOT a twenty-batch qualification matrix.

The extension lowering full-range membership probes, paired masks and
ordinary-run seeks to checked foreign leaves was slower than .real-buffer:
51.683/52.367 ms vs bracketing 50.085/49.580 ms; RSS 29130752/29114368.
P95 69/72 us, P99 90/112 us, CPU 51.611/52.286 ms, five GCs throughout.
Rolled back byte-spans API and every compiler lowering site. Evidence:
.rejected-buffer-scans.patch, .rejected-buffer-consumer-scans-test.ss,
.buffer-scans-preflight-{1,2}.log. .real-buffer-scans is a rejected artifact.

Official std/crypto/cipher.ss and std/misc/ports.ss were inspected for
private-buffer shrink publication. It removed the final copy in an experiment
but did not establish stable throughput/RSS improvement over .real-buffer:

| Owned publication | Wall ms | P95 us | P99 us | Requests/s | CPU ms | RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| 1 | 76.635 | 194 | 397 | 13048.868 | 70.494 | 29114368 |
| 2 | 49.250 | 68 | 97 | 20304.569 | 49.160 | 29147136 |
| 3 | 56.690 | 77 | 187 | 17639.795 | 54.085 | 29147136 |
| 4 | 49.776 | 66 | 76 | 20090.003 | 49.767 | 29130752 |

GC fell 5 -> 4; raw allocation-counter delta 23037048 -> 21104040, but
neither delta is admitted allocated bytes because GC occurred. GC count is
not a wall/throughput claim. Conservative selection restored copy publication;
archive .rejected-owned-publication.patch and .real-buffer-owned artifacts.
Retain every negative sample; neither GC nor scheduler cause is proven by
wall/CPU differences alone. No unsupported SMP acceptance or formal-model
proof is inferred from these controls.

Active private source now corresponds to .real-buffer, not scans/owned.
Final restored test-fold-buffer-consumer completed with eight CASE-OK and
HARNESS-OK/OK (exit 0). Static input comparisons are not byte-identical
because compile products include varying generated metadata; do not claim
artifact identity from source restoration alone. All task build
and preflight processes finished; no duplicated matrix/process is running.
No full matrix is active; no README qualified-performance claim or push.
Continue with actual generated state execution/dependence and remaining
allocation ownership. Do not repeat the rejected unconditional foreign scan
or shrink publication as newly discovered candidates. Automation stays off.

## Architecture review: event representation versus recognition execution

The user requested an architecture diagnosis before further optimization.
New implementation is paused. Condition-only Boolean lowering and lexical
horizontal-prefix sharing are experimental source edits: compilation is not
actual AOT conformance or performance acceptance. Do not promote these edits.
The immutable .real-buffer/prepared-tape-fold executable remains the control.

The ready-gated .fold-profile-2.sample observes the actual Fold generated units,
state execution and event writer. A concurrent build was active; the sample is
not matched exclusive CPU attribution or qualified performance evidence.
.fold-profile-1.sample captured startup and is excluded from Fold attribution.

The diagnostic .real-counts/counts.log records 4518 calls to expression-3471
across 24 complete AOT controls. In .real-buffer/parse-org-compiled-events-part-40.ss,
3471 scans horizontal whitespace; 3473 invokes 3471 and 3476 invokes 3473.
Code interning therefore does not establish shared dynamic scan results.
Counts are not exclusive timing, and the diagnostic Just invocation did not
complete successfully at its external resource-reporting boundary. The printed
diagnostic completion does not turn that invocation into a passed recipe.

Primary research distinguishes output representation from efficient recognition:
flap (https://arxiv.org/html/2304.05276) fuses deterministic lexer/parser work
and eliminates ephemeral tokens; its LL(1) assumptions are not established for
the whole Org strategy. A Typed, Algebraic Approach to Parsing
(https://www.cl.cam.ac.uk/~jdy22/papers/a-typed-algebraic-approach-to-parsing.pdf)
uses grammar restrictions and staging, not merely AOT compilation of every
rule. These papers do not prove this EventFold compiler correct or fast.

Current judgment: retain the lossless ordered event boundary provisionally;
do not qualify the recognition/execution architecture as high-performance.
Audit frame-local scan dependencies, effect/ordering barriers, active-rule
dispatch, source progress and per-byte work before selecting a restructuring.
Independent language conformance remains distinct from agreement between an
interpreter and a compiler consuming the same strategy. Lean owns semantic
refinement/progress/work bounds; Quint owns finite ownership/publication and
activation histories, not duplicate algebra or a physical latency proof.
The 71.459 ms historical spike remains only partly attributed; wall-minus-CPU
alone cannot identify a cause. No full matrix, baseline promotion, PR push,
kernel/SDK change or Automation restart occurred.

## Authorized architecture implementation follow-up

Audit: docs/research/event-fold-recognition-architecture.org records research
assumptions, recognizer/output separation and unproved whole-program bounds.
The frame-fact/condition candidate .real-frame-plan passes five frame lowering
controls, four condition controls and 24 actual complete Org AOT controls.
Its native prepared executable is .real-frame-plan/sdk/prepared-tape-fold.
Same independent input/oracle corpus and grammar identity are retained.

Sequential control/candidate/candidate/control preflight results:

| Lane | Batch wall ms | P95 us | P99 us | Requests/s | CPU ms | Peak RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Buffer 11 | 83.344 | 150 | 445 | 11998.464 | 60.326 | 29097984 |
| Frame 1 | 47.997 | 67 | 114 | 20834.635 | 47.856 | 28803072 |
| Frame 2 | 47.991 | 65 | 108 | 20837.240 | 47.823 | 28819456 |
| Buffer 12 | 50.837 | 67 | 106 | 19670.712 | 50.802 | 29065216 |

All four exit 70 at the unchanged physical gate. Buffer 11 includes a 13.505 ms
request spike: no large causal speedup is admitted from that disturbed lane.
Both candidates retain five GCs; raw counter deltas are not allocated bytes.
Static call graphs change 3590/7599 definitions/calls to 3494/7444. Counts are
not a runtime throughput proof. Do not start the twenty-batch matrix.

Baseline executable SHA256:
0db434809bbc5af932bc587a76f1f6663f7f9550401548546924d4dbbd5cf73d
Independent .real-preflight.ss SHA256:
e9ccee6c366bdc07f733c525634ed9efed2eb1d2932ef93b54f5ba4cd820df40

The next opt-in candidate moves private state into lexical execution-region
owners. Four ownership lowering controls pass. Its generated .real-lexical-plan
has 1616 definitions / 6365 private call sites; these are static counts only.
Actual linking is owned by exec session 2108; do not duplicate that build.
Its complete AOT controls and physical preflight have NOT yet run.
Compiler flag current-fold-scheme-lexical-state defaults to false. Root/helper
layout and parameter overrides are changed only in the explicit candidate.

Lean formal-fold, including the new conditional frame-representation transition
refinement theorem, passes. The theorem requires transition commutation; it is
not a proof of the concrete compiler. Quint formal-protocol passes publication,
scan, idle and run directed controls plus seeded bounded histories. Source
controls are wired into qualify-fold-plan and source identities bound by the
collector; the full Lake/ASCENT chain was not executed in this private checkout
because its pinned LeanPoo materialization is absent. Do not claim full closure.

## Follow-up closure: rejected lexical owners and fresh restored qualification

Supersedes the earlier missing-dependency state: exact LeanPoo manifest commit
0a111f6c5de43b261de5bba9287b48540d9f7c2e was materialized offline in the private
Lake package checkout; GitHub origin and manifests remain unchanged. Full
`just formal` passes all 38 jobs. Locked Quint dependencies are prepared offline.
The existing upstream .gerbil/native/libascent.dylib is loaded, not rebuilt.
The helper source/AOT bundle passed native evidence-real-fold-controls with
Lean, Quint and scheme-conformance established, matched-performance missing and
executionAuthorized=false. That source-bound packet is historical after rollback.

Strict matched results (1000 changing requests, complete comparison in wall):

| Lane | Wall ms | P95 us | P99 us | Requests/s | CPU ms | Peak RSS bytes |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Frame 3 | 46.292 | 61 | 76 | 21602.005 | 45.174 | 28819456 |
| Lexical 1 | 41.072 | 57 | 65 | 24347.487 | 41.066 | 28147712 |
| Lexical 2 | 104.958 | 154 | 1168 | 9527.621 | 57.008 | 28196864 |
| Frame 4 | 44.342 | 61 | 75 | 22551.982 | 44.317 | 28770304 |
| Frame 5 | 44.977 | 59 | 69 | 22233.586 | 44.193 | 28770304 |
| Helper 1 | 145.675 | 299 | 2130 | 6864.596 | 69.181 | 27426816 |
| Helper 2 | 44.563 | 62 | 103 | 22440.141 | 44.497 | 27361280 |
| Frame 6 | 48.229 | 65 | 132 | 20734.413 | 47.908 | 28803072 |

GC: lexical six, helper seven, controls five. Raw deltas under GC remain
unadmitted allocated bytes. Slow-lane non-CPU gaps remain unattributed.
Both candidates passed 24 actual complete AOT controls but fail physical gates.
Their compiler flag/root/helper layouts and temporary lexical test were removed;
compiled artifacts and every negative timing receipt are retained. The source
snapshot .rejected-lexical-owner-source.diff includes other task changes versus
base and is not a standalone rollback patch. No user source was deleted.

Active source retains frame-fact/condition lowering and explicit vector state.
`git diff --check` passes. Immutable baseline SHA256 remains
0db434809bbc5af932bc587a76f1f6663f7f9550401548546924d4dbbd5cf73d.
Restored current-source evidence is running as exec 80767 against fresh
.restored-evidence-controls/.restored-evidence-real. Do not duplicate this run;
the historical helper-source packet cannot stand in for the restored source.
No matrix, PR push, kernel/SDK change or Automation restart.

Restored exec 80767 completed exit 0. Native ASCENT now establishes the current
source's Lean model f5ca890c3f848f63c7a19456ffd5051504cef8b1d63238b8e1d939f813e8d681,
Quint protocol 675f8d76e6213f6d8e37b78e073f03bb831c0bd06276c0b5eb64e64083b3039b,
and Scheme/AOT conformance fd9fe9fedf9bd499a1f16effd613878eddee1668fc81d393c686a257060851da.
Review bundle 8fbb6f1450f89e5b7ab89c6b2c5a8560bbadd88e2c94760b509faf02e1d7186a
retains matched-performance missing and executionAuthorized=false. Its durable
packet is .data/parser-evidence/fold-controls.json. Do not mislabel it full
compiler proof, independent Org grammar completeness, FFI/Tokio/Python admission
or a physical speed claim. Fresh restored compiler controls are in
.restored-evidence-controls/lib; prefer them over the older .aligned-1cfb
compiler products containing historical experimental lowering.
All owned builds, model checks, AOT controls and preflight processes completed.
The <=10 ms gate remains failed; no twenty-batch matrix or promoted baseline.

## One-step state settling, 2026-10-13

New owned candidate: event-fold-run-settle.ss computes per-transition constant
post-state facts, joins both branches and preserves the entry transition.
Ordinary-run admission now accepts a stable overwrite residual after that
entry, rejects read/write recurrences and keeps prefix/entry guards unwritten
across both entry and residual. Lowering executes entry at the first byte and
residual at the last byte; the trigger remains for the original transition.
No runtime cache, event-owner transfer, kernel/SDK change or Automation restart.

Seven native admission cases pass, including first-byte capture, external
stable reads, recurrences and event/query barriers. Static real-IR run coverage
is 47/285 versus 33/285, not a performance measurement. Full actual Org AOT
product .real-settled-plan passes all 24 controls. Native preflight is linking
as exec 95907. Source-bound Lean/Quint/AOT collector is running as exec 12801
in .settled-evidence-controls/.settled-evidence-real; do not mutate source
inputs or duplicate these processes. Lean settled_run_refinement explicitly
requires stable-region agreement/preservation; Quint owns peeled-entry epoch
and commit ordering. Focused Lean and Quint checks pass; neither proves speed.
The original .real-buffer baseline SHA256 remains unchanged.

Settled v1 completed all source-bound model/actual-AOT controls through native
Ascent: Lean ff668b2f8f4379edd59aa4be8b3f2eb79f064168f38668ecc92137ab7d7d2900,
Quint f96bb76c75a2647997c0f1e08f786226b3f12fa9c7a24a7547369e3bf6734000,
Scheme e1b46397fe28968588df1644859c60f6e61152de1c50fb3d14537b7c1499699d.
Matched-performance stays missing, executionAuthorized=false. Sequential ABBA:
Frame7 46.374 ms, P95 62 us/P99 97 us, 21563.807 req/s, RSS 28819456 bytes;
Settled1 48.930 ms, P95 65 us/P99 137 us, 20437.359 req/s, RSS 28819456;
Settled2 47.077 ms, P95 65 us/P99 98 us, 21241.795 req/s, RSS 28819456;
Frame8 47.903 ms, P95 65 us/P99 119 us, 20875.519 req/s, RSS 28835840.
All complete 1000 requests/independent full tapes, five GCs, gate exit 70.
V1 is rejected, immutable product/receipts retained. V2 restores unchanged
already-admitted overwrite kernels and removes duplicate entry/residual
execution on singleton runs. V1 source-bound receipt is historical after this
revision. Do not claim a whole-engine speedup or start the three-by-twenty matrix.

V2 completed native source-bound closure after fast-forward to PR #13 exact
head b5e1895c8bc9a79dcb3d4e845130cdc113b7d2be. The upstream delta is one
request-owned LR commit; verified no Fold compiler/tape/source-lines delta.
No unrelated dirty edits were discarded; no push or path-pin mutation occurred.
V2 bundle: Lean 94acb95e2f31b961a211ddff3ded9a10f593a5ef766a467e20e9d2daee050d27,
Quint ded044e2bfdf63df7180ae40b3836ec34199e2c548e102b31d271a5b997052cc,
Scheme 1c356314bab03672f9a558211fc2a0fd3cbbc443b12cc659af56e3c8fca43149;
review 7b8cd2d742e3e3134ecbdaceb89ad5879ea7478f3aa63cc28a53eec5a989a64e
still lacks matched-performance and executionAuthorized=false.

V2 ABBA1: Frame9 137.279 ms (CPU64.381, max27865 us), candidate
57.128/46.096 ms (P95 79/61 us, P99 394/71 us), Frame10 46.635 ms.
No speedup inferred from the disturbed control. Repeated calmer ABBA2:
Frame11 44.537 ms, P95 60 us/P99 73 us, 22453.241 req/s, RSS28786688;
V2-3 45.936 ms, P95 62 us/P99 86 us, 21769.418 req/s, RSS28803072;
V2-4 45.341 ms, P95 62 us/P99 85 us, 22055.094 req/s, RSS28803072;
Frame12 46.412 ms, P95 63 us/P99 117 us, 21546.152 req/s, RSS28819456.
All 1000 complete independent tapes, five GCs, gate exit70. No robust physical
benefit; V2 is rejected as well. Baseline mean45.4745 ms versus45.6385 ms
candidate in this calm sequence; do not turn an individual faster lane into
an admitted improvement. Raw allocation counter deltas remain unadmitted.

Removed only this turn's settling production path/new module, extra test cases,
Justfile projection and model changes. Original ordinary-run/test files show
no tracked diff; frame-fact/condition and prior work remain. Prototype moved
to .rejected-event-fold-run-settle.ss; .rejected-settling-source.diff includes
other task diffs versus Git head and is NOT a standalone rollback patch.
Both compiled products, generated source and negative timing receipts remain.
Fresh restored latest-head source/Ascent/AOT qualification will use
.restored-latest-controls/.restored-latest-real; older V2 packets are historical
after rollback. No full matrix, promoted baseline, kernel/SDK change or Automation.

Restored latest-head exec11018 completed exit0. Current source-bound native
Ascent packet re-establishes Lean22b42bd804ff2ede969e742c556e907507b17c2537da011aae924e63ee080a9f,
Quint b3b72589621a30c1bbadfacd84a1419dd54cc14e39f5175d3e41b947fedfacee,
Scheme c290d4bff26ccba06fc95cc74557686cd4189077a538e76ae943a1c11b5ad175.
Review de405019967899a253a0a1616fafd6445a076f5a0666bec36711910659effbcb
retains matched-performance missing and executionAuthorized=false. Prefer fresh
.restored-latest-controls/lib for continued compilation. The candidate packets
remain historical; this is Fold qualification, not new LR/FFI/Tokio/downstream
acceptance. All task processes completed. No three-by-twenty matrix was started
because every real 1000-request preflight failed the unchanged <=10 ms gate.
Next implementation must target shared recognition across consumers/frames with
explicit action-order and ownership admission; static loop eligibility alone
did not identify the dominating dynamic costs. Do not repeat either settling
candidate or interpret host scheduling spikes as a causal optimizer benefit.

## Cross-consumer delimiter facts, 2026-10-13

Exact parser PR13 head remains b5e1895c8bc9a79dcb3d4e845130cdc113b7d2be.
The first prototype propagated immutable horizontal facts across declared
helpers, but actual IR inspection found zero eligible scans in all 27 helpers.
It was removed, not reported as a runtime improvement. Its generated product
.real-cross-consumer is historical and must not qualify current source.

The active prototype lowers dominating start-anchored delimiter/word/key scans
to lexical facts across ordered consumers. It does not hoist a conditionally
reached first observation or cross helper, loop, opaque or frame barriers.
The first consumer must have no query/error effect; state-dependent scans are
not admitted. Existing rejected unconditional content/trim sharing is not
re-enabled. No runtime lookup/cache, event list or new owner is introduced.

Directed tests caught an incorrect traversal of the statement list: the first
statement was not rewritten and its scan remained duplicated. Fixed by mapping
expression rewriting over each statement. The obsolete generator PID8791 was
terminated, preserving unrelated Ascent builds. .real-cross-delimiter is an
incomplete historical product. Corrected .real-cross-delimiter-v2 is active
in exec session65618; do not duplicate it. Five frame and five statement tests
now pass. Lean owns observation/ordered fold equality; Quint owns same-extent
consumer lifecycle and invalidation, not throughput. Actual AOT/Ascent and
physical preflight remain required. Baseline/corpus hashes remain unchanged.

Corrected AOT generated nine lexical delimiter facts: 3499 definitions,
7423 private call sites, 2079 syntactic tail calls, 70 event assignments.
Frame control has 3494/7443/2081/70. No dynamic cost inferred from these counts.
The new prepared executable SHA256 is
a65833e33e410f6f2ebc2022a41177241df153dea745953d67b050d0cfd2f155.
Actual 24 independent Org tape controls pass. Source-bound native Ascent join
exec92813 is active; current Lean observation is
3da42191790121abb3a98e0ec602d78d0dcbad452ecfb3b58ba1844a77e49dbf,
Quint746813527f5a2be1d8b8a9ed5528a651caa1f58722ce362e3782ef0368b31b5d.
Nine directed Fold lifecycle controls pass; bounded histories are not exhaustive.
Do not edit bound source while this collector runs. Physical ABBA must wait
for compilation to finish. No full matrix or baseline promotion occurred.

Installed official optimize-call.ss:239-296 explicitly checks procedure effect
signatures before discarding evaluation and preserves evaluation with begin
otherwise. This prototype uses a stricter no-query/no-error/no-allocation
admission rather than treating successful model tests as a purity oracle.

Source-bound Ascent exec92813 completed exit0. Scheme conformance observation
a2b60a430cd22f3e0bc0f7732d6e0e77c02e185e790bc72a84dadc8b1f9ab532;
review20b3dccbdad6120c1da21a9bb30725230cf34d0dd4fed277252eb3d53f1a6f08
still lacks matched-performance, executionAuthorized=false. Directed tests,
bounded Quint histories, Lean and 24 actual AOT tapes are distinct from speed.

All owned compilations ended before sequential ABBA preflights. Same pinned
SDK, executable controls, 1000 changing inputs, 2856770 bytes, independent
complete expected tapes and original 10 ms/100000 requests/s gate:

| Lane | Wall ms | P95 us | P99 us | Requests/s | Peak RSS bytes | CPU ms |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| Frame1 | 52.707 | 63 | 353 | 18972.812 | 28835840 | 50.031 |
| Candidate1 | 45.609 | 62 | 79 | 21925.497 | 28770304 | 45.606 |
| Candidate2 | 45.573 | 62 | 71 | 21942.817 | 28753920 | 45.548 |
| Frame2 | 46.113 | 63 | 85 | 21685.859 | 28819456 | 46.099 |
| Frame3 | 46.906 | 64 | 91 | 21319.234 | 28819456 | 46.805 |
| Candidate3 | 44.855 | 64 | 86 | 22294.059 | 28737536 | 44.810 |
| Candidate4 | 45.843 | 62 | 83 | 21813.581 | 28721152 | 45.785 |
| Frame4 | 45.827 | 62 | 81 | 21821.197 | 28819456 | 45.787 |

All eight lanes complete full semantic comparison, have five GCs and reject
with exit70. Allocation counter delta23037048 is NOT allocated-byte evidence
across GC. Candidate RSS27.390625-27.4375 MiB includes initialization, retained
inputs/oracles and native code; not per-request memory. Request percentiles
cover compiled Fold calls; batch wall includes full tape comparison, not Rust,
Tokio, Python or public Org::parse. No claim of a stable large speedup: Frame1
is disturbed and Frame4 nearly ties Candidate4. No consistent regression was
observed, so the source remains an unpromoted local candidate, not a new baseline.
Original .real-buffer and .real-frame-plan hashes remain unchanged. No push,
SDK/kernel change, Automation restart or three-by-twenty matrix occurred.

Next work must address ordinary-text recognition dispatch across consumers,
not mistake nine delimiter facts for whole-engine fusion or repeat the unused
declared-helper contract. Do not rebuild current products merely to repeat
the same diagnostic. Full workload matrix remains blocked by the unchanged
preflight gate, not by missing model evidence. All owned sessions completed.

## Whole-frame necessary-prefix rejection experiment (2026-10-13)

Existing PR13 still points to b5e1895c8bc9a79dcb3d4e845130cdc113b7d2be
(live GitHub head check). The experiment stages a shared necessary-prefix trie
across ordered consumers and selects a negative residual program. Positive
classification retains the original program. Pure state reads before the
necessary prefix may disappear only on the negative path. Queries/errors,
helpers, loops and source-frame changes remain opaque. No request cache,
kernel change, SDK rebuild, Automation restart or dependency path pin is added.

Changed source owners are byte dispatch, prefix-family and frame-statement
lowering. Lean negative_line_consumer has explicit necessity/effect premises;
Quint NegativeConsumer concerns current frame/owner histories only. The model
collector now binds the prefix-family implementation and byte-dispatch tests.
The directed byte-dispatch suite has two CASE-OK markers and final OK after
compiling the current prefix implementation. An earlier module load observed
the stale first-byte experiment; neither that failed check nor its historical
.real-line-negative product is a qualified current-source result.

Fresh .line-prefix-controls build completed. Actual .real-line-prefix contains
3811 definitions, 8697 private calls, 2637 syntactic tail calls and 70 event
assignments. These are static counts, not execution counts or speed evidence.
Native links are active sessions3627 and85884; do not duplicate them.
Prepared independent corpus and all three historical baseline executable
SHA256 values rechecked unchanged. Concurrent unrelated Ascent/parser-quality
compilation was observed; no affected timing is admitted as an idle-host result.
Current candidate has not yet received source-bound full AOT closure or timing.

The duplicate-program prefix experiment is rejected, not promoted. Actual24
complete tapes passed, but matched elevated ABBA lanes were control45.078ms,
candidate90.471ms, candidate47.987ms, control51.529ms. Candidate peak RSS
29360128/29409280bytes exceeds control28753920/28770304bytes. Candidate P95
122/67us, P99 818/117us, throughput11053/20839requests/s, fiveGCs. The slow
candidate's wall-minus-CPU36.447ms does not identify a cause. Earlier sandboxed
lanes1/2 cannot qualify RSS because time's sysctl was denied. All raw results
remain. Immutable rejected executable SHA256 is
c29d3d31d514096711ece4dba129242be342735ee9c54121a0b6211d8b685c44.

Production prototype now removes that duplicate-program expansion: one lexical
prefix classification guards admitted original conditions, keeping each
transition once. It does not introduce a runtime cache. The internal immutable
predicate is admitted as total and discarded on nested source-frame entry.
Fresh .line-shared-controls compiler and directed tests completed with finalOK.
Analysis was recompiled after the internal fact's totality declaration before
actual generation. Session17453 is preparing .real-line-shared; do not duplicate.
This replacement still requires current-source AOT closure and timing. No
three-by-twenty workload matrix is authorized by the failed preflight.

Final current-turn boundary: the single-body variant is an unpromoted source
prototype, NOT qualified AOT or performance. The independent executable link
failed with truncated generated C during overlapping links; subsequent owned
CLI and official compile-exe parallel:#f retries stopped making progress with
no compiler children. All were terminated with TERM, not left as active jobs.
.line-shared-linker.sample records select waiting; it does not establish a
kernel root cause. No SDK/kernel source was edited. Future links sharing static
outputs must be sequential. Retry logs include .line-shared-product.log,
.line-shared-preflight-serial-build.log and .line-shared-official-serial-build.log.

Correction: the 24 GXI checks import generated Scheme source, NOT a native
compiled module. They establish source-level complete-tape agreement only.
The attempted module preflights failed at import (illegal declare in ~0.scm),
before timing, and overlapped; they provide NO performance/RSS qualification.
The temporary module driver was removed from Justfile. Do not present these
receipts as AOT acceptance or compare them to the standalone baseline.

Current source-bound Lean identity28d41357d95bec7df065c5a2ac86152d510f6250a2a097674f054d7f88ec6645
and Quint identity674445d5c530f4235895970f9976c98dfed5c90575075109d5b978c4ce1b0c10
passed. Native review3e319af898280354eab2479757e42d6f1f070622d61a6e7954ae45010fd21008
correctly lacks scheme-conformance and matched-performance, executionAuthorized=false.
No source-bound full current AOT packet, standalone single-body performance,
twenty-batch matrix, push or baseline promotion is claimed. Historical baseline
executable hashes remain unchanged. No owned benchmark/compiler sessions remain.
Next work must close the generated-product linking gate without kernel/SDK
modifications, then measure this single-body variant under matched conditions;
do not duplicate an active matrix or revive the rejected duplicate-program path.

## Independent AOT lock recovery and horizontal anchors (2026-10-13)

Live PR13 remains b5e1895c8bc9a79dcb3d4e845130cdc113b7d2be. Official driver.ss
compile-obj retries while its exact .o.lock exists. Process inventory and lsof
found no owner for the retained part-11 lock under .real-line-shared/lib/static.
Moved only that lock to .real-line-shared/part-11.stale-object-lock.evidence.
No SDK/kernel source changed. Sequential existing CLI links then completed.
The prior select-only sample was a lock wait, not a proved kernel deadlock.
No active unrelated compiler was terminated or baseline binary overwritten.

Just now exposes link-org-fold-candidate for resume without regeneration.
Both link recipes call the small t/benchmarks/parser-throughput/link-locks.ss
guard: any lock rejects the build, no automatic deletion. Its native GXI
fixture checks clean admission, dirty rejection, unchanged evidence and clean
re-admission. Final OK passed; initial import-syntax failures were fixed before
qualification. qualify-fold-plan includes this regression check.

Recovered .real-line-shared/org-fold SHA256:
c14618c3a26ccf542dffa2ba668bd95d9dc6a54de3b6b8361f47e9a95835e4dd.
Recovered prepared-tape-fold SHA256:
21f8b36cb45965c5d4a121eb1ed8e6093ce07445e5e047997fc0d78aa42f5078.
All 24 independent native Org tapes pass. Eight sequential delimiter-control
ABBA preflights have candidate43.006-45.728ms, P9560-65us, P9966-87us;
control43.494-47.975ms. Candidate RSS28803072-28917760bytes. All semantic1000
requests pass, all have fiveGCs, all fail the unchanged10ms gate with exit70.
No large/stable speedup or promoted baseline is claimed.

Current unpromoted prototype extends necessary prefix classification to total
line-skip-horizontal start anchors and expanded byte membership predicates.
The horizontal offset is bound once and reused in original consumer bodies;
mutable state offsets and query/error predecessors cannot admit a guard.
Nested source frames clear both prefix and horizontal facts. No runtime cache,
duplicated full/residual program, kernel edit or Automation restart is added.
Fresh .line-anchored-controls compilation and two directed byte controls pass.
Actual .real-line-anchored has3499defs/7420privatecalls/2081syntactic-tailcalls/
70eventassignments, static observations only. Its native24complete tapes pass.
org-fold SHA256 b7424e97e412c293cf3a0f83b66f3b17825fdfc968eb53f3a05578aab5a17bcb;
prepared-tape-fold71d478a55a052d0bf04f658aa3671bcd79c788fa3c9ed7264f7b74b32ebb691b.

Sequential ABBA versus delimiter control:47.230/44.626/45.312/46.060ms.
Sequential ABBA versus single-body predecessor:51.264/44.320/44.919/49.605ms.
Four anchored lanes P9563-64us, P9996-124us, throughput22069.209-22563.177req/s,
RSS28901376-28917760bytes, CPU44.209-45.212ms, fiveGCs. The later predecessor
controls are slower than earlier43ms lanes; no stable large benefit inferred.
Candidate memory exceeds delimiter controls. All full1000independent tapes
pass and all gate exits are70. Counterdelta23037048 acrossGC is not allocated
bytes. Request percentiles are compiledFold calls; wall includes complete tape
comparison; wholeprocessRSS includes retained inputs/oracles/nativecode. No
FFI/Tokio/Python/publicOrg::parse or SMP parallelism acceptance is implied.

Source-bound Lean and Quint model checks passed. Current real AOT join is
running as exec15974, using .line-anchored-evidence-controls and immutable
.line-anchored-real-evidence. Do not change bound Scheme/Just/model/test inputs
or duplicate this qualification while active. There is no active benchmark
matrix. No20batchmatrix, push or baseline promotion occurred. README and the
research report record explicit units, failed gates and evidence boundaries.
Historical baseline hashes rechecked unchanged. Unsupported SMP remains open,
not passed. Continue whole-engine ordinary-loop work after this evidence gate;
the anchored frame classifier alone does not eliminate that remaining work.

Final evidence exec15974 completed exit0 within the unchanged240s gate:
Lean cd0c4843cf7df4969a5401eef5e21f4ee1f5deca1059544e83a0d531cf4fa2a1;
Quint e2dd4fab0166eb1c9eeca6763400822a2eaef991d3d146748d9329999d46e869;
Scheme-to-actual-AOT9b9a8362dd437b28c14522da24796ea14c1d603afee00ba8a0945d71a01009ba.
Native review627eafa6dddbe7fc767fcae6571e9886d3331024b62a8a6b8c818c8236ffff04
has realAotControlsBound=true, matched-performance missing and
executionAuthorized=false. This is conditional model plus differential
conformance, not a complete compiler proof or speed admission. All owned
builds/models/benchmarks have ended. Three historical executable hashes remain
unchanged; git diff --check passes. No push, baseline promotion, kernel/SDK
change or Automation restart. Retain the prototype unpromoted: current matched
receipts do not establish a consistent regression against its predecessors,
but also do not qualify the hard gate or eliminate ordinary-loop dispatch.

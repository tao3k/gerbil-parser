import ParserEngine.Lookahead
import ParserEngine.Capture
import ParserEngine.Reuse
import ParserEngine.Recognition
import ParserEngine.Preparation
import ParserEngine.Sequence
import ParserEngine.Reduction
import ParserEngine.Examples
import ParserEngine.Fold
import ParserEngine.OwnedStack
import ParserEngine.Execution

/-! Proofs for logical deterministic reduction and an explicit operation model.
No theorem asserts that Scheme execution refines this model or establishes CPU speed. -/

#print axioms ParserEngine.prepared_plan_equivalent
#print axioms ParserEngine.reverse_reduction_equivalent
#print axioms ParserEngine.sequence_visits_exact
#print axioms ParserEngine.copied_prefix_triangular
#print axioms ParserEngine.reduction_preserves_depth
#print axioms ParserEngine.complete_publication_lower_bound

#print axioms ParserEngine.materialize_into_exact
#print axioms ParserEngine.prepared_reverse_reduction_equivalent

#print axioms ParserEngine.freeze_stack_length
#print axioms ParserEngine.freeze_stack_ext
#print axioms ParserEngine.freeze_stack_roundtrip
#print axioms ParserEngine.frozen_frame_aligned
#print axioms ParserEngine.freeze_stack_push

#print axioms ParserEngine.staged_action_equivalent


#print axioms ParserEngine.Reuse.admitted_identity
#print axioms ParserEngine.Reuse.retained_cost_not_more
#print axioms ParserEngine.Reuse.retained_cost_decomposition

#print axioms ParserEngine.Reuse.checked_reuse_break_even

#print axioms ParserEngine.Reuse.nested_commit_survives_outer
#print axioms ParserEngine.Reuse.failed_publication_preserves_history

#print axioms ParserEngine.Capture.split_prefix_exact
#print axioms ParserEngine.Capture.capture_visits_strict

#print axioms ParserEngine.Capture.token_count_order_independent

#print axioms ParserEngine.Lookahead.admitted_projection
#print axioms ParserEngine.Lookahead.extent_break_even
#print axioms ParserEngine.Lookahead.complete_request_with_extra_work
#print axioms ParserEngine.Lookahead.extent_observations_equal
#print axioms ParserEngine.Lookahead.singleton_overhead_regresses

#print axioms ParserEngine.Lookahead.unused_extent_can_pay_failed_admission
#print axioms ParserEngine.Lookahead.one_failed_attempt_charged

#print axioms ParserEngine.Lookahead.extent_with_retries_break_even

#print axioms ParserEngine.Reuse.failed_source_retries_original
#print axioms ParserEngine.Reuse.successful_source_reuses

#print axioms ParserEngine.Capture.classified_metadata_exact
#print axioms ParserEngine.Capture.classified_consumer_exact
#print axioms ParserEngine.Capture.classification_break_even

#print axioms ParserEngine.Capture.sparse_frontiers_exact
#print axioms ParserEngine.Capture.first_shift_skipped
#print axioms ParserEngine.Capture.fused_driver_order

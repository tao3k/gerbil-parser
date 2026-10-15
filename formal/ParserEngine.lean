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

#print axioms ParserEngine.Lookahead.reductions_preserve_pending
#print axioms ParserEngine.Lookahead.reduced_shift_preserves_admission
#print axioms ParserEngine.Lookahead.per_query_conversion_regresses

#print axioms ParserEngine.Lookahead.request_transport_regresses

#print axioms ParserEngine.Lookahead.selector_transport_exact

#print axioms ParserEngine.goto_column_exact
#print axioms ParserEngine.goto_plan_within_budget

#print axioms ParserEngine.freeze_stack_pop
#print axioms ParserEngine.captured_entry_after_pop

#print axioms ParserEngine.certified_fallback_loses
#print axioms ParserEngine.omitted_fallback_has_witness

#print axioms ParserEngine.kernel_unroll_two
#print axioms ParserEngine.kernel_unroll_four
#print axioms ParserEngine.prepared_kernel_exact

#print axioms ParserEngine.deferred_terminal_field_exact
#print axioms ParserEngine.restored_owned_snapshot
#print axioms ParserEngine.deferred_terminal_chain_exact

#print axioms ParserEngine.singleton_state_elimination
#print axioms ParserEngine.prepared_class_membership

#print axioms ParserEngine.prepared_start_query_exact
#print axioms ParserEngine.absent_literal_probe

#print axioms ParserEngine.Capture.fused_yield_observation
#print axioms ParserEngine.Capture.fused_wide_decisions_strict
#print axioms ParserEngine.Capture.capture_cost_requires_all_charges


#print axioms ParserEngine.Capture.changed_boundary_rejected

#print axioms ParserEngine.Capture.delayed_yield_first_exact
#print axioms ParserEngine.Capture.delayed_yield_warm_exact
#print axioms ParserEngine.Capture.fresh_yield_allocations_eliminated
#print axioms ParserEngine.Capture.forced_yield_allocations_retained

#print axioms ParserEngine.Capture.capture_bounds_exact

#print axioms ParserEngine.Capture.shared_context_roundtrip
#print axioms ParserEngine.Capture.shared_context_consumer
#print axioms ParserEngine.Capture.cached_context_admission
#print axioms ParserEngine.Capture.shared_context_slot_break_even

#print axioms ParserEngine.retained_empty_digest

#print axioms ParserEngine.Capture.prepared_row_context_admission

#print axioms ParserEngine.Capture.unary_children_exact
#print axioms ParserEngine.Capture.unary_children_warm

#print axioms ParserEngine.Capture.boundary_snapshot_exact

#print axioms ParserEngine.Capture.prepared_capture_prefix_exact
#print axioms ParserEngine.Capture.prepared_capture_arguments_saved

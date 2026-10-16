import ParserEngine.HostLowering
import ParserEngine.Host
import ParserEngine.Representation
import ParserEngine.Operational
import ParserEngine.Construction
import ParserEngine.Names
import ParserEngine.Encoding
import ParserEngine.Lowering
import ParserEngine.Solver
import ParserEngine.NativeFacts
import ParserEngine.RawRequest
import ParserEngine.Grammar
import ParserEngine.Request
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

#print axioms ParserEngine.Capture.canonical_context_admission

#print axioms ParserEngine.Capture.capture_row_partitions_disjoint
#print axioms ParserEngine.Capture.capture_row_custom_index_bounded

#print axioms ParserEngine.Capture.deferred_prefix_children_exact
#print axioms ParserEngine.Capture.deferred_prefix_children_warm
#print axioms ParserEngine.Capture.deferred_prefix_no_demand
#print axioms ParserEngine.Capture.deferred_prefix_first_demand

#print axioms ParserEngine.Capture.packed_children_exact
#print axioms ParserEngine.Capture.packed_children_warm
#print axioms ParserEngine.Capture.packed_children_exclude_suffix
#print axioms ParserEngine.Capture.packed_children_no_demand
#print axioms ParserEngine.Capture.packed_children_first_demand

#print axioms ParserEngine.Capture.capture_two_static
#print axioms ParserEngine.Capture.capture_three_static
#print axioms ParserEngine.Capture.capture_four_static
#print axioms ParserEngine.Capture.capture_static_observation_order

#print axioms ParserEngine.Capture.capture_countdown_stays_bounded
#print axioms ParserEngine.Capture.capture_countdown_index

#print axioms ParserEngine.prepared_corpus_preserves
#print axioms ParserEngine.corpus_preparations_saved

#print axioms ParserEngine.Capture.direct_packed_one_fewer
#print axioms ParserEngine.Capture.direct_packed_observation
#print axioms ParserEngine.Capture.pending_capture_vector_is_not_token
#print axioms ParserEngine.Capture.tagged_capture_vector_payload
#print axioms ParserEngine.Capture.tagged_capture_vector_is_not_token
#print axioms ParserEngine.Capture.unary_vector_mutation_keeps_storage_kind

#print axioms ParserEngine.Request.initial_valid
#print axioms ParserEngine.Request.capture_conserves
#print axioms ParserEngine.Request.shift_valid
#print axioms ParserEngine.Request.reduce_valid
#print axioms ParserEngine.Request.trace_valid
#print axioms ParserEngine.Request.root_and_remainder
#print axioms ParserEngine.Request.complete_root

#print axioms ParserEngine.Grammar.nullable_iff_derives
#print axioms ParserEngine.Grammar.nullable_least
#print axioms ParserEngine.Grammar.first_least
#print axioms ParserEngine.Grammar.first_reach
#print axioms ParserEngine.Grammar.nullable_productive
#print axioms ParserEngine.Grammar.eof_lookahead_requires_nullable
#print axioms ParserEngine.Grammar.terminal_lookahead_has_derivation
#print axioms ParserEngine.Grammar.erase_reduce
#print axioms ParserEngine.Grammar.trace_valid
#print axioms ParserEngine.Grammar.accepted_trace_derivation
#print axioms ParserEngine.Grammar.table_driven_acceptance
#print axioms ParserEngine.ranked_fold_maximum

#print axioms ParserEngine.Grammar.source_request_derivation

#print axioms ParserEngine.Lookahead.raw_terminal_is_state_sensitive
#print axioms ParserEngine.Lookahead.exact_literal_precedes_kind

#print axioms ParserEngine.Grammar.Fixture.repeated_S

#print axioms ParserEngine.Viable.completed_rhs
#print axioms ParserEngine.Viable.accepting_stack
#print axioms ParserEngine.RawRequest.reduce_valid
#print axioms ParserEngine.RawRequest.complete_request

#print axioms ParserEngine.Lookahead.selected_raw_supported

#print axioms ParserEngine.Viable.lr1_graph_constructed

#print axioms ParserEngine.RawRequest.builtin_action_and_terminal

#print axioms ParserEngine.Completion.nullable_certificate_exact
#print axioms ParserEngine.Completion.first_certificate_exact
#print axioms ParserEngine.NativeFacts.fixture0_first_exact

#print axioms ParserEngine.Completion.fresh_notification_counts_occurrences
#print axioms ParserEngine.Completion.zero_remaining

#print axioms ParserEngine.Lowering.lowering_exact
#print axioms ParserEngine.Lowering.named_lowering_exact
#print axioms ParserEngine.Lowering.native_root_sound
#print axioms ParserEngine.Worklist.quiescent_exact
#print axioms ParserEngine.Solver.preparation_pipeline_exact

#print axioms ParserEngine.Lowering.isolated_repeat_origin
#print axioms ParserEngine.Lowering.isolated_repeat1_origin

#print axioms ParserEngine.Encoding.delta_conservation
#print axioms ParserEngine.Encoding.field_complete
#print axioms ParserEngine.Worklist.batch_quiescent_exact

#print axioms ParserEngine.Names.unique_names
#print axioms ParserEngine.Names.reserved_not_helper
#print axioms ParserEngine.Names.ordinal_alignment
#print axioms ParserEngine.Names.checked_native_root_sound
#print axioms ParserEngine.Worklist.mask_notification_valid

#print axioms ParserEngine.Solver.batched_preparation_exact

#print axioms ParserEngine.Encoding.publish_queue_valid
#print axioms ParserEngine.Encoding.drain_queue_valid

#print axioms ParserEngine.Names.helpers_avoid_source
#print axioms ParserEngine.Encoding.merge_delivered
#print axioms ParserEngine.Encoding.queue_empty_exact

#print axioms ParserEngine.Completion.register_count
#print axioms ParserEngine.Completion.mutable_notification_exact
#print axioms ParserEngine.Completion.mutable_prefix_nonnegative

#print axioms ParserEngine.Completion.mutable_enables_exact

#print axioms ParserEngine.Completion.counter_trace_exact

#print axioms ParserEngine.Names.produced_recipe_checked
#print axioms ParserEngine.Names.sequence_recipe_produced
#print axioms ParserEngine.Names.choice_recipe_produced
#print axioms ParserEngine.Names.repeat_recipe_produced
#print axioms ParserEngine.Names.repeat1_recipe_produced
#print axioms ParserEngine.Names.admitted_native_sound
#print axioms ParserEngine.Viable.dense_coordinate_roundtrip
#print axioms ParserEngine.Viable.dense_goto_refines_advance
#print axioms ParserEngine.Viable.closure_trace_exact
#print axioms ParserEngine.Names.admitted_native_root_sound
#print axioms ParserEngine.Viable.goto_kernel_exact
#print axioms ParserEngine.Viable.construction_traces_supported
#print axioms ParserEngine.Names.operand_recipe_produced
#print axioms ParserEngine.Names.native_sequence_recipe_produced
#print axioms ParserEngine.Viable.lr1_trace_exact
#print axioms ParserEngine.Viable.lr1_goto_kernel_exact
#print axioms ParserEngine.Viable.lr1_construction_traces_supported

#print axioms ParserEngine.Construction.intern_decodes
#print axioms ParserEngine.Construction.intern_unique
#print axioms ParserEngine.Construction.interned_identity_injective
#print axioms ParserEngine.Construction.positive_kernel_identity
#print axioms ParserEngine.Construction.refinement_never_merges
#print axioms ParserEngine.Construction.active_signature_agrees
#print axioms ParserEngine.Construction.packed_conflict_has_origin
#print axioms ParserEngine.Construction.packed_bits_exact
#print axioms ParserEngine.Construction.published_table_supported
#print axioms ParserEngine.Construction.install_choice_origin
#print axioms ParserEngine.Construction.admitted_event_supported
#print axioms ParserEngine.Construction.install_preserves_cell_trace

#print axioms ParserEngine.Construction.physical_code_injective
#print axioms ParserEngine.Construction.duplicate_rules_keep_physical_identity
#print axioms ParserEngine.Construction.split_successor_dirty
#print axioms ParserEngine.Construction.clean_predecessor_transport
#print axioms ParserEngine.Construction.replay_candidates_trace
#print axioms ParserEngine.Construction.replayed_table_supported
#print axioms ParserEngine.Construction.scratch_write_covered
#print axioms ParserEngine.Construction.scratch_clear_empty
#print axioms ParserEngine.Construction.publication_retains_row_and_clears
#print axioms ParserEngine.Construction.admitted_physical_event_supported
#print axioms ParserEngine.Construction.augmented_cannot_reduce
#print axioms ParserEngine.Construction.authored_cannot_accept
#print axioms ParserEngine.Construction.decoded_candidates_admitted
#print axioms ParserEngine.Construction.physical_replayed_table_supported
#print axioms ParserEngine.Construction.clean_group_uniform
#print axioms ParserEngine.Construction.event_scratch_covered
#print axioms ParserEngine.Construction.event_scratch_trace
#print axioms ParserEngine.Construction.event_row_publication
#print axioms ParserEngine.Construction.refined_signature_uniform

#print axioms ParserEngine.Construction.vector_write_refines
#print axioms ParserEngine.Construction.vector_clear_refines
#print axioms ParserEngine.Construction.vector_clear_covered
#print axioms ParserEngine.Construction.vector_event_projection
#print axioms ParserEngine.Construction.vector_event_publication
#print axioms ParserEngine.Construction.raw_reductions_origin
#print axioms ParserEngine.Construction.layout_selection_origin
#print axioms ParserEngine.Construction.layout_replay_supported
#print axioms ParserEngine.Construction.union_masks_exact
#print axioms ParserEngine.Construction.quotient_lookahead_origin
#print axioms ParserEngine.Construction.suffix_evidence_exact
#print axioms ParserEngine.Construction.suffix_delivery_exact
#print axioms ParserEngine.Construction.suffix_evidence_allowed
#print axioms ParserEngine.Construction.suffix_prediction_supported
#print axioms ParserEngine.Construction.shifted_lookahead_retained
#print axioms ParserEngine.Construction.cell_events_origin
#print axioms ParserEngine.Construction.vector_layout_publication

#print axioms ParserEngine.Operational.Heap.allocation_identity
#print axioms ParserEngine.Operational.Heap.vector_write_retains
#print axioms ParserEngine.Operational.Heap.failed_write_atomic
#print axioms ParserEngine.Operational.Heap.publication_survives_trace
#print axioms ParserEngine.Operational.Scheduling.step_decreases
#print axioms ParserEngine.Operational.Scheduling.finish_stopped
#print axioms ParserEngine.Operational.Scheduling.finish_quiescent
#print axioms ParserEngine.Operational.Scheduling.partition_round_bound
#print axioms ParserEngine.Operational.Layout.end_requires_owned_frame
#print axioms ParserEngine.Operational.Layout.eligible_next_cannot_close
#print axioms ParserEngine.Operational.Layout.declared_boundary_closes
#print axioms ParserEngine.Operational.Layout.context_selection_origin
#print axioms ParserEngine.Operational.Layout.unwind_owned_scope
#print axioms ParserEngine.Operational.Layout.every_exit_restores
#print axioms ParserEngine.Operational.Heap.private_trace_retains
#print axioms ParserEngine.Operational.Scheduling.native_delta_progress
#print axioms ParserEngine.Operational.Scheduling.trace_length_bound
#print axioms ParserEngine.Operational.Layout.outer_columns_survive
#print axioms ParserEngine.Operational.Heap.allocation_is_fresh
#print axioms ParserEngine.Operational.Layout.token_final_byte_in_bounds

#print axioms ParserEngine.Operational.Scheduling.stale_publication_noop

#print axioms ParserEngine.Operational.Collection.frozen_survives

#print axioms ParserEngine.Operational.Collection.identity_survives

#print axioms ParserEngine.Operational.Collection.private_owner_survives

#print axioms ParserEngine.Operational.Collection.suspended_publication_survives

#print axioms ParserEngine.Operational.Reentry.crossing_decomposes

#print axioms ParserEngine.Operational.Reentry.resume_captured_path

#print axioms ParserEngine.Operational.Reentry.same_path_has_no_crossings

#print axioms ParserEngine.Operational.Reentry.repeated_resume

#print axioms ParserEngine.Operational.Reentry.captured_context_restored

#print axioms ParserEngine.Operational.Reentry.captured_column_survives

#print axioms ParserEngine.Operational.Backward.buckets_mass

#print axioms ParserEngine.Operational.Backward.buckets_nonempty

#print axioms ParserEngine.Operational.Backward.refined_vertices

#print axioms ParserEngine.Operational.Backward.changed_grows

#print axioms ParserEngine.Operational.Canonical.matrix_is_enumerated

#print axioms ParserEngine.Operational.Canonical.intern_catalogue_bound

#print axioms ParserEngine.Operational.Canonical.closure_enqueue_decreases

#print axioms ParserEngine.Operational.Canonical.existing_never_requeues

#print axioms ParserEngine.Operational.Canonical.novel_intern_decreases

#print axioms ParserEngine.Operational.Canonical.intern_accounting

#print axioms ParserEngine.Operational.Canonical.drain_decreases

#print axioms ParserEngine.Operational.Canonical.completed_processes_catalogue

#print axioms ParserEngine.Operational.Backward.round_bound

#print axioms ParserEngine.Operational.Canonical.expand_rank_le

#print axioms ParserEngine.Operational.Canonical.finish_quiescent

#print axioms ParserEngine.Operational.Canonical.finish_processes_all_keys

#print axioms ParserEngine.Operational.Canonical.catalogue_count

#print axioms ParserEngine.Representation.Roots.frame_slot
#print axioms ParserEngine.Representation.Roots.frame_sound
#print axioms ParserEngine.Representation.Roots.captured_handle
#print axioms ParserEngine.Representation.Roots.active_slot
#print axioms ParserEngine.Representation.Roots.captured_frame_reached
#print axioms ParserEngine.Representation.Roots.path_reached
#print axioms ParserEngine.Representation.Roots.discovered_path_relocated
#print axioms ParserEngine.Representation.Roots.program_member
#print axioms ParserEngine.Representation.Roots.marking_program_exact
#print axioms ParserEngine.Representation.Roots.marking_control_flow_exact
#print axioms ParserEngine.Representation.Roots.marked_reached
#print axioms ParserEngine.Representation.Masks.callback_unique
#print axioms ParserEngine.Representation.Masks.field_address_injective
#print axioms ParserEngine.Representation.Masks.field_callback_exact
#print axioms ParserEngine.Representation.Masks.frame_field_exact
#print axioms ParserEngine.Representation.Hash.get_put
#print axioms ParserEngine.Representation.Hash.get_put_other
#print axioms ParserEngine.Representation.Hash.reuse_zero
#print axioms ParserEngine.Representation.Hash.intern_lookup
#print axioms ParserEngine.Representation.Hash.intern_correspondence
#print axioms ParserEngine.Representation.Hash.intern_preserves_index
#print axioms ParserEngine.Representation.Dependencies.reverse_member
#print axioms ParserEngine.Representation.Dependencies.dirty_member
#print axioms ParserEngine.Representation.Dependencies.projected_notification
#print axioms ParserEngine.Representation.Dependencies.clean_outgoing_transport

#print axioms ParserEngine.Representation.Dependencies.built_reverse_member
#print axioms ParserEngine.Representation.Dependencies.native_notification_exact
#print axioms ParserEngine.Representation.Dependencies.metadata_edge_member
#print axioms ParserEngine.Representation.Dependencies.metadata_reader_notified
#print axioms ParserEngine.Representation.Hash.queue_step_refines
#print axioms ParserEngine.Representation.Hash.queue_step_indexed

#print axioms ParserEngine.Representation.Hash.probe_refines_search
#print axioms ParserEngine.Representation.Hash.rehash_lookup

#print axioms ParserEngine.Representation.Dependencies.decoded_shift_notified
#print axioms ParserEngine.Representation.Dependencies.decoded_child_notified

#print axioms ParserEngine.Representation.FrameMaps.generate_exact
#print axioms ParserEngine.Representation.FrameMaps.generated_bit
#print axioms ParserEngine.Representation.FrameMaps.saved_register_live
#print axioms ParserEngine.Representation.FrameMaps.chunk_read
#print axioms ParserEngine.Representation.FrameMaps.emitted_frame_root
#print axioms ParserEngine.Representation.FrameMaps.joined_word_bit
#print axioms ParserEngine.Representation.FrameMaps.cursor_bound
#print axioms ParserEngine.Representation.FrameMaps.cursor_advances
#print axioms ParserEngine.Representation.FrameMaps.cursor_reads_bit
#print axioms ParserEngine.Representation.FrameMaps.cursor_shift
#print axioms ParserEngine.Representation.FrameMaps.cursor_reload
#print axioms ParserEngine.Representation.FrameMaps.signed_literal_bits
#print axioms ParserEngine.Representation.FrameMaps.signed_cursor_reads
#print axioms ParserEngine.Representation.FrameMaps.emitted_chunk_bound
#print axioms ParserEngine.Representation.FrameMaps.emitted_signed_root
#print axioms ParserEngine.Representation.FrameMaps.trailing_signed_word
#print axioms ParserEngine.Representation.FrameMaps.paired_word_reads
#print axioms ParserEngine.Representation.ProbeCoverage.position_bound
#print axioms ParserEngine.Representation.ProbeCoverage.position_next
#print axioms ParserEngine.Representation.ProbeCoverage.position_injective
#print axioms ParserEngine.Representation.ProbeCoverage.position_covers
#print axioms ParserEngine.Representation.ProbeCoverage.observed_slot
#print axioms ParserEngine.Representation.ProbeCoverage.physical_probe_terminates
#print axioms ParserEngine.Representation.ProbeCoverage.hash_start_bound
#print axioms ParserEngine.Representation.ProbeCoverage.generic_start_bound
#print axioms ParserEngine.Representation.ProbeCoverage.physical_step
#print axioms ParserEngine.Representation.ProbeCoverage.scan_refines_probe
#print axioms ParserEngine.Representation.CollectorSchedule.publish_valid
#print axioms ParserEngine.Representation.CollectorSchedule.reserve_valid
#print axioms ParserEngine.Representation.CollectorSchedule.reservation_retains_work
#print axioms ParserEngine.Representation.CollectorSchedule.idle_valid
#print axioms ParserEngine.Representation.CollectorSchedule.wake_valid
#print axioms ParserEngine.Representation.CollectorSchedule.reservation_preserves_pending
#print axioms ParserEngine.Representation.CollectorSchedule.steal_valid
#print axioms ParserEngine.Representation.CollectorSchedule.steal_preserves_pending
#print axioms ParserEngine.Representation.CollectorSchedule.zero_workers_drained
#print axioms ParserEngine.Representation.CollectorSchedule.zero_workers_marking_exact

#print axioms ParserEngine.Representation.CollectorSchedule.counter_bound
#print axioms ParserEngine.Representation.CollectorSchedule.counter_fits

#print axioms ParserEngine.Host.Liveness.before_exact
#print axioms ParserEngine.Host.Liveness.boundary_suffix
#print axioms ParserEngine.Host.Liveness.future_capture_root
#print axioms ParserEngine.Host.Publication.observed_payload
#print axioms ParserEngine.Host.Publication.distinct_reservations
#print axioms ParserEngine.Host.Atomic.tickets_unique
#print axioms ParserEngine.Host.Publication.reservation_visible
#print axioms ParserEngine.Host.Atomic.bounded_tickets_unique
#print axioms ParserEngine.Host.Liveness.future_capture_signed
#print axioms ParserEngine.Host.Publication.end_chunk_safe

#print axioms ParserEngine.Host.Slots.physical_exact
#print axioms ParserEngine.Host.Slots.future_capture_stored
#print axioms ParserEngine.Host.Slots.saved_register_at
#print axioms ParserEngine.Host.Slots.return_variable_at
#print axioms ParserEngine.Host.Acquire.owner_observes
#print axioms ParserEngine.Host.Acquire.plain_poll_counterexample

#print axioms ParserEngine.Host.Slots.declared_adapter
#print axioms ParserEngine.Host.Slots.emitted_byte_address
#print axioms ParserEngine.Host.Slots.internal_size_matches
#print axioms ParserEngine.Host.Acquire.no_smp_inference

#print axioms ParserEngine.Host.Acquire.complete_handoff

#print axioms ParserEngine.Host.Contexts.assignments_valid
#print axioms ParserEngine.Host.Contexts.task_valid
#print axioms ParserEngine.Host.Contexts.join_valid
#print axioms ParserEngine.Host.Cache.flush_refines
#print axioms ParserEngine.Host.Cache.dirty_read_materialized
#print axioms ParserEngine.Host.Ticket.try_success_observes

#print axioms ParserEngine.Host.Cache.trace_unique
#print axioms ParserEngine.Host.Cache.trace_bound
#print axioms ParserEngine.Host.Cache.history_dirty_read
#print axioms ParserEngine.Host.Ticket.stale_cas_success_safe
#print axioms ParserEngine.Host.Ticket.stale_cas_fails

#print axioms ParserEngine.Host.Contexts.constructed_layout
#print axioms ParserEngine.Host.Cache.dirty_spill_signed

#print axioms ParserEngine.Host.Cache.new_stamp_fresh
#print axioms ParserEngine.Host.Ticket.unlock_valid
#print axioms ParserEngine.Host.Ticket.try_preserves_valid

#print axioms ParserEngine.Host.Spill.all_live_retained
#print axioms ParserEngine.Host.Spill.live_read_preserved

#print axioms ParserEngine.Host.Operand.encode_decode
#print axioms ParserEngine.Host.Operand.intern_preserves
#print axioms ParserEngine.Host.Operand.copy_value
#print axioms ParserEngine.Host.Boxing.allocation_identity
#print axioms ParserEngine.Host.Boxing.cache_allocation_root
#print axioms ParserEngine.Host.Contended.no_plain_poll_refinement

#print axioms ParserEngine.Host.Operand.destination_read
#print axioms ParserEngine.Host.Operand.closure_aliases
#print axioms ParserEngine.Host.Boxing.trace_fresh
#print axioms ParserEngine.Host.Contended.ordinary_poll_not_universal

#print axioms ParserEngine.Host.Operand.decoded_copy_readback

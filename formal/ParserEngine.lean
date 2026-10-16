import ParserEngine.Publication
import ParserEngine.RequestAST
import ParserEngine.TableBridge
import ParserEngine.SemanticValues
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

#print axioms ParserEngine.Host.Operand.Table.extract_read
#print axioms ParserEngine.Host.Operand.Table.append_snapshot
#print axioms ParserEngine.Host.CacheExecution.decoded_payload
#print axioms ParserEngine.Host.CacheExecution.dirty_copy
#print axioms ParserEngine.Host.CacheExecution.copy_other
#print axioms ParserEngine.Host.CacheExecution.copy_collection
#print axioms ParserEngine.Host.HeapCheck.overflow_exact
#print axioms ParserEngine.Host.HeapCheck.boxed_register_survives

#print axioms ParserEngine.Host.Boxing.selected_fresh
#print axioms ParserEngine.Host.Boxing.selected_retains

#print axioms ParserEngine.Host.Boxing.trace_retains

#print axioms ParserEngine.Host.CacheExecution.physical_flush

#print axioms ParserEngine.Host.CacheExecution.physical_invalidate
#print axioms ParserEngine.Host.CacheExecution.physical_copy_rows

#print axioms ParserEngine.Host.CacheExecution.reported_size_is_not_occupancy_bound

#print axioms ParserEngine.Host.Boxing.trace_extent
#print axioms ParserEngine.Host.HeapCheck.selected_snapshot_retains

#print axioms ParserEngine.Host.Reservation.trace_shape
#print axioms ParserEngine.Host.Reservation.reported_bound
#print axioms ParserEngine.Host.Reservation.batch_conservation
#print axioms ParserEngine.Host.Reservation.batch_potential
#print axioms ParserEngine.Host.Reservation.prepared_bounds
#print axioms ParserEngine.Host.Reservation.qualified_no_check_safe
#print axioms ParserEngine.Host.Reservation.triggering_flush_fits
#print axioms ParserEngine.Host.Reservation.native_budget_admits_reads
#print axioms ParserEngine.Host.Reservation.accounting_bounds

#print axioms ParserEngine.Host.Reservation.prepared_accounted_reads

#print axioms ParserEngine.Host.Reservation.triggering_dirty_reads

#print axioms ParserEngine.Host.CheckEmission.complete_block

#print axioms ParserEngine.Host.CheckEmission.reset_exact

#print axioms ParserEngine.Host.CheckEmission.labels_exact

#print axioms ParserEngine.Host.CheckEmission.code_length_exact

#print axioms ParserEngine.Host.HeapCheck.completion_counts

#print axioms ParserEngine.Host.HeapCheck.rooted_cycles_preserve

#print axioms ParserEngine.Host.Contended.early_fence_does_not_import_later_release

#print axioms ParserEngine.Host.CheckEmission.materialize_corresponds
#print axioms ParserEngine.Host.CheckEmission.flush_and_check_complete

#print axioms ParserEngine.Host.ClosureEmission.complete_emission

#print axioms ParserEngine.Host.ClosureEmission.event_count

#print axioms ParserEngine.Host.ClosureEmission.high_water_bounds

#print axioms ParserEngine.Host.CollectorCopy.exact_words

#print axioms ParserEngine.Host.CollectorCopy.moved_payload

#print axioms ParserEngine.Host.CollectorCopy.moved_headers

#print axioms ParserEngine.Host.JumpEmission.local_fallthrough_exact

#print axioms ParserEngine.Host.Contended.ordinary_handoff_safe_iff

#print axioms ParserEngine.Host.Forwarding.records_target

#print axioms ParserEngine.Host.Forwarding.retains

#print axioms ParserEngine.Host.Forwarding.scan_bounded

#print axioms ParserEngine.Host.Forwarding.scan_unique

#print axioms ParserEngine.Host.Forwarding.scanned_slot

#print axioms ParserEngine.Host.Forwarding.same_alias_destination

#print axioms ParserEngine.Host.Forwarding.native_split_scan

#print axioms ParserEngine.Host.SwitchEmission.stable_group

#print axioms ParserEngine.Host.SwitchEmission.stable_bucket

#print axioms ParserEngine.Host.SwitchEmission.hashed_case_covered

#print axioms ParserEngine.Host.IfJumpEmission.routing_exact

#print axioms ParserEngine.Host.IfJumpEmission.clear_result_once

#print axioms ParserEngine.Host.SwitchEmission.every_hashed_case_emitted

#print axioms ParserEngine.Host.SwitchEmission.bucket_default_emitted

#print axioms ParserEngine.Host.Forwarding.extent_bounded

#print axioms ParserEngine.Host.LabelEmission.complete_simple_task_entry

#print axioms ParserEngine.Host.Forwarding.native_scan_from_empty

#print axioms ParserEngine.Host.Forwarding.mixed_scan_projection
#print axioms ParserEngine.Host.Forwarding.immediate_slot_retained

#print axioms ParserEngine.Host.Forwarding.header_trace_unique

#print axioms ParserEngine.Host.Forwarding.header_trace_retains

#print axioms ParserEngine.Host.Forwarding.lower_section_admitted

#print axioms ParserEngine.Host.SwitchEmission.capacity_loop_terminates

#print axioms ParserEngine.Host.SwitchEmission.capacity_power

#print axioms ParserEngine.Host.SwitchEmission.mask_matches_generator

#print axioms ParserEngine.Host.SwitchEmission.native_bucket_retains

#print axioms ParserEngine.Host.ApplyFusion.guard_exact

#print axioms ParserEngine.Host.ApplyFusion.destination_exact

#print axioms ParserEngine.Host.ApplyFusion.pair_complete

#print axioms ParserEngine.Host.EntryDispatch.accepts_exactly

#print axioms ParserEngine.Host.EntryDispatch.dispatch_decreases

#print axioms ParserEngine.Host.EntryDispatch.setup_order

#print axioms ParserEngine.Host.ReturnEmission.retained_slot

#print axioms ParserEngine.Host.ReturnEmission.task_continuation

#print axioms ParserEngine.Host.ForwardingCodec.roundtrip

#print axioms ParserEngine.Host.ForwardingCodec.forwarding_body

#print axioms ParserEngine.Host.ForwardingCodec.loser_rollback

#print axioms ParserEngine.Host.ForwardingCodec.failed_cas_is_forwarded

#print axioms ParserEngine.Host.SpecialScan.closure_fields_exact

#print axioms ParserEngine.Host.SpecialScan.continuation_skips_frame

#print axioms ParserEngine.Host.SpecialScan.ordinary_tail_partition

#print axioms ParserEngine.Host.ReturnEmission.source_slots
#print axioms ParserEngine.Host.ReturnEmission.source_return_link

#print axioms ParserEngine.Host.EntryDispatch.source_counts_exact

#print axioms ParserEngine.Host.EntryDispatch.empty_present_keys

#print axioms ParserEngine.Host.SpecialScan.strong_hash_keys_exact

#print axioms ParserEngine.Host.SpecialScan.strong_hash_values_exact

#print axioms ParserEngine.Host.SpecialScan.selected_field_forwarded

#print axioms ParserEngine.Host.SpecialScan.excluded_field_unchanged

#print axioms ParserEngine.Host.SpecialScan.selected_aliases_agree

#print axioms ParserEngine.Host.EntryDispatch.argument_stack_monotone
#print axioms ParserEngine.Host.EntryDispatch.later_source_retained

#print axioms ParserEngine.Host.SpecialScan.selected_fields_in_bounds

#print axioms ParserEngine.Host.ObjectClassification.immediate_never_reads_header

#print axioms ParserEngine.Host.ObjectClassification.missing_reference_header_rejected

#print axioms ParserEngine.Host.ObjectClassification.native_header_dispatch

#print axioms ParserEngine.Host.ObjectClassification.packed_decode

#print axioms ParserEngine.Host.ObjectClassification.header_mask

#print axioms ParserEngine.Host.ObjectClassification.still_pair_scans_both_fields

#print axioms ParserEngine.Host.ObjectClassification.movable_pair_tail_requires_first_mark

#print axioms ParserEngine.Host.ObjectClassification.decoded_fields_in_bounds

#print axioms ParserEngine.Host.ObjectClassification.repeated_still_visit_once

#print axioms ParserEngine.Host.ChunkPublication.copied_body_and_marker

#print axioms ParserEngine.Host.ChunkPublication.enqueue_exact

#print axioms ParserEngine.Host.ChunkPublication.reader_link_precedes_fence

#print axioms ParserEngine.Host.ChunkPublication.late_import_retains_link

#print axioms ParserEngine.Host.ChunkPublication.link_requires_prior_visibility

#print axioms ParserEngine.Host.ChunkPublication.ordinary_reader_gap

#print axioms ParserEngine.Host.ChunkPublication.acquire_before_link

#print axioms ParserEngine.Host.ChunkPublication.snapshot_needs_link

#print axioms ParserEngine.Host.ChunkOrder.unordered_slice_acyclic

#print axioms ParserEngine.Host.ChunkOrder.ordered_stale_cycle

#print axioms ParserEngine.Host.ChunkOrder.ordered_slice_rejects_stale

#print axioms ParserEngine.Host.ChunkOrder.native_reader_boundaries

#print axioms ParserEngine.Host.ChunkOrder.source_link_slice_open

#print axioms ParserEngine.Host.ChunkOrder.source_body_slice_rejects_stale

#print axioms ParserEngine.Host.ObjectClassification.invalid_is_not_permanent

#print axioms ParserEngine.Host.ObjectClassification.packed_word_decode

#print axioms ParserEngine.Host.ObjectClassification.classification_to_scan

#print axioms ParserEngine.Host.ObjectClassification.memory_mask

#print axioms ParserEngine.Host.TicketMachine.prefix_exact
#print axioms ParserEngine.Host.TicketMachine.failed_poll
#print axioms ParserEngine.Host.TicketMachine.poll_block
#print axioms ParserEngine.Host.TicketMachine.failed_prefix
#print axioms ParserEngine.Host.TicketMachine.successful_read
#print axioms ParserEngine.Host.TicketMachine.forwarding_sub
#print axioms ParserEngine.Host.TicketMachine.queue_link_before_fence
#print axioms ParserEngine.Host.TicketMachine.empty_queue_no_link
#print axioms ParserEngine.Host.TicketMachine.prefix_registers
#print axioms ParserEngine.Host.TicketMachine.run_add
#print axioms ParserEngine.Host.TicketMachine.pollRun_eq_run

#print axioms ParserEngine.RawRequest.Entry.scanEnd_le_iff
#print axioms ParserEngine.RawRequest.Entry.token_end_bounded
#print axioms ParserEngine.RawRequest.Entry.config_exact
#print axioms ParserEngine.RawRequest.Entry.constructed_valid
#print axioms ParserEngine.RawRequest.Entry.fresh_fields
#print axioms ParserEngine.RawRequest.Entry.complete_from_constructed

#print axioms ParserEngine.RawRequest.Backend.kernel_preserved
#print axioms ParserEngine.RawRequest.Backend.cached_before_eligibility
#print axioms ParserEngine.RawRequest.Backend.complete_request

#print axioms ParserEngine.TableBridge.lookup_origin
#print axioms ParserEngine.TableBridge.reduction_id_exact
#print axioms ParserEngine.TableBridge.decoded_table_supported
#print axioms ParserEngine.TableBridge.selected_meaning
#print axioms ParserEngine.SemanticValues.field_decodes
#print axioms ParserEngine.SemanticValues.alias_decodes
#print axioms ParserEngine.SemanticValues.materialization_decodes
#print axioms ParserEngine.SemanticValues.actions_decode
#print axioms ParserEngine.SemanticValues.operand_decodes
#print axioms ParserEngine.SemanticValues.reduction_decodes

#print axioms ParserEngine.TableBridge.invalid_first_cell_blocks_fallback

#print axioms ParserEngine.SemanticValues.prepared_operand_decodes
#print axioms ParserEngine.SemanticValues.prepared_reduction_decodes

#print axioms ParserEngine.TableBridge.physical_action_decodes
#print axioms ParserEngine.TableBridge.origins_from_publication

#print axioms ParserEngine.SemanticValues.same_id_views

#print axioms ParserEngine.RequestAST.run_append
#print axioms ParserEngine.RequestAST.step_erases
#print axioms ParserEngine.RequestAST.step_executes
#print axioms ParserEngine.RequestAST.trace_erases
#print axioms ParserEngine.RequestAST.trace_executes
#print axioms ParserEngine.RequestAST.complete_request

#print axioms ParserEngine.Publication.extraction_bounds
#print axioms ParserEngine.Publication.flush_conserves
#print axioms ParserEngine.Publication.step_conserves
#print axioms ParserEngine.Publication.trace_conserves
#print axioms ParserEngine.Publication.event_extraction_roundtrip
#print axioms ParserEngine.Publication.complete_product

#print axioms ParserEngine.Publication.trace_node_counter

#print axioms ParserEngine.Publication.numbered_values

#print axioms ParserEngine.Publication.event_extraction_exact

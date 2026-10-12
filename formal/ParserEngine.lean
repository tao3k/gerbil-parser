import ParserEngine.Recognition
import ParserEngine.Preparation
import ParserEngine.Sequence
import ParserEngine.Reduction
import ParserEngine.Examples
import ParserEngine.Fold
import ParserEngine.OwnedStack

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

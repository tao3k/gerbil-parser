import ParserEngine.LoweringTraversal

/-! The recursive producer, callback publication protocol and retained LeanPoo
construction share one result. Materialization supplies physical records, never
recipes. Its name/terminal correspondence is an explicit remaining obligation. -/
namespace ParserEngine.LoweringTraversalPublication
open LoweringBranches

def pending (rules : List (Nat × LoweringTraversal.Syntax))
    (materialize : Emission (LoweringTraversal.normalized rules) → ProducerAdmission.Production) :
    List LoweringPublication.Pending :=
  (LoweringTraversal.declarations rules).map (fun row => ⟨materialize row, row.recipe, 0⟩)

def metadata (rules : List (Nat × LoweringTraversal.Syntax)) (value : LoweringPublication.Metadata) :
    LoweringPublication.Metadata := { value with rules := LoweringTraversal.normalized rules }

def construct (rules : List (Nat × LoweringTraversal.Syntax))
    (materialize : Emission (LoweringTraversal.normalized rules) → ProducerAdmission.Production)
    (lowering : LoweringPublication.Sources) (sources : ConstructionConsumer.Sources)
    (value : LoweringPublication.Metadata) (observers : LoweringPublication.Observers)
    (traversal : CachedConstruction.Traversal) :=
  LoweringPublication.construct lowering sources (metadata rules value) observers
    (pending rules materialize) traversal

theorem published_recipes (rules : List (Nat × LoweringTraversal.Syntax))
    (materialize : Emission (LoweringTraversal.normalized rules) → ProducerAdmission.Production)
    (value : LoweringPublication.Metadata) (observers : LoweringPublication.Observers)
    (returned : (LoweringPublication.run observers LoweringPublication.initial
      (pending rules materialize)).returned = true) :
    (LoweringPublication.input (metadata rules value)
      (LoweringPublication.run observers LoweringPublication.initial (pending rules materialize)).state).recipes =
      (LoweringTraversal.declarations rules).map Emission.recipe := by
  have same := (LoweringPublication.successful_payloads (metadata rules value) observers
    (pending rules materialize) returned).2
  simpa only [pending, List.map_map, Function.comp_def] using same

/-- Successful construction retains the original dependent factory product;
it cannot substitute a separately validated graph or truncate recursive output. -/
theorem complete (rules : List (Nat × LoweringTraversal.Syntax))
    (materialize : Emission (LoweringTraversal.normalized rules) → ProducerAdmission.Production)
    (lowering : LoweringPublication.Sources) (sources : ConstructionConsumer.Sources)
    (value : LoweringPublication.Metadata) (observers : LoweringPublication.Observers)
    (traversal : CachedConstruction.Traversal)
    (output : Σ input, ConstructionGrammar.Output input traversal)
    (built : construct rules materialize lowering sources value observers traversal = some output) :
    ConstructionFIFO.executeSource sources.builder sources.growth sources.append sources.template
      (ConstructionGrammar.grammar output.1) (ConstructionGrammar.nonempty output.1 output.2.receipt) traversal =
      some ((output.2.product.construction.1.machine, output.2.product.construction.2), output.2.product.actions) ∧
    ConstructionReadiness.Complete (ConstructionGrammar.grammar output.1) traversal output.2.product.layout ∧
    ConstructionReadiness.Visit (ConstructionGrammar.grammar output.1) traversal output.2.product.layout
      output.2.product.construction.1.machine output.2.product.construction.2 :=
  LoweringPublication.complete lowering sources (metadata rules value) observers
    (pending rules materialize) traversal output built

end ParserEngine.LoweringTraversalPublication

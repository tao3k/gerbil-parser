import ParserEngine.LoweringPublication
import ParserEngine.SourceData

namespace ParserEngine.LoweringPublicationWitness
open LoweringPublication

def sources : Sources := ⟨emitterSource, constructorSource, initialSource, finalizerSource⟩

set_option maxRecDepth 8192 in
theorem source_checked : admitted sources = true := by
  simp only [admitted, decide_eq_true_eq]
  exact ⟨rfl, rfl, rfl, rfl⟩

theorem changed_emitter_rejected (replacement : WalkSource.Datum)
    (changed : replacement ≠ Source.emitter) :
    admitted {sources with emitter := replacement} = false := by
  simp [admitted, changed]

theorem changed_initial_rejected (replacement : WalkSource.Datum)
    (changed : replacement ≠ Source.initializer) :
    admitted {sources with initializer := replacement} = false := by
  simp [admitted, changed]

theorem changed_constructor_rejected (replacement : WalkSource.Datum)
    (changed : replacement ≠ Source.constructor) :
    admitted {sources with constructor := replacement} = false := by
  simp [admitted, changed]

theorem changed_finalizer_rejected (replacement : WalkSource.Datum)
    (changed : replacement ≠ Source.finalizer) :
    admitted {sources with finalizer := replacement} = false := by
  simp [admitted, changed]

end ParserEngine.LoweringPublicationWitness

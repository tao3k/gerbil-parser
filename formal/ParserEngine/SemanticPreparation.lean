import LeanPoo.Object.MethodCompilation

/-! Backend methods select kernels during preparation. The request supplies its
own context to those retained kernels. These pure staging laws reuse LeanPoo's
continuation compilation; native C4, mutable callbacks, allocation and the Gerbil
compiler remain separate executable obligations. -/
namespace ParserEngine.SemanticPreparation
open LeanPoo.Object.MethodCombination

abbrev Kernel (Context Value : Type) := Value → Context → Value

def stage {Context Value : Type} (context : Context) (kernel : Kernel Context Value)
    (next : Value → Value) : Value → Value := fun value => next (kernel value context)

def compiled {Context Value : Type} (kernels : List (Kernel Context Value))
    (context : Context) : Value → Value :=
  composeContinuations (kernels.map (stage context)) id

def ordered {Context Value : Type} : List (Kernel Context Value) → Context → Value → Value
  | [], _, value => value
  | kernel :: rest, context, value => ordered rest context (kernel value context)

theorem upstream_lowering {Context Value : Type}
    (kernels : List (Kernel Context Value)) (context : Context) :
    compiled kernels context =
      (kernels.map (stage context)).foldr (fun method next => method next) id :=
  composeContinuations_foldr _ _

theorem compiled_ordered {Context Value : Type}
    (kernels : List (Kernel Context Value)) (context : Context) (value : Value) :
    compiled kernels context value = ordered kernels context value := by
  induction kernels generalizing value with
  | nil => rfl
  | cons kernel rest ih =>
    change compiled rest context (kernel value context) = _
    exact ih _

/-- Capturing methods does not capture request offsets, constructors or tokens. -/
theorem request_context {Context Value : Type}
    (kernels : List (Kernel Context Value)) (left right : Context) (value : Value) :
    compiled kernels left value = ordered kernels left value ∧
    compiled kernels right value = ordered kernels right value :=
  ⟨compiled_ordered _ _ _, compiled_ordered _ _ _⟩

/-- A plan retains its selected callable values, independent of later lookup. -/
structure Plan (Context Value : Type) where
  kernels : List (Kernel Context Value)

def prepare {Backend Action Context Value : Type}
    (select : Backend → Action → Kernel Context Value) (backend : Backend)
    (actions : List Action) : Plan Context Value := ⟨actions.map (select backend)⟩

theorem selected_callables {Backend Action Context Value : Type}
    (select : Backend → Action → Kernel Context Value) (backend : Backend)
    (actions : List Action) (context : Context) (value : Value) :
    compiled (prepare select backend actions).kernels context value =
      ordered (actions.map (select backend)) context value := compiled_ordered _ _ _

#print axioms upstream_lowering
#print axioms compiled_ordered
end ParserEngine.SemanticPreparation

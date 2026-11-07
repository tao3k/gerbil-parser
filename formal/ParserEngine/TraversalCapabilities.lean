import ParserEngine.BitTraversal
import LeanPoo.Functional.CertifiedConsumer
import LeanPoo.Functional.CertifiedView
import LeanPoo.Functional.ContractConsequence

/-! Retain traversal strategies as a LeanPoo capability family. Joint proof,
order consequence, selected projection and proof-aware consumer are reused
from LeanPoo; integer algorithms remain ordinary mathematical functions. -/
namespace ParserEngine.TraversalCapabilities
open LeanPoo.Functional

inductive Mode where
  | direct | field | selected
  deriving DecidableEq

structure Context where
  mask : Nat
  width : Nat
  positive : 0 < width
  population : Nat
  fixnum : Bool

abbrev Value (_ : Context) (_ : Mode) := List Nat
def modes : List Mode := [.direct, .field, .selected]

def retained : Requirements.Factories Context Value modes :=
  (fun c => BitTraversal.direct c.mask,
    (fun c => BitTraversal.fields c.mask c.width (BitTraversal.integerLength c.mask) c.positive (BitTraversal.start c.mask c.width),
      (fun c => BitTraversal.dispatch c.mask c.width c.population c.positive c.fixnum, PUnit.unit)))

def Joint (context : Context) (data : Requirements.Results Value context modes) : Prop :=
  data.1 = BitTraversal.direct context.mask ∧ data.2.1 = BitTraversal.direct context.mask ∧
    data.2.2.1 = BitTraversal.direct context.mask

def certified : Requirements.Certified modes Joint := Requirements.certify retained Joint (by
  intro context
  exact ⟨rfl, BitTraversal.fields_exact context.mask context.width context.positive,
    BitTraversal.dispatch_exact context.mask context.width context.population context.positive context.fixnum⟩)

def Ordered (context : Context) (data : Requirements.Results Value context modes) : Prop :=
  Joint context data ∧ data.2.2.1.Pairwise (· < ·)

def ordered : Requirements.Certified modes Ordered :=
  certified.entails Ordered (by
    intro context data joint
    exact ⟨joint, by rw [joint.2.2]; exact BitTraversal.direct_sorted context.mask⟩)

def selected : Requirements.Certified (Value := Value) [Mode.selected] (fun context data => data.1 = BitTraversal.direct context.mask) :=
  ordered.project [.selected] (by intro key member; have same := List.mem_singleton.mp member; subst key; simp [modes])
    (fun context data => data.1 = BitTraversal.direct context.mask)
    (by intro context joint; exact joint.1.2.2)

/-- Only the selected factory is applied; the broad family remains a joint
proof interface, not an instruction to execute all three traversal paths. -/
def callbacks : (context : Context) → {trace : List Nat // trace = BitTraversal.direct context.mask} :=
  selected.consume (fun _ data proof => ⟨data.1, proof⟩)

theorem callbacks_exact (context : Context) : (callbacks context).val = BitTraversal.direct context.mask :=
  (callbacks context).property

theorem order_retains_factories : ordered.factories = certified.factories :=
  Requirements.Certified.entails_factories _ _ _

/-- Equality is strong enough for any order-sensitive callback consumer,
not merely a commutative set fold or a callback-count observation. -/
theorem consumer_exact (context : Context) (consume : List Nat → Output) :
    consume (callbacks context).val = consume (BitTraversal.direct context.mask) :=
  congrArg consume (callbacks_exact context)

end ParserEngine.TraversalCapabilities

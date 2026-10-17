import ParserEngine.PredictionExtraction
import ParserEngine.TransitionPublication

/-! Construct the name-index law and RHS coverage from first-occurrence
interning of the actual grammar symbol stream. Completed slots add no name. -/
namespace ParserEngine.NameDiscovery
open Grammar StateInterning

def compute (_ : Nat) : List Nat × Unit := ([], ())
def empty : Registry Nat Unit := ⟨[], fun _ => none, fun _ => none, fun _ => none, [], 0⟩

def discover (before : Registry Nat Unit) : List Nat → Registry Nat Unit
  | [] => before
  | key :: rest => discover (intern compute before key).registry rest

theorem empty_valid : Valid compute empty := by
  refine ⟨by simp [empty], ?_, ?_, ?_, rfl, by simp [empty], ?_⟩
  · intro key id; simp [empty]
  · intro id key read; simp [empty] at read
  · intro id key read; simp [empty] at read
  · intro id member; simp [empty] at member

theorem discover_valid (before : Registry Nat Unit) (valid : Valid compute before) (input : List Nat) :
    Valid compute (discover before input) := by
  induction input generalizing before with
  | nil => exact valid
  | cons key rest ih => exact ih _ (intern_valid compute before valid key)

theorem retains (before : Registry Nat Unit) (input : List Nat) (key node : Nat)
    (found : before.index key = some node) : (discover before input).index key = some node := by
  induction input generalizing before with
  | nil => exact found
  | cons next rest ih => exact ih _ (TransitionPublication.intern_retains compute before next key node found)

theorem discovered (before : Registry Nat Unit) (input : List Nat) (key : Nat) (member : key ∈ input) :
    ∃ node, (discover before input).index key = some node := by
  induction input generalizing before with
  | nil => cases member
  | cons head rest ih =>
    rcases List.mem_cons.mp member with same | later
    · subst head
      exact ⟨(intern compute before key).id, retains _ rest key _ (TransitionPublication.intern_lookup compute before key)⟩
    · exact ih _ later

def nameOf : Symbol → Option Nat | .nonterminal key => some key | _ => none

def stream (g : CFG) : List Nat := g.flatMap fun rule => rule.rhs.filterMap nameOf
def catalogue (g : CFG) : Registry Nat Unit := discover empty (stream g)

theorem catalogue_valid (g : CFG) : Valid compute (catalogue g) := discover_valid empty empty_valid (stream g)

theorem name_index (g : CFG) : PredictionExtraction.NameIndex (catalogue g).keys (catalogue g).index :=
  PredictionExtraction.from_registry compute (catalogue g) (catalogue_valid g)

theorem coverage (g : CFG) :
    ∀ rule ∈ g, ∀ key, Symbol.nonterminal key ∈ rule.rhs → key ∈ (catalogue g).keys := by
  intro rule member key symbol
  have input : key ∈ stream g := List.mem_flatMap.mpr ⟨rule, member,
    List.mem_filterMap.mpr ⟨.nonterminal key, symbol, rfl⟩⟩
  obtain ⟨node, found⟩ := discovered empty (stream g) key input
  exact List.mem_of_getElem? ((name_index g).lookup key node |>.mp found)

/-- The physical core stream includes one completed #f slot per production.
Removing absent names gives exactly the grammar discovery input above. -/
def coreStream (g : CFG) : List (Option Nat) := g.flatMap fun rule => rule.rhs.map nameOf ++ [none]

theorem completed_slots_ignored (g : CFG) : (coreStream g).filterMap id = stream g := by
  induction g with
  | nil => rfl
  | cons rule rest ih =>
    change ((rule.rhs.map nameOf ++ [none]) ++ coreStream rest).filterMap id = rule.rhs.filterMap nameOf ++ stream rest
    simp [ih]

end ParserEngine.NameDiscovery

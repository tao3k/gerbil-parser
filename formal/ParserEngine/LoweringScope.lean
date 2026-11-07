import ParserEngine.LoweringNameStore
import ParserEngine.LoweringBranches

/-! Lexical references and captured Scheme values. Physical names are obtained
from the actual allocator, never selected by an owner-to-name encoding. -/
namespace ParserEngine.LoweringScope
open Lowering LoweringNameStore

inductive Reference (reserved : List Nat) : List Owner → Owner → Type where
  | authored (name : Nat) (reservedName : name ∈ reserved) : Reference reserved scope (.named name)
  | here : Reference reserved (owner :: scope) owner
  | there (reference : Reference reserved scope owner) : Reference reserved (other :: scope) owner

inductive Environment {reserved} (store : Store reserved) : List Owner → Type where
  | nil : Environment store []
  | cons (head : Handle store owner) (tail : Environment store scope) : Environment store (owner :: scope)

def Environment.carry {reserved} {before after : Store reserved}
    (history : Names.Trace before.names after.names) : Environment before scope → Environment after scope
  | .nil => .nil
  | .cons head tail => .cons (LoweringNameStore.carry history head) (tail.carry history)

def Reference.resolve {reserved} (store : Store reserved) :
    Reference reserved scope owner → Environment store scope → Handle store owner
  | .authored name member, _ => LoweringNameStore.authored store name member
  | .here, .cons head _ => head
  | .there reference, .cons _ tail => reference.resolve store tail

inductive Operands {reserved} (store : Store reserved) : List Lowering.Symbol → Type where
  | nil : Operands store []
  | terminal (token : Nat) (rest : Operands store rhs) : Operands store (.terminal token :: rhs)
  | nonterminal (handle : Handle store owner) (rest : Operands store rhs) : Operands store (.nonterminal owner :: rhs)

def Operands.physical {reserved} {store : Store reserved} : Operands store rhs → List Grammar.Symbol
  | .nil => []
  | .terminal token rest => .terminal token :: rest.physical
  | .nonterminal handle rest => .nonterminal handle.name :: rest.physical

def Operands.carry {reserved} {before after : Store reserved}
    (history : Names.Trace before.names after.names) : Operands before rhs → Operands after rhs
  | .nil => .nil
  | .terminal token rest => .terminal token (rest.carry history)
  | .nonterminal handle rest => .nonterminal (LoweringNameStore.carry history handle) (rest.carry history)

theorem Operands.carried {reserved} {before after : Store reserved}
    (history : Names.Trace before.names after.names) (values : Operands before rhs) :
    (values.carry history).physical = values.physical := by
  induction values with
  | nil => rfl
  | terminal token rest ih => exact congrArg (List.cons (.terminal token)) ih
  | nonterminal handle rest ih => exact congrArg (List.cons (.nonterminal handle.name)) ih

theorem Operands.meaning {reserved} {store : Store reserved} (values : Operands store rhs) :
    values.physical.map (nativeSymbol (decode store.names)) = rhs := by
  induction values with
  | nil => rfl
  | terminal token rest ih => simpa [physical, nativeSymbol] using congrArg (List.cons (.terminal token)) ih
  | nonterminal handle rest ih =>
      simp only [physical, List.map_cons, nativeSymbol, handle.meaning, ih]

inductive OperandRefs (reserved : List Nat) (scope : List Owner) : List Lowering.Symbol → Type where
  | nil : OperandRefs reserved scope []
  | terminal (token : Nat) (rest : OperandRefs reserved scope rhs) : OperandRefs reserved scope (.terminal token :: rhs)
  | nonterminal (reference : Reference reserved scope owner) (rest : OperandRefs reserved scope rhs) :
      OperandRefs reserved scope (.nonterminal owner :: rhs)

def OperandRefs.resolve {reserved} (store : Store reserved) (env : Environment store scope) :
    OperandRefs reserved scope rhs → Operands store rhs
  | .nil => .nil
  | .terminal token rest => .terminal token (rest.resolve store env)
  | .nonterminal reference rest => .nonterminal (reference.resolve store env) (rest.resolve store env)

structure Packet {reserved} (rules : Rules) (store : Store reserved) where
  logical : LoweringBranches.Emission rules
  lhs : Handle store logical.owner
  rhs : Operands store logical.rhs

def Packet.physical {reserved rules} {store : Store reserved} (packet : Packet rules store) : Grammar.Production :=
  ⟨packet.lhs.name, packet.rhs.physical⟩

def Packet.carry {reserved rules} {before after : Store reserved} (history : Names.Trace before.names after.names)
    (packet : Packet rules before) : Packet rules after :=
  ⟨packet.logical, LoweringNameStore.carry history packet.lhs, packet.rhs.carry history⟩

theorem Packet.carried {reserved rules} {before after : Store reserved} (history : Names.Trace before.names after.names)
    (packet : Packet rules before) : (packet.carry history).physical = packet.physical := by
  simp only [physical, carry, LoweringNameStore.carry, Operands.carried]

theorem Packet.realizes {reserved rules} {store : Store reserved} (packet : Packet rules store) :
    LoweringBranches.Realizes (decode store.names) packet.logical packet.physical :=
  ⟨packet.lhs.meaning, packet.rhs.meaning⟩

theorem realized {reserved rules} {store : Store reserved} (packets : List (Packet rules store)) :
    LoweringBranches.RealizedRows (decode store.names) (packets.map Packet.logical) (packets.map Packet.physical) := by
  induction packets with
  | nil => exact .nil
  | cons head tail ih => exact .cons head.realizes ih

end ParserEngine.LoweringScope

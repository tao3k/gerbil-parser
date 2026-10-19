import ParserEngine.LoweringScope

/-! Lexical allocation and lower-to call/return are distinct instructions.
Return precedes the caller continuation, while captured names survive in the
name store. RealizedRows is an output theorem, never an interpreter input. -/
namespace ParserEngine.LoweringCallStack
open Lowering LoweringNameStore LoweringScope

inductive Code (reserved : List Nat) (rules : Rules) : List Owner → Type where
  | done : Code reserved rules scope
  | emit (row : LoweringBranches.Emission rules) (lhs : Reference reserved scope row.owner)
      (rhs : OperandRefs reserved scope row.rhs) (next : Code reserved rules scope) : Code reserved rules scope
  | bind (authoredOwner : Nat) (owner : Owner) (body : Code reserved rules (owner :: scope))
      (next : Code reserved rules scope) : Code reserved rules scope
  | call (callee : Reference reserved scope owner) (body next : Code reserved rules scope) : Code reserved rules scope

def erase : Code reserved rules scope → List (LoweringBranches.Emission rules)
  | .done => []
  | .emit row _ _ next => row :: erase next
  | .bind _ _ body next => erase body ++ erase next
  | .call _ body next => erase body ++ erase next

/-- Authored allocator owners survive nested helper calls. They cannot be
recovered from the logical expression owner or the final physical helper name. -/
def requests : Code reserved rules scope → List (Nat × Owner)
  | .done => []
  | .emit _ _ _ next => requests next
  | .bind authored owner body next => (authored, owner) :: (requests body ++ requests next)
  | .call _ body next => requests body ++ requests next

inductive Event where
  | claim (authoredOwner index name : Nat) (owner : Owner)
  | declare (name : Nat) (owner : Owner)
  | enter (name : Nat)
  | leave (name : Nat)
  | emit (row : Grammar.Production)

def emissions (events : List Event) : List Grammar.Production :=
  events.filterMap (fun event => match event with | .emit row => some row | _ => none)

/-- The callback stream is independent of production emission, including
helpers for the empty language. Bindings retain their expression owner. -/
def declarations (events : List Event) : List (Nat × Owner) :=
  events.filterMap (fun event => match event with | .declare name owner => some (name, owner) | _ => none)

def claims (events : List Event) : List (Nat × Owner) :=
  events.filterMap (fun event => match event with | .claim _ _ name owner => some (name, owner) | _ => none)

def allocationRequests (events : List Event) : List (Nat × Owner) :=
  events.filterMap (fun event => match event with | .claim authored _ _ owner => some (authored, owner) | _ => none)

def unwind : List Event → List Nat → Option (List Nat)
  | [], stack => some stack
  | .enter name :: rest, stack => unwind rest (name :: stack)
  | .leave name :: rest, actual :: stack => if name = actual then unwind rest stack else none
  | .leave _ :: _, [] => none
  | _ :: rest, stack => unwind rest stack

theorem unwind_append (first last : List Event) (stack : List Nat) :
    unwind (first ++ last) stack = (unwind first stack).bind (unwind last) := by
  induction first generalizing stack with
  | nil => rfl
  | cons event rest ih =>
      cases event with
      | claim authored index name owner => exact ih stack
      | declare name owner => exact ih stack
      | emit row => exact ih stack
      | enter name => exact ih (name :: stack)
      | leave name => cases stack with
        | nil => rfl
        | cons actual stack => by_cases same : name = actual <;> simp [unwind, same, ih]

structure Execution {reserved rules scope} (code : Code reserved rules scope) (before : Store reserved) where
  after : Store reserved
  history : Names.Trace before.names after.names
  packets : List (Packet rules after)
  projection : packets.map Packet.logical = erase code
  events : List Event
  trajectory : emissions events = packets.map Packet.physical
  frames : ∀ stack, unwind events stack = some stack
  requested : allocationRequests events = requests code
  callbacks : declarations events = claims events
  storage : after.names.bindings.reverse = before.names.bindings.reverse ++ declarations events

theorem carried_logical {reserved rules} {before after : Store reserved}
    (history : Names.Trace before.names after.names) (packets : List (Packet rules before)) :
    (packets.map (Packet.carry history)).map Packet.logical = packets.map Packet.logical := by
  simp only [List.map_map, Function.comp_def, Packet.carry]

theorem carried_physical {reserved rules} {before after : Store reserved}
    (history : Names.Trace before.names after.names) (packets : List (Packet rules before)) :
    (packets.map (Packet.carry history)).map Packet.physical = packets.map Packet.physical := by
  simp only [List.map_map, Function.comp_def, Packet.carried]

def execute (candidate : Nat → Nat → Nat)
    (injective : ∀ owner a b, candidate owner a = candidate owner b → a = b)
    (code : Code reserved rules scope) (before : Store reserved) (env : Environment before scope) :
    Execution code before :=
  match code with
  | .done => ⟨before, .refl _, [], rfl, [], rfl, (fun _ => rfl), rfl, rfl, (by simp [declarations])⟩
  | .emit row lhs rhs next =>
      let packet : Packet rules before := ⟨row, lhs.resolve before env, rhs.resolve before env⟩
      let later := execute candidate injective next before env
      { after := later.after
        history := later.history
        packets := packet.carry later.history :: later.packets
        projection := by simpa [erase, Packet.carry, packet] using congrArg (List.cons row) later.projection
        events := .emit packet.physical :: later.events
        trajectory := by
          simp only [emissions, List.filterMap_cons, List.map_cons, Packet.carried]
          exact congrArg (List.cons packet.physical) later.trajectory
        frames := later.frames
        requested := later.requested
        callbacks := later.callbacks
        storage := later.storage }
  | .bind authoredOwner owner body next =>
      let allocation := allocate (candidate authoredOwner) (injective authoredOwner) before owner
      let inner := execute candidate injective body allocation.after
        (.cons allocation.handle (env.carry allocation.history))
      let entered := trans allocation.history inner.history
      let later := execute candidate injective next inner.after (env.carry entered)
      { after := later.after
        history := trans entered later.history
        packets := inner.packets.map (Packet.carry later.history) ++ later.packets
        projection := by
          rw [List.map_append, carried_logical, inner.projection, later.projection]
          rfl
        events := .claim authoredOwner allocation.index allocation.handle.name owner :: .declare allocation.handle.name owner :: (inner.events ++ later.events)
        trajectory := by
          simp only [emissions, List.filterMap_append, List.filterMap_cons, List.map_append, carried_physical]
          change emissions inner.events ++ emissions later.events = _
          rw [inner.trajectory, later.trajectory]
        frames := by intro stack; simpa only [unwind, unwind_append, inner.frames, Option.bind_some] using later.frames stack
        requested := by
          simp only [allocationRequests, List.filterMap_cons, List.filterMap_append]
          change (authoredOwner, owner) :: (allocationRequests inner.events ++ allocationRequests later.events) = _
          rw [inner.requested, later.requested]
          rfl
        callbacks := by
          simp only [declarations, claims, List.filterMap_cons, List.filterMap_append]
          change (allocation.handle.name, owner) :: (declarations inner.events ++ declarations later.events) =
            (allocation.handle.name, owner) :: (claims inner.events ++ claims later.events)
          rw [inner.callbacks, later.callbacks]
        storage := by
          rw [later.storage, inner.storage, allocation.bindings]
          simp [declarations, List.filterMap_append, List.append_assoc] }
  | .call callee body next =>
      let handle := callee.resolve before env
      let inner := execute candidate injective body before env
      let later := execute candidate injective next inner.after (env.carry inner.history)
      { after := later.after
        history := trans inner.history later.history
        packets := inner.packets.map (Packet.carry later.history) ++ later.packets
        projection := by
          rw [List.map_append, carried_logical, inner.projection, later.projection]
          rfl
        events := .enter handle.name :: (inner.events ++ .leave handle.name :: later.events)
        trajectory := by
          simp only [emissions, List.filterMap_cons, List.filterMap_append, List.map_append, carried_physical]
          change emissions inner.events ++ emissions later.events = _
          rw [inner.trajectory, later.trajectory]
        frames := by
          intro stack
          simp only [unwind, unwind_append, inner.frames, Option.bind_some]
          simpa only [unwind, ↓reduceIte] using later.frames stack
        requested := by
          simp only [allocationRequests, List.filterMap_cons, List.filterMap_append]
          change allocationRequests inner.events ++ allocationRequests later.events = _
          rw [inner.requested, later.requested]
          rfl
        callbacks := by
          simp only [declarations, claims, List.filterMap_cons, List.filterMap_append]
          change declarations inner.events ++ declarations later.events = claims inner.events ++ claims later.events
          rw [inner.callbacks, later.callbacks]
        storage := by
          rw [later.storage, inner.storage]
          simp [declarations, List.filterMap_append, List.append_assoc] }

theorem returns_before_continuation (candidate : Nat → Nat → Nat)
    (injective : ∀ owner a b, candidate owner a = candidate owner b → a = b)
    (callee : Reference reserved scope owner) (body next : Code reserved rules scope)
    (before : Store reserved) (env : Environment before scope) :
    let handle := callee.resolve before env
    let inner := execute candidate injective body before env
    let later := execute candidate injective next inner.after (env.carry inner.history)
    (execute candidate injective (.call callee body next) before env).events =
      .enter handle.name :: (inner.events ++ .leave handle.name :: later.events) := by
  rw [execute]

/-- All physical rows, including duplicate logical owners, get their decoded
meaning from allocation and lexical scope. No caller supplies a whole-trace
alignment, per-row decoding equality or successful origin-check premise. -/
theorem realized {reserved rules scope} (code : Code reserved rules scope) (before : Store reserved)
    (execution : Execution code before) :
    LoweringBranches.RealizedRows (decode execution.after.names) (erase code)
      (execution.packets.map Packet.physical) := by
  rw [← execution.projection]
  exact LoweringScope.realized execution.packets

theorem admitted {reserved rules scope} (code : Code reserved rules scope) (before : Store reserved)
    (execution : Execution code before) :
    Names.admitNative rules (decode execution.after.names) (emissions execution.events)
      ((erase code).map LoweringBranches.Emission.recipe) = true := by
  rw [execution.trajectory]
  exact LoweringBranches.realized_native (realized code before execution)

end ParserEngine.LoweringCallStack

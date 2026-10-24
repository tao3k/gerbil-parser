import ParserEngine.Representation
import Init.Data.List.Nat.TakeDrop

/-! Source adapters for Gambit frontend liveness and GCC chunk publication.
Varsets are lists modulo membership. Physical placement and compiler refinement
remain separate obligations. No C sequential-consistency axiom is introduced. -/
namespace ParserEngine.Host
open ParserEngine.Representation
namespace Liveness
inductive Expr where
  | placeholder
  | returnValue
  | ordinary (free : List Nat)
  deriving Repr, DecidableEq

def uses (tail : Bool) (ret : Nat) : Expr → List Nat
  | .placeholder => []
  | .returnValue => if tail then [ret] else []
  | .ordinary free => free

/-- compute-live-vars-at-each-expr; force-live is absent at the final boundary. -/
def before (base force : List Nat) (tail : Bool) (ret : Nat) : List Expr → List Nat
  | [] => base
  | expr :: rest => force ++ uses tail ret expr ++ before base force tail ret rest

def boundaries (base force : List Nat) (tail : Bool) (ret : Nat) : List Expr → List (List Nat)
  | [] => [base]
  | expr :: rest => before base force tail ret (expr :: rest) :: boundaries base force tail ret rest

theorem boundary_count (base force : List Nat) (tail : Bool) (ret : Nat) (exprs : List Expr) :
    (boundaries base force tail ret exprs).length = exprs.length + 1 := by
  induction exprs <;> simp_all [boundaries]

/-- Applies to the actual arg-eval-order sequence, never stale original indices. -/
theorem boundary_suffix (base force : List Nat) (tail : Bool) (ret : Nat)
    (leading suffix : List Expr) :
    (boundaries base force tail ret (leading ++ suffix))[leading.length]? =
      some (before base force tail ret suffix) := by
  induction leading with
  | nil => cases suffix <;> simp [boundaries, before]
  | cons expr rest ih => simpa [boundaries] using ih

theorem before_exact (base force : List Nat) (tail : Bool) (ret var : Nat) (exprs : List Expr) :
    var ∈ before base force tail ret exprs ↔
      var ∈ base ∨ (exprs ≠ [] ∧ var ∈ force) ∨
        ∃ expr ∈ exprs, var ∈ uses tail ret expr := by
  induction exprs with
  | nil => simp [before]
  | cons expr rest ih =>
    simp only [before, List.mem_append, ih, List.mem_cons]
    grind

/-- frame-live? returns the closed list, Scheme-truthy even when empty.
This Bool models truth only, not the value of that list. -/
def frameLive (environment : Nat) (closed live : List Nat) (var : Nat) : Bool :=
  decide (if var = environment then var ∈ live ∨ ∃ captured ∈ closed, captured ∈ live
          else var ∈ live)

theorem captured_environment_live {environment captured : Nat} {closed live : List Nat}
    (stored : captured ∈ closed) (needed : captured ∈ live) :
    frameLive environment closed live environment = true := by
  simp [frameLive, show ∃ c ∈ closed, c ∈ live from ⟨captured, stored, needed⟩]

/-- Arbitrary suffix use selects the physically placed closure environment. -/
theorem future_capture_selected {base force closed : List Nat} {tail : Bool}
    {ret environment captured index : Nat} {suffix : List Expr}
    {expr : Expr} {slots : List Nat}
    (future : expr ∈ suffix) (used : captured ∈ uses tail ret expr)
    (stored : captured ∈ closed) (placed : slots[index]? = some environment) :
    (slots.map (frameLive environment closed
      (before base force tail ret suffix)))[index]? = some true := by
  have needed := (before_exact base force tail ret captured suffix).mpr
    (Or.inr (Or.inr ⟨expr, future, used⟩))
  simp [List.getElem?_map, placed, captured_environment_live stored needed]

/-- Future use -> environment selection -> generated map root. Physical
slot-to-word identity remains explicit rather than inferred from a finite test. -/
theorem future_capture_root {base force closed : List Nat} {tail : Bool}
    {ret environment captured index root : Nat} {suffix : List Expr}
    {expr : Expr} {slots : List Nat} {words : List Roots.Word}
    (future : expr ∈ suffix) (used : captured ∈ uses tail ret expr)
    (stored : captured ∈ closed) (placed : slots[index]? = some environment)
    (read : words[index]? = some (.reference root)) :
    root ∈ Roots.frame (FrameMaps.generate 1 0
      (slots.map (frameLive environment closed (before base force tail ret suffix)))) 0 words :=
  FrameMaps.emitted_frame_root (future_capture_selected future used stored placed) read

/-- The same producer chain crosses serialization and the signed C field cursor.
Neither final bit correctness nor root retention is an input certificate. -/
theorem future_capture_signed {base force closed : List Nat} {tail : Bool}
    {ret environment captured index root width : Nat} {suffix : List Expr}
    {expr : Expr} {slots : List Nat} {words : List Roots.Word}
    (positive : 0 < width) (future : expr ∈ suffix) (used : captured ∈ uses tail ret expr)
    (stored : captured ∈ closed) (placed : slots[index]? = some environment)
    (read : words[index]? = some (.reference root)) :
    let flags := slots.map (frameLive environment closed (before base force tail ret suffix))
    let mask := FrameMaps.generate 1 0 flags
    ((BitVec.ofNat width (Encoding.field mask width (width * (index / width)))).sshiftRight
      (index % width)).getLsbD 0 = true ∧ root ∈ Roots.frame mask 0 words :=
  (FrameMaps.emitted_signed_root positive (future_capture_selected future used stored placed) read).2
end Liveness


namespace Slots
/-- extend-vars pads the front when the requested size grows, or drops the
front when it shrinks. Reversal then gives physical slot order. -/
def extend (empty : α) (slots : List α) (size : Nat) : List α :=
  List.replicate (size - slots.length) empty ++ slots.drop (slots.length - size)

def physical (empty : α) (slots : List α) (size : Nat) : List α :=
  (extend empty slots size).reverse

theorem physical_exact (empty : α) (slots : List α) (size : Nat) :
    physical empty slots size = slots.reverse.take size ++ List.replicate (size - slots.length) empty := by
  by_cases grows : slots.length ≤ size
  · simp [physical, extend, Nat.sub_eq_zero_of_le grows, List.reverse_append,
      List.take_of_length_le (show slots.reverse.length ≤ size by simpa using grows)]
  · have short : size ≤ slots.length := by omega
    have diff : slots.length - (slots.length - size) = size := by omega
    simp [physical, extend, Nat.sub_eq_zero_of_le short, List.reverse_drop, diff]

theorem physical_length (empty : α) (slots : List α) (size : Nat) :
    (physical empty slots size).length = size := by
  rw [physical_exact]
  simp only [List.length_append, List.length_take, List.length_reverse, List.length_replicate]
  omega

theorem physical_retained {empty value : α} {slots : List α} {size index : Nat}
    (inside : index < size) (source : slots.reverse[index]? = some value) :
    (physical empty slots size)[index]? = some value := by
  have old : index < slots.length := by
    by_cases bound : index < slots.length
    · exact bound
    · have absent : slots.reverse[index]? = none := List.getElem?_eq_none (by simp; omega)
      rw [absent] at source
      cases source
  rw [physical_exact, List.getElem?_append_left (by simp; omega)]
  rw [List.getElem?_take_of_lt inside]
  exact source

theorem physical_padding {empty : α} {slots : List α} {size index : Nat}
    (after : slots.length ≤ index) (inside : index < size) :
    (physical empty slots size)[index]? = some empty := by
  rw [physical_exact, List.getElem?_append_right (by simp; omega)]
  simp only [List.length_take, List.length_reverse]
  rw [List.getElem?_replicate_of_lt (by omega)]

/-- Native contexts carry size separately. The target adapter requires their
agreement; actual push/shrink branches preserve it for arbitrary contexts. -/
structure Context (α : Type) where
  declared : Nat
  slots : List α

def Valid (context : Context α) : Prop := context.declared = context.slots.length

def push (context : Context α) (value : α) : Context α :=
  ⟨context.declared + 1, value :: context.slots⟩
def shrink (context : Context α) (size : Nat) : Context α :=
  ⟨size, context.slots.drop (context.declared - size)⟩

theorem push_valid {context : Context α} (valid : Valid context) (value : α) :
    Valid (push context value) := by simpa [Valid, push] using valid

theorem shrink_valid {context : Context α} (valid : Valid context) {size : Nat}
    (inside : size ≤ context.declared) : Valid (shrink context size) := by
  simp only [Valid, shrink, List.length_drop]
  simp only [Valid] at valid
  omega

def extendDeclared (empty : α) (context : Context α) (size : Nat) : List α :=
  List.replicate (size - context.declared) empty ++ context.slots.drop (context.declared - size)

theorem declared_adapter {context : Context α} (valid : Valid context) (empty : α) (size : Nat) :
    (extendDeclared empty context size).reverse = physical empty context.slots size := by
  simp only [Valid] at valid
  simp [extendDeclared, physical, extend, valid]

/-- STK(slot-fp) uses current fp=base-fp; FP_STK subtracts its argument.
Use mathematical word addresses here; pointer validity/alignment are separate. -/
def emittedAddress (base fp slot : Int) : Int := (base - fp) - (slot - fp)
def scannedAddress (base : Int) (index : Nat) : Int := base - (index + 1 : Nat)

theorem emitted_scanned (base fp : Int) (index : Nat) :
    emittedAddress base fp (index + 1 : Nat) = scannedAddress base index := by
  simp [emittedAddress, scannedAddress]; omega

theorem collector_base (base frameSpace : Int) :
    (base - frameSpace) - (-frameSpace) = base := by omega

/-- Pointer subtraction scales by the physical word width. This identity is
independent of that width; validity of the allocated address interval is external. -/
theorem emitted_byte_address (base fp slot bytes : Int) :
    (base - bytes * fp) - bytes * (slot - fp) = base - bytes * slot := by
  rw [Int.mul_sub]
  omega

def align (size boundary : Nat) : Nat := ((size + boundary - 1) / boundary) * boundary

theorem alignment_covers (size boundary : Nat) (positive : 0 < boundary) :
    size ≤ align size boundary ∧ align size boundary < size + boundary := by
  have division := Nat.mod_add_div (size + boundary - 1) boundary
  have residue := Nat.mod_lt (size + boundary - 1) positive
  simp only [align, Nat.mul_comm] at *
  omega

def store (heap : Int → Roots.Word) (address : Int) (value : Roots.Word) : Int → Roots.Word :=
  fun where_ => if where_ = address then value else heap where_

def scanWords (heap : Int → Roots.Word) (base : Int) (size : Nat) : List Roots.Word :=
  (List.range size).map (fun index => heap (scannedAddress base index))

theorem stored_word_read (heap : Int → Roots.Word) (base fp : Int) (size index : Nat)
    (inside : index < size) (value : Roots.Word) :
    (scanWords (store heap (emittedAddress base fp (index + 1 : Nat)) value) base size)[index]? = some value := by
  rw [emitted_scanned]
  simp [scanWords, List.getElem?_map, List.getElem?_range inside, store]

/-- Discharge the previous physical read/placement premises using the source
resize/reverse operation and the generated store/C reader address equations. -/
theorem future_capture_stored {baseLive force closed slots : List Nat} {tail : Bool}
    {ret environment captured index root empty size width : Nat} {suffix : List Liveness.Expr}
    {expr : Liveness.Expr} (heap : Int → Roots.Word) (base fp : Int)
    (positive : 0 < width) (future : expr ∈ suffix) (used : captured ∈ Liveness.uses tail ret expr)
    (closedVar : captured ∈ closed) (inside : index < size)
    (source : slots.reverse[index]? = some environment) :
    let vars := physical empty slots size
    let memory := store heap (emittedAddress base fp (index + 1 : Nat)) (.reference root)
    let mask := FrameMaps.generate 1 0
      (vars.map (Liveness.frameLive environment closed (Liveness.before baseLive force tail ret suffix)))
    ((BitVec.ofNat width (Encoding.field mask width (width * (index / width)))).sshiftRight
      (index % width)).getLsbD 0 = true ∧ root ∈ Roots.frame mask 0 (scanWords memory base size) :=
  Liveness.future_capture_signed positive future used closedVar (physical_retained inside source)
    (stored_word_read heap base fp size index inside (.reference root))

/-- Internal-return order: aligned ordinary frame, saved GVM registers in
register order, return variable, then padding. -/
def internal (empty ret : α) (slots regs : List α) (aligned registers trailing : Nat) : List α :=
  physical empty slots aligned ++ physical empty regs.reverse registers ++ [ret] ++
    List.replicate trailing empty

theorem saved_register_at {empty ret value : α} {slots regs : List α}
    {aligned registers trailing index : Nat} (inside : index < registers)
    (source : regs[index]? = some value) :
    (internal empty ret slots regs aligned registers trailing)[aligned + index]? = some value := by
  simp only [internal, List.append_assoc]
  rw [List.getElem?_append_right (by rw [physical_length]; omega)]
  rw [physical_length]
  simp only [Nat.add_sub_cancel_left]
  rw [List.getElem?_append_left (by rw [physical_length]; exact inside)]
  exact physical_retained inside (by simpa using source)

theorem return_variable_at (empty ret : α) (slots regs : List α) (aligned registers trailing : Nat) :
    (internal empty ret slots regs aligned registers trailing)[aligned + registers]? = some ret := by
  simp only [internal, List.append_assoc]
  rw [List.getElem?_append_right (by rw [physical_length]; omega), physical_length]
  simp only [Nat.add_sub_cancel_left]
  rw [List.getElem?_append_right (by rw [physical_length]; exact Nat.le_refl _), physical_length]
  simp

theorem internal_length (empty ret : α) (slots regs : List α) (aligned registers trailing : Nat) :
    (internal empty ret slots regs aligned registers trailing).length = aligned + registers + 1 + trailing := by
  simp only [internal, List.length_append, physical_length, List.length_cons,
    List.length_nil, List.length_replicate]
/-- Match RETI_CFS_TO_FS using arbitrary alignment and reserve constants, not
only the installed ABI's small example. -/
def frameSpace (size reserve boundary : Nat) : Nat := align (size + reserve) boundary

theorem internal_size_matches (empty ret : α) (slots regs : List α)
    (cfs registers reserve boundary : Nat) (positive : 0 < boundary) :
    (internal empty ret slots regs (align cfs boundary) registers
      (frameSpace (registers + 1) reserve boundary - reserve - (registers + 1))).length =
      align cfs boundary + frameSpace (registers + 1) reserve boundary - reserve := by
  have room := (alignment_covers (registers + 1 + reserve) boundary positive).1
  rw [internal_length]
  unfold frameSpace at *
  omega

end Slots

namespace Contexts
/-- All six fields of make-context; None in a slot is empty-var, not a missing
physical cell. The cleared machine state is distinct from an active size-zero frame. -/
structure Context where
  stack : Slots.Context (Option Nat)
  registers : List (Option Nat)
  closed : List Nat
  poll : Bool × Nat
  entry : Option Nat

def Valid (context : Context) : Prop := Slots.Valid context.stack

def entry (size registers budget : Nat) (closed : List Nat) : Context :=
  ⟨⟨size, List.replicate size none⟩, List.replicate registers none, closed, (false, budget), none⟩

theorem entry_valid (size registers budget : Nat) (closed : List Nat) : Valid (entry size registers budget closed) := by
  simp [Valid, Slots.Valid, entry]

inductive Location where
  | register (index : Nat)
  | stack (slot : Nat)
  deriving DecidableEq, Repr

/-- pcontext assignment is admitted only for an existing empty cell. Source
reg<=nb-regs alone is insufficient at equality; car/drop must also exist. -/
def assign (context : Context) (where_ : Location) (identifier : Nat) : Option Context :=
  match where_ with
  | .register index => if context.registers[index]? = some none then
      some {context with registers := context.registers.set index (some identifier)} else none
  | .stack slot =>
      if 0 < slot ∧ slot ≤ context.stack.declared ∧
          context.stack.slots[context.stack.declared - slot]? = some none then
        some {context with stack := ⟨context.stack.declared,
          context.stack.slots.set (context.stack.declared - slot) (some identifier)⟩}
      else none

theorem assign_valid {before after : Context} (valid : Valid before) {where_ : Location} {identifier : Nat}
    (accepted : assign before where_ identifier = some after) : Valid after := by
  cases where_ with
  | register index =>
    simp only [assign] at accepted
    split at accepted
    · cases accepted; exact valid
    · cases accepted
  | stack slot =>
    simp only [assign] at accepted
    split at accepted
    · cases accepted; simpa [Valid, Slots.Valid] using valid
    · cases accepted

def assignments : Context → List (Location × Nat) → Option Context
  | context, [] => some context
  | context, (where_, identifier) :: rest =>
      (assign context where_ identifier).bind (fun updated => assignments updated rest)

theorem assignments_valid {before after : Context} (valid : Valid before) (writes : List (Location × Nat))
    (accepted : assignments before writes = some after) : Valid after := by
  induction writes generalizing before with
  | nil => simp [assignments] at accepted; cases accepted; exact valid
  | cons step rest ih =>
    simp only [assignments] at accepted
    cases assigned : assign before step.1 step.2 with
    | none => simp [assigned] at accepted
    | some updated =>
      simp only [assigned, Option.bind_some] at accepted
      exact ih (assign_valid valid assigned) accepted

inductive Machine where
  | cleared
  | active (context : Context)

def snapshot : Machine → Option Context
  | .cleared => none
  | .active context => some context

def restore (context : Context) : Machine := .active context

theorem snapshot_restore (context : Context) : snapshot (restore context) = some context := rfl

theorem clear_has_no_frame : snapshot .cleared = none := rfl

/-- Task split and parent return context in gen-future: top/bottom pieces of
one valid physical slot list, with their declared sizes derived separately. -/
def task (context : Context) (start : Nat) : Slots.Context (Option Nat) :=
  ⟨context.stack.declared - start, (context.stack.slots.reverse.drop start).reverse⟩
def parent (context : Context) (start : Nat) : Slots.Context (Option Nat) :=
  ⟨start, context.stack.slots.drop (context.stack.declared - start)⟩

theorem task_valid {context : Context} (valid : Valid context) (start : Nat) : Slots.Valid (task context start) := by
  simp only [Valid, Slots.Valid] at valid
  simp [Slots.Valid, task, valid]

theorem parent_valid {context : Context} (valid : Valid context) {start : Nat}
    (inside : start ≤ context.stack.declared) : Slots.Valid (parent context start) :=
  Slots.shrink_valid valid inside

/-- The join constructor drops unused leading cells from a full-width row.
The displacement producer must still supply exactly that full width. -/
theorem join_valid (rows : List (Option Nat)) (maximum selected : Nat)
    (width : rows.length = maximum) (bound : selected ≤ maximum) :
    Slots.Valid (⟨selected, rows.drop (maximum - selected)⟩ : Slots.Context (Option Nat)) := by
  simp only [Slots.Valid, List.length_drop]
  omega
def taskContext (context : Context) (start ret : Nat) : Context :=
  {context with stack := task context start, registers := some ret :: context.registers.drop 1, closed := []}
def parentContext (context : Context) (start : Nat) : Context :=
  {context with stack := parent context start, registers := []}

/-- Constructors compose from actual entry rows, not arbitrary prevalidated
frame snapshots. Metadata validity beyond slot counts remains separate. -/
inductive Constructed : Context → Prop where
  | entry (size registers budget : Nat) (closed : List Nat) : Constructed (entry size registers budget closed)
  | assign {before after} : Constructed before → (writes : List (Location × Nat)) →
      (accepted : assignments before writes = some after) → Constructed after
  | task {before} : Constructed before → (start ret : Nat) → (inside : start ≤ before.stack.declared) → (hasRegisters : before.registers ≠ []) →
      Constructed (taskContext before start ret)
  | parent {before} : Constructed before → (start : Nat) → (inside : start ≤ before.stack.declared) →
      Constructed (parentContext before start)

theorem constructed_valid {context : Context} (history : Constructed context) : Valid context := by
  induction history with
  | entry size registers budget closed => exact entry_valid size registers budget closed
  | assign _ writes accepted ih => exact assignments_valid ih writes accepted
  | task _ start ret inside hasRegisters ih => exact task_valid ih start
  | parent _ start inside ih => exact parent_valid ih inside

theorem constructed_layout {context : Context} (history : Constructed context) (empty : Option Nat) (size : Nat) :
    (Slots.extendDeclared empty context.stack size).reverse = Slots.physical empty context.stack.slots size :=
  Slots.declared_adapter (constructed_valid history) empty size

end Contexts

namespace Spill
/-- Physical slots in increasing slot order. Liveness includes any closure-env
promotion before selecting a dead destination. -/
structure Cell where
  variableId : Nat
  value : Roots.Word
  deriving DecidableEq, Repr

def choose (live : Nat → Bool) : List Cell → Nat
  | [] => 0
  | cell :: rest => if live cell.variableId then choose live rest + 1 else 0

theorem chosen_bound (live : Nat → Bool) (rows : List Cell) : choose live rows ≤ rows.length := by
  induction rows with
  | nil => simp [choose]
  | cons cell rest ih => simp only [choose, List.length_cons]; split <;> omega

theorem chosen_dead (live : Nat → Bool) (rows : List Cell) {cell : Cell}
    (selected : rows[choose live rows]? = some cell) : live cell.variableId = false := by
  induction rows with
  | nil => simp [choose] at selected
  | cons first rest ih =>
    by_cases held : live first.variableId = true
    · simpa [choose, held] using ih (by simpa [choose, held] using selected)
    · simp only [choose, held, Bool.false_eq_true, ite_false, List.getElem?_cons_zero, Option.some.injEq] at selected
      subst cell
      exact Bool.eq_false_iff.mpr held

/-- lowest-dead-slot chooses an existing dead slot or exactly the next new slot. -/
def save (live : Nat → Bool) (rows : List Cell) (cell : Cell) : List Cell :=
  if choose live rows < rows.length then rows.set (choose live rows) cell else rows ++ [cell]

theorem saved_read (live : Nat → Bool) (rows : List Cell) (cell : Cell) :
    (save live rows cell)[choose live rows]? = some cell := by
  unfold save
  split
  · exact List.getElem?_set_self ‹_›
  · have bound := chosen_bound live rows
    have equal : choose live rows = rows.length := by omega
    simp [equal]

theorem live_read_preserved (live : Nat → Bool) (rows : List Cell) {index : Nat} {cell replacement : Cell}
    (selected : rows[index]? = some cell) (held : live cell.variableId = true) :
    (save live rows replacement)[index]? = some cell := by
  have different : index ≠ choose live rows := by
    intro equal
    have dead := chosen_dead live rows (by simpa [equal] using selected)
    rw [held] at dead
    cases dead
  unfold save
  split
  · rw [List.getElem?_set_ne different.symm]
    exact selected
  · have inside : index < rows.length := by
      by_cases bound : index < rows.length
      · exact bound
      · have absent := List.getElem?_eq_none (Nat.le_of_not_gt bound)
        rw [absent] at selected
        cases selected
    rw [List.getElem?_append_left inside]
    exact selected

theorem live_member_preserved (live : Nat → Bool) (rows : List Cell) {cell : Cell}
    (member : cell ∈ rows) (held : live cell.variableId = true) (replacement : Cell) :
    cell ∈ save live rows replacement := by
  obtain ⟨index, selected⟩ := List.mem_iff_getElem?.mp member
  exact List.mem_iff_getElem?.mpr ⟨index, live_read_preserved live rows selected held⟩

def all (live : Nat → Bool) : List Cell → List Cell → List Cell
  | rows, [] => rows
  | rows, copy :: rest => all live (save live rows copy) rest

/-- Arbitrarily many saves preserve both preexisting live slots and every live
copied register, including when the frame extends repeatedly. -/
theorem all_live_retained (live : Nat → Bool) (rows copies : List Cell) {cell : Cell}
    (held : live cell.variableId = true) (present : cell ∈ rows ∨ cell ∈ copies) : cell ∈ all live rows copies := by
  induction copies generalizing rows with
  | nil => simpa [all] using present
  | cons copy rest ih =>
    apply ih (save live rows copy)
    rcases present with old | copied
    · exact Or.inl (live_member_preserved live rows old held copy)
    · rcases List.mem_cons.mp copied with same | later
      · subst cell
        exact Or.inl (List.mem_iff_getElem?.mpr ⟨choose live rows, saved_read live rows copy⟩)
      · exact Or.inr later
end Spill


end ParserEngine.Host

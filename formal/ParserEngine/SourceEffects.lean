import ParserEngine.WalkSource

/-! Parser-owned mutation witnesses. Primitive vector/storage operations and
Scheme evaluation/compiler behavior are external contracts. These laws describe
normal-return effects, not collector behavior or arbitrary callback purity. -/
namespace ParserEngine.SourceEffects
open WalkSource IteratorEncoding

def quotedData : Datum → Bool
  | .list (.symbol "quote" :: _) => true
  | _ => false
def marked : Datum → Bool
  | .list (.symbol "set!" :: _) | .list (.symbol "vector-set!" :: _) => true
  | _ => false

def sites (tree : Datum) : List Datum :=
  if quotedData tree then [] else
    (if marked tree then [tree] else []) ++
      match tree with
      | .list items => items.flatMap sites
      | _ => []
termination_by sizeOf tree

/-- All syntactically explicit mutation sites, including inside lambdas. Quoted
values are data; malformed mutation forms remain sites and must be rejected. -/
inductive Occurs : Datum → Datum → Prop where
  | root (tree) (openTree : quotedData tree = false) (mutation : marked tree = true) : Occurs tree tree
  | child (items child effect) (openTree : quotedData (.list items) = false)
      (inside : child ∈ items) (nested : Occurs child effect) : Occurs (.list items) effect

theorem occurrence_retained (occurrence : Occurs tree effect) : effect ∈ sites tree := by
  induction occurrence with
  | root tree openTree mutation => cases tree <;> simp [sites, openTree, mutation]
  | child items child effect openTree inside nested ih =>
    simp only [sites, openTree, Bool.false_eq_true, ↓reduceIte, List.mem_append]
    exact Or.inr (List.mem_flatMap.mpr ⟨child, inside, ih⟩)

inductive Effect where
  | assign (name : String) (value : Datum)
  | vectorWrite (target index value : Datum)
  deriving Repr

def decodeEffect : Datum → Option Effect
  | .list [.symbol "set!", .symbol name, value] => some (.assign name value)
  | .list [.symbol "vector-set!", target, index, value] => some (.vectorWrite target index value)
  | _ => none

def effects (tree : Datum) : Option (List Effect) := (sites tree).mapM decodeEffect

theorem decoded_list_sites (entries : List Datum) (checked : entries.mapM decodeEffect = some extracted) :
    ∀ site ∈ entries, ∃ value, decodeEffect site = some value := by
  induction entries generalizing extracted with
  | nil => simp
  | cons head tail ih =>
    cases first : decodeEffect head with
    | none => simp [List.mapM_cons, first] at checked
    | some value =>
      cases rest : tail.mapM decodeEffect with
      | none => simp [List.mapM_cons, first, rest] at checked
      | some values =>
        intro site inside
        rcases List.mem_cons.mp inside with rfl | inside
        · exact ⟨value, first⟩
        · exact ih rest site inside

/-- Successful extraction cannot hide a malformed explicit mutation. -/
theorem extracted_site_decodes (checked : effects tree = some extracted)
    (occurrence : Occurs tree effect) : ∃ decoded, decodeEffect effect = some decoded :=
  decoded_list_sites (sites tree) checked effect (occurrence_retained occurrence)

/-- A store uses object identity, so every alias sees the write. The primitive
must preserve length, bounds and identity; no physical address is assumed. -/
def write (heap : Nat → List α) (object index : Nat) (value : α) : Nat → List α :=
  fun address => if address = object then (heap object).set index value else heap address

theorem write_same (heap : Nat → List α) (object index : Nat) (value : α) :
    write heap object index value object = (heap object).set index value := by simp [write]
theorem write_other (heap : Nat → List α) (object index other : Nat) (value : α) (different : other ≠ object) :
    write heap object index value other = heap other := by simp [write, different]
theorem write_alias (heap : Nat → List α) (object alias index : Nat) (value : α) (same : alias = object) :
    write heap object index value alias = (heap object).set index value := by subst alias; simp [write]
theorem write_length (heap : Nat → List α) (object index address : Nat) (value : α) :
    (write heap object index value address).length = (heap address).length := by
  by_cases same : address = object <;> simp [write, same]

theorem write_read_same (heap : Nat → List α) (object index : Nat) (value : α)
    (bounded : index < (heap object).length) :
    (write heap object index value object)[index]? = some value := by simp [write, bounded]
theorem write_read_other_slot (heap : Nat → List α) (object index other : Nat) (value : α)
    (different : index ≠ other) :
    (write heap object index value object)[other]? = (heap object)[other]? := by
  simp [write, List.getElem?_set_ne different]

def assign (environment : String → α) (name : String) (value : α) : String → α :=
  fun identifier => if identifier = name then value else environment identifier
@[simp] theorem assign_same (environment : String → α) (name : String) (value : α) :
    assign environment name value name = value := by simp [assign]
theorem assign_other (environment : String → α) (name other : String) (value : α) (different : other ≠ name) :
    assign environment name value other = environment other := by simp [assign, different]

/-- Executing the extracted frame-index write commutes with decoding the frame.
The old lexical i supplies the RHS; mutation does not rebind it. -/
theorem chunk_write_decodes (code : Ref) (index delta : Int) (moved : Bool) :
    decodeFrame ((encodeFrame (.chunk code index delta moved)).set 1 (.number (index + 1))) =
      some (.chunk code (index + 1) delta moved) := physical_chunk_write code index delta moved

/-- Repeated assignments to the local frames binding are a left fold of cons.
This is the source for-each order, not an independently supplied reversed list. -/
def appendAssignments (blocks : List Ref) (delta : Int) (moved : Bool) (frames : List Frame) : List Frame :=
  blocks.foldl (fun saved block => .next block delta moved :: saved) frames

theorem append_assignment_refines (blocks : List Ref) (delta : Int) (moved : Bool) (frames : List Frame) :
    appendAssignments blocks delta moved frames = pushBlocks blocks delta moved frames := rfl

/-- LR row indexing: cons during the source for-each, followed by reversal of
all vector slots. State and payload projections may be arbitrary pure functions. -/
def insertRow (state : Row → Nat) (payload : Row → Entry) (store : Nat → List Entry) (row : Row) : Nat → List Entry :=
  fun index => if index = state row then payload row :: store index else store index

def accumulate (state : Row → Nat) (payload : Row → Entry) (rows : List Row) (store : Nat → List Entry) : Nat → List Entry :=
  rows.foldl (insertRow state payload) store

theorem accumulate_slot (state : Row → Nat) (payload : Row → Entry) (rows : List Row)
    (store : Nat → List Entry) (index : Nat) :
    accumulate state payload rows store index =
      ((rows.filter (fun row => index == state row)).map payload).reverse ++ store index := by
  induction rows generalizing store with
  | nil => rfl
  | cons row rows ih =>
    simp only [accumulate, List.foldl_cons]
    rw [show rows.foldl (insertRow state payload) (insertRow state payload store row) index =
      ((rows.filter (fun row => index == state row)).map payload).reverse ++ insertRow state payload store row index from ih _]
    by_cases same : index = state row
    · simp [same, insertRow, List.reverse_cons, List.append_assoc]
    · simp [same, insertRow]

def indexedRows (state : Row → Nat) (payload : Row → Entry) (rows : List Row) : Nat → List Entry :=
  fun index => (accumulate state payload rows (fun _ => []) index).reverse

theorem indexed_rows_order (state : Row → Nat) (payload : Row → Entry) (rows : List Row) (index : Nat) :
    indexedRows state payload rows index = (rows.filter (fun row => index == state row)).map payload := by
  simp [indexedRows, accumulate_slot]

structure ResumeProgram where
  readSlot : Nat
  writeSlot : Nat
  step : Nat
  deltaSlot : Nat
  movedSlot : Nat
  deriving DecidableEq, Repr

def frameSlot : Datum → Option Nat
  | .list [.symbol "vector-ref", .symbol "frame", .integer index] => some index
  | _ => none

/-- Locate the actual pending-pop branch, preserving all operands and the
normal-frame alternative. Other dispatch clauses are not certified by this seam. -/
def resumeBlock : Datum → Option Datum
  | .list [.symbol "def", signature,
      .list [.symbol "let", .symbol "loop", bindings,
        .list (.symbol "cond" :: .list [test,
          .list [.symbol "unless", pendingTest,
            .list [.symbol "let*", locals, .list [.symbol "if", guard, chunk, normal]]]] :: _)]] =>
    if signature = l [s "iterative", s "initial", s "delta", s "moved?"] ∧
      bindings = l [l [s "code", s "initial"], l [s "finish?", b false], l [s "delta", s "delta"],
        l [s "moved?", s "moved?"], l [s "state", b false], l [s "pending", app "quote" [l []]]] ∧
      test = app "not" [s "code"] ∧ pendingTest = app "null?" [s "pending"] ∧
      locals = l [l [s "frame", app "car" [s "pending"]], l [s "next", app "vector-ref" [s "frame", n 0]]] ∧
      guard = app "and" [app "event-code-chunk?" [s "next"], app "integer?" [app "vector-ref" [s "frame", n 1]]] ∧
      normal = app "loop" [s "next", app "vector-ref" [s "frame", n 1], app "vector-ref" [s "frame", n 2],
        app "vector-ref" [s "frame", n 3],
        app "if" [app "=" [app "vector-length" [s "frame"], n 5], app "vector-ref" [s "frame", n 4], b false],
        app "cdr" [s "pending"]]
    then some chunk else none
  | _ => none

def decodeResume (source : Datum) : Option ResumeProgram := do
  let chunk ← resumeBlock source
  match chunk with
  | .list [.symbol "let*", .list [items, .list [.symbol "i", readIndex], last],
      .list [.symbol "unless", .symbol "last?", .list [.symbol "vector-set!", .symbol "frame", .integer writeSlot,
        .list [.symbol "+", .symbol "i", .integer step]]],
      .list [.symbol "loop", child, .boolean false, delta, moved, .boolean false, rest]] =>
    if items ≠ l [s "items", app "event-code-chunk-items" [s "next"]] ∨
      last ≠ l [s "last?", app "=" [app "+" [s "i", n step], app "vector-length" [s "items"]]] ∨
      child ≠ app "vector-ref" [s "items", s "i"] ∨
      rest ≠ app "if" [s "last?", app "cdr" [s "pending"], s "pending"] then none
    else do
      let readSlot ← frameSlot readIndex
      let deltaSlot ← frameSlot delta
      let movedSlot ← frameSlot moved
      pure ⟨readSlot, writeSlot, step, deltaSlot, movedSlot⟩
  | _ => none

def resumeAdmitted (program : ResumeProgram) : Bool := program == ⟨1, 1, 1, 2, 3⟩
def executeResumeWrite (program : ResumeProgram) (code : Ref) (index delta : Int) (moved : Bool) :=
  (encodeFrame (.chunk code index delta moved)).set program.writeSlot (.number (index + (program.step : Int)))

theorem source_chunk_write (source : Datum) (checked : (decodeResume source).map resumeAdmitted = some true)
    (code : Ref) (index delta : Int) (moved : Bool) :
    ∃ program, decodeResume source = some program ∧
      decodeFrame (executeResumeWrite program code index delta moved) = some (.chunk code (index + 1) delta moved) := by
  cases decoded : decodeResume source with
  | none => simp [decoded] at checked
  | some program =>
    have accepted : resumeAdmitted program = true := by simpa [decoded] using checked
    have equal : program = ⟨1, 1, 1, 2, 3⟩ := by simpa [resumeAdmitted] using accepted
    refine ⟨program, rfl, ?_⟩
    subst program
    exact chunk_write_decodes code index delta moved

theorem last_guard_from_suffix (items : List α) (index : Nat) (head : α) (tail : List α)
    (remaining : items.drop index = head :: tail) : index + 1 = items.length ↔ tail = [] := by
  have lengths := congrArg List.length remaining
  simp only [List.length_drop, List.length_cons] at lengths
  constructor
  · intro last
    have zero : tail.length = 0 := by omega
    simpa using zero
  · intro empty
    subst tail
    simp only [List.length_nil] at lengths
    omega

def resumePending (program : ResumeProgram) (code : Ref) (index : Nat) (delta : Int) (moved : Bool)
    (items : List Ref) (rest : List Frame) : List Frame :=
  if index + program.step = items.length then rest
  else .chunk code ((index + program.step : Nat) : Int) delta moved :: rest

/-- Source guard, pop/retain decision and register restoration refine the grouped
packet transition. This is a single admitted resume step, not the whole loop. -/
theorem source_resume_step (source : Datum) (checked : (decodeResume source).map resumeAdmitted = some true)
    (heap : Nat → Option (List Cell)) (code : Ref) (items : List Ref) (start finish : Int)
    (read : readRecord heap code = some (.chunk items start finish))
    (index : Nat) (head : Ref) (tail : List Ref) (remaining : items.drop index = head :: tail)
    (delta : Int) (moved : Bool) (rest : List Frame) (out : List IteratorMachine.Observation) (counter : Nat) :
    ∃ program, decodeResume source = some program ∧
      IteratorExecutor.advance heap ⟨.chunk code (index : Int) delta moved :: rest, out, counter⟩ =
        some ⟨.next head delta moved :: resumePending program code index delta moved items rest, out, counter⟩ := by
  cases decoded : decodeResume source with
  | none => simp [decoded] at checked
  | some program =>
    have accepted : resumeAdmitted program = true := by simpa [decoded] using checked
    have equal : program = ⟨1, 1, 1, 2, 3⟩ := by simpa [resumeAdmitted] using accepted
    refine ⟨program, rfl, ?_⟩
    subst program
    have last := last_guard_from_suffix items index head tail remaining
    cases tail with
    | nil =>
      have final : index + 1 = items.length := last.mpr rfl
      simp [IteratorExecutor.advance, read, remaining, resumePending, final]
    | cons next more =>
      have notFinal : ¬index + 1 = items.length := by simp_all
      simp [IteratorExecutor.advance, read, remaining, resumePending, notFinal, Int.natCast_add]

def iterativeClauses : Datum → Option (List Datum)
  | .list [.symbol "def", _, .list [.symbol "let", .symbol "loop", _, .list (.symbol "cond" :: clauses)]] => some clauses
  | _ => none

def appendSite (source : Datum) : Option Datum := do
  let clauses ← iterativeClauses source
  match clauses.filter (fun clause => match clause with
      | .list (guard :: _) => decide (guard = get "event-code-append?")
      | _ => false) with
  | [.list [_, body]] => some body
  | _ => none

def nextPacket (code : Datum) := app "vector" [code, b false, s "delta", s "moved?"]
def appendSchema := app "let" [l [s "frames", app "cons" [nextPacket (get "event-code-append-tail"), s "pending"]],
  app "for-each" [l [s "lambda", l [s "block"],
    app "set!" [s "frames", app "cons" [nextPacket (s "block"), s "frames"]]], get "event-code-append-blocks"],
  app "loop" [get "event-code-append-prefix", b false, s "delta", s "moved?", b false, s "frames"]]
def appendChecked (source : Datum) : Bool := (appendSite source).map (fun body => decide (body = appendSchema)) == some true

def executeAssignments (blocks : List Ref) (delta : Int) (moved : Bool)
    (environment : String → List Frame) : String → List Frame :=
  blocks.foldl (fun bindings block => assign bindings "frames" (.next block delta moved :: bindings "frames")) environment

theorem assignments_frames (blocks : List Ref) (delta : Int) (moved : Bool) (environment : String → List Frame) :
    executeAssignments blocks delta moved environment "frames" = pushBlocks blocks delta moved (environment "frames") := by
  induction blocks generalizing environment with
  | nil => rfl
  | cons block blocks ih =>
    simpa [executeAssignments, pushBlocks, List.foldl_cons] using
      ih (assign environment "frames" (.next block delta moved :: environment "frames"))

theorem assignments_other (blocks : List Ref) (delta : Int) (moved : Bool) (environment : String → List Frame)
    (name : String) (different : name ≠ "frames") :
    executeAssignments blocks delta moved environment name = environment name := by
  induction blocks generalizing environment with
  | nil => rfl
  | cons block blocks ih =>
    simpa [executeAssignments, List.foldl_cons, assign_other, different] using
      ih (assign environment "frames" (.next block delta moved :: environment "frames"))

theorem source_append_assignment (source : Datum) (checked : appendChecked source = true)
    (blocks : List Ref) (delta : Int) (moved : Bool) (environment : String → List Frame) :
    appendSite source = some appendSchema ∧
    executeAssignments blocks delta moved environment "frames" =
      blocks.reverse.map (fun block => Frame.next block delta moved) ++ environment "frames" := by
  have admitted : (appendSite source).map (fun body => decide (body = appendSchema)) = some true := by
    simpa [appendChecked] using checked
  cases site : appendSite source with
  | none => simp [site] at admitted
  | some body =>
    have equal : body = appendSchema := by simpa [site] using admitted
    exact ⟨by simp [equal], (assignments_frames blocks delta moved environment).trans (pushed_blocks_order ..)⟩

#print axioms occurrence_retained
#print axioms extracted_site_decodes
#print axioms chunk_write_decodes
#print axioms source_chunk_write
#print axioms source_resume_step
#print axioms append_assignment_refines
#print axioms assignments_other
#print axioms source_append_assignment
#print axioms indexed_rows_order
end ParserEngine.SourceEffects

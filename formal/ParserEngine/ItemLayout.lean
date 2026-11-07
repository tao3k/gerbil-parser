import ParserEngine.SourceData

/-! Dense dotted positions from make-item-layout. Each production owns its
completed dot, including an empty RHS. Widths are arbitrary; no grammar-size or
maximum-width enumeration is a premise. Vector primitives, record projection,
allocation and compiler behavior remain external premises. -/
namespace ParserEngine.ItemLayout
open WalkSource

def cells : List Nat → Nat
  | [] => 0
  | width :: rest => width + 1 + cells rest

def offsets (base : Nat) : List Nat → List Nat
  | [] => [base]
  | width :: rest => base :: offsets (base + 1 + width) rest

def owners (first : Nat) : List Nat → List Nat
  | [] => []
  | width :: rest => List.replicate (width + 1) first ++ owners (first + 1) rest

theorem cells_append (before after : List Nat) :
    cells (before ++ after) = cells before + cells after := by
  induction before with
  | nil => simp [cells]
  | cons width rest ih => simp [cells, ih, Nat.add_assoc]

theorem offsets_length (base : Nat) (widths : List Nat) :
    (offsets base widths).length = widths.length + 1 := by
  induction widths generalizing base with
  | nil => simp [offsets]
  | cons width rest ih => simp [offsets, ih, Nat.add_comm]

theorem owners_length (first : Nat) (widths : List Nat) :
    (owners first widths).length = cells widths := by
  induction widths generalizing first with
  | nil => simp [owners, cells]
  | cons width rest ih => simp [owners, cells, ih]

/-- Includes the final sentinel, even for an empty production table. -/
theorem offset_at_split (before after : List Nat) (base : Nat) :
    (offsets base (before ++ after))[before.length]? = some (base + cells before) := by
  induction before generalizing base with
  | nil => cases after <;> simp [offsets, cells]
  | cons width rest ih =>
    simp only [List.cons_append, List.length_cons, offsets, List.getElem?_cons_succ]
    simpa [cells, Nat.add_assoc, Nat.add_comm, Nat.add_left_comm] using ih (base + 1 + width)

/-- The fill pass writes one owner over exactly width+1 consecutive cells. -/
theorem owner_at_split (before after : List Nat) (width dot first : Nat) (valid : dot ≤ width) :
    (owners first (before ++ width :: after))[cells before + dot]? = some (first + before.length) := by
  induction before generalizing first with
  | nil =>
    have inside : dot < width + 1 := by omega
    simp [owners, cells, List.getElem?_append, inside]
  | cons head rest ih =>
    have outside : ¬ cells (head :: rest) + dot < (List.replicate (head + 1) first).length := by
      simp [cells]; omega
    simp only [List.cons_append, owners]
    rw [List.getElem?_append_right (Nat.le_of_not_gt outside)]
    simp only [List.length_replicate]
    have index : cells (head :: rest) + dot - (head + 1) = cells rest + dot := by
      simp [cells]; omega
    rw [index, ih (first + 1)]
    simp only [List.length_cons]
    congr 1 <;> omega

def encode (widths : List Nat) (production dot : Nat) : Option Nat :=
  (offsets 0 widths)[production]?.map (· + dot)

def decode (widths : List Nat) (address : Nat) : Option (Nat × Nat) := do
  let production ← (owners 0 widths)[address]?
  let start ← (offsets 0 widths)[production]?
  pure (production, address - start)

theorem encode_split (before after : List Nat) (width dot : Nat) :
    encode (before ++ width :: after) before.length dot = some (cells before + dot) := by
  simp [encode, offset_at_split]

theorem decode_split (before after : List Nat) (width dot : Nat) (valid : dot ≤ width) :
    decode (before ++ width :: after) (cells before + dot) = some (before.length, dot) := by
  rw [decode, owner_at_split before after width dot 0 valid]
  simp [offset_at_split]

theorem roundtrip (before after : List Nat) (width dot : Nat) (valid : dot ≤ width) :
    (encode (before ++ width :: after) before.length dot).bind (decode (before ++ width :: after)) =
      some (before.length, dot) := by
  rw [encode_split]
  exact decode_split before after width dot valid

/-- Every source offset read and owner write has a derived physical bound. -/
theorem source_bounds (before after : List Nat) (width dot : Nat) (valid : dot ≤ width) :
    before.length < (before ++ width :: after).length ∧
    before.length + 1 < (offsets 0 (before ++ width :: after)).length ∧
    cells before + dot < (owners 0 (before ++ width :: after)).length ∧
    cells before + width + 1 ≤ cells (before ++ width :: after) := by
  simp only [List.length_append, List.length_cons, offsets_length, owners_length, cells_append, cells]
  omega

/-- The source's id=count termination guard agrees with exhaustion of its
remaining table projection; no caller-supplied iteration count is needed. -/
theorem index_guard (before after : List Nat) :
    before.length = (before ++ after).length ↔ after = [] := by
  simp only [List.length_append]
  constructor
  · intro same
    have : after.length = 0 := by omega
    cases after <;> simp_all
  · intro empty
    simp [empty]

/-- Executable fill recurrence with a checked exhaustion case. The source's
strict end guard precedes every write, including the final no-write call. -/
def fill (owner finish : Nat) : Nat → Nat → Option (List (Nat × Nat))
  | 0, cursor => if cursor < finish then none else some []
  | fuel + 1, cursor =>
    if cursor < finish then ((cursor, owner) :: ·) <$> fill owner finish fuel (cursor + 1)
    else some []

theorem fill_exact (owner start count : Nat) :
    fill owner (start + count) count start =
      some ((List.range count).map (fun dot => (start + dot, owner))) := by
  induction count generalizing start with
  | zero => simp [fill]
  | succ count ih =>
    have guard : start < start + (count + 1) := by omega
    have boundary : start + (count + 1) = (start + 1) + count := by omega
    simp only [fill, guard, ↓reduceIte]
    rw [boundary, ih]
    simp [List.range_succ_eq_map, List.map_map, Nat.add_comm, Nat.add_left_comm]

#print axioms offset_at_split
#print axioms owner_at_split
#print axioms roundtrip
#print axioms source_bounds
#print axioms fill_exact
end ParserEngine.ItemLayout

namespace ParserEngine.ItemLayout.Source
open WalkSource
/-- Entire two-pass source declaration; only the completed-dot increment is a parameter. -/
def schema (padding : Nat) : Datum :=
  app "def" [l [s "make-item-layout", s "table", s "terminal-values"],
    app "let*" [l [l [s "count", app "vector-length" [s "table"]],
                     l [s "offsets", app "make-vector" [app "+" [s "count", n 1]]]],
      app "let" [s "index", l [l [s "id", n 0], l [s "offset", n 0]],
        app "vector-set!" [s "offsets", s "id", s "offset"],
        app "if" [app "=" [s "id", s "count"],
          app "let" [l [s "owners", app "make-vector" [s "offset"]],
            app "let" [s "production-loop", l [l [s "id", n 0]],
              app "when" [app "<" [s "id", s "count"],
                app "let" [s "fill",
                  l [l [s "item", app "vector-ref" [s "offsets", s "id"]],
                     l [s "end", app "vector-ref" [s "offsets", app "+" [s "id", n 1]]]],
                  app "when" [app "<" [s "item", s "end"],
                    app "vector-set!" [s "owners", s "item", s "id"],
                    app "fill" [app "+" [s "item", n 1], s "end"]]],
                app "production-loop" [app "+" [s "id", n 1]]]],
            app "make-lr-item-layout" [app "vector-length" [s "terminal-values"], s "offsets", s "owners"]],
          app "index" [app "+" [s "id", n 1],
            app "+" [s "offset", n padding,
              app "length" [app "production-rhs" [app "vector-ref" [s "table", s "id"]]]]]]]]]
def atPath : List Nat → Datum → Option Datum
  | [], source => some source
  | index :: rest, .list elements => elements[index]?.bind (atPath rest)
  | _ :: _, _ => none
def decode (source : Datum) : Option Nat := do
  let .integer padding ← atPath [2, 2, 4, 3, 2, 2] source | none
  if source = schema padding then some padding else none
def admitted (padding : Nat) : Bool := padding == 1
/-- Construct every physical write; the caller does not supply an execution trace. -/
def fillBlocks (owner start : Nat) : List Nat → Option (List (Nat × Nat))
  | [] => some []
  | width :: rest => do
    let block ← fill owner (start + (width + 1)) (width + 1) start
    let tail ← fillBlocks (owner + 1) (start + (width + 1)) rest
    pure (block ++ tail)
def indexPass (id offset : Nat) : List Nat → List (Nat × Nat)
  | [] => [(id, offset)]
  | width :: rest => (id, offset) :: indexPass (id + 1) (offset + 1 + width) rest

theorem index_values (widths : List Nat) (id offset : Nat) :
    (indexPass id offset widths).map Prod.snd = offsets offset widths := by
  induction widths generalizing id offset with
  | nil => rfl
  | cons width rest ih => simp [indexPass, offsets, ih]

theorem fill_blocks_exists (widths : List Nat) (owner start : Nat) :
    ∃ writes, fillBlocks owner start widths = some writes ∧
      writes.map Prod.snd = owners owner widths ∧ writes.length = cells widths := by
  induction widths generalizing owner start with
  | nil => exact ⟨[], rfl, rfl, rfl⟩
  | cons width rest ih =>
    obtain ⟨tail, tailRead, tailOwners, tailLength⟩ := ih (owner + 1) (start + (width + 1))
    refine ⟨(List.range (width + 1)).map (fun dot => (start + dot, owner)) ++ tail, ?_, ?_, ?_⟩
    · simp [fillBlocks, fill_exact, tailRead]
    · simp [List.map_append, List.map_map, Function.comp_def, tailOwners, owners, List.map_const']
    · simp [tailLength, cells]

def execute (source : Datum) (widths : List Nat) : Option (List Nat × List Nat) := do
  let padding ← decode source
  if admitted padding then
    let writes ← fillBlocks 0 0 widths
    pure ((indexPass 0 0 widths).map Prod.snd, writes.map Prod.snd)
  else none

theorem checked_execution (source : Datum)
    (checked : (decode source).map admitted = some true) (widths : List Nat) :
    execute source widths = some (offsets 0 widths, owners 0 widths) := by
  obtain ⟨writes, read, own, _⟩ := fill_blocks_exists widths 0 0
  cases decoded : decode source with
  | none => simp [decoded] at checked
  | some padding =>
    have valid : admitted padding = true := by simpa [decoded] using checked
    simp [execute, decoded, valid, read, own, index_values]
end ParserEngine.ItemLayout.Source

namespace ParserEngine.ItemLayoutWitness
open WalkSource ItemLayout

theorem source_checked : (Source.decode layoutSource).map Source.admitted = some true := by decide +kernel
example : Source.decode (replace (s "owners") (s "offsets") layoutSource) = none := by decide +kernel
example : (Source.decode (replace (n 1) (n 2) layoutSource)).map Source.admitted ≠ some true := by decide +kernel
example : Source.decode (replace (app "<" [s "item", s "end"]) (app "<=" [s "item", s "end"]) layoutSource) = none := by decide +kernel
example : Source.decode (replace (app "vector-length" [s "terminal-values"]) (n 0) layoutSource) = none := by decide +kernel

theorem complete_layout (widths : List Nat) :
    Source.execute layoutSource widths = some (offsets 0 widths, owners 0 widths) :=
  Source.checked_execution layoutSource source_checked widths

theorem checked_roundtrip (before after : List Nat) (width dot : Nat) (valid : dot ≤ width) :
    Source.execute layoutSource (before ++ width :: after) =
      some (offsets 0 (before ++ width :: after), owners 0 (before ++ width :: after)) ∧
    (encode (before ++ width :: after) before.length dot).bind (ItemLayout.decode (before ++ width :: after)) =
      some (before.length, dot) :=
  ⟨complete_layout _, roundtrip before after width dot valid⟩
#print axioms complete_layout
#print axioms checked_roundtrip
end ParserEngine.ItemLayoutWitness

namespace ParserEngine.ItemLayout.Source
/-- Source-assignment semantics over addressable mutable slots. The initial
contents are arbitrary: correct construction must initialize every valid slot. -/
def update (store : Nat → Option Nat) (index value : Nat) : Nat → Option Nat :=
  fun address => if address = index then some value else store address
def writeAll : List (Nat × Nat) → (Nat → Option Nat) → Nat → Option Nat
  | [], store => store
  | (index, value) :: rest, store => writeAll rest (update store index value)
def writeRows (start : Nat) : List Nat → (Nat → Option Nat) → Nat → Option Nat
  | [], store => store
  | value :: rest, store => writeRows (start + 1) rest (update store start value)

theorem fill_addresses (widths : List Nat) (owner start : Nat) (writes : List (Nat × Nat))
    (read : fillBlocks owner start widths = some writes) :
    writes.map Prod.fst = (List.range (cells widths)).map (start + ·) := by
  induction widths generalizing owner start writes with
  | nil => simp [fillBlocks] at read; subst writes; rfl
  | cons width rest ih =>
    cases next : fillBlocks (owner + 1) (start + (width + 1)) rest with
    | none => simp [fillBlocks, fill_exact, next] at read
    | some tail =>
      have same : (List.range (width + 1)).map (fun dot => (start + dot, owner)) ++ tail = writes := by
        simpa [fillBlocks, fill_exact, next] using read
      rw [← same, List.map_append, ih _ _ tail next]
      simp [cells, List.range_add, List.map_append, List.map_map, Function.comp_def, Nat.add_assoc]

theorem dense_write (writes : List (Nat × Nat)) (start : Nat)
    (dense : writes.map Prod.fst = List.range' start writes.length) (store : Nat → Option Nat) :
    writeAll writes store = writeRows start (writes.map Prod.snd) store := by
  induction writes generalizing start store with
  | nil => rfl
  | cons pair rest ih =>
    rcases pair with ⟨index, value⟩
    simp only [List.map_cons, List.length_cons, List.range'_succ] at dense
    have head := (List.cons.inj dense).1
    have tail := (List.cons.inj dense).2
    subst index
    simp only [writeAll, writeRows, List.map_cons]
    exact ih (start + 1) tail (update store start value)

theorem rows_before (values : List Nat) (start index : Nat) (store : Nat → Option Nat)
    (before : index < start) : writeRows start values store index = store index := by
  induction values generalizing start store with
  | nil => rfl
  | cons value rest ih =>
    rw [writeRows, ih (start + 1) (update store start value) (by omega)]
    simp [update, show index ≠ start by omega]

theorem rows_at (values : List Nat) (start index : Nat) (store : Nat → Option Nat)
    (inside : index < values.length) : writeRows start values store (start + index) = values[index]? := by
  induction values generalizing start index store with
  | nil => simp at inside
  | cons value rest ih =>
    cases index with
    | zero =>
      simp only [writeRows, Nat.add_zero, List.getElem?_cons_zero]
      rw [rows_before rest (start + 1) start (update store start value) (by omega)]
      simp [update]
    | succ index =>
      have pos : start + (index + 1) = start + 1 + index := by omega
      simp only [writeRows, List.getElem?_cons_succ]
      rw [pos, ih (start + 1) index (update store start value) (by simpa using inside)]

/-- Actual owner writes initialize the complete finite vector, independently of
its previous contents. The proof uses generated addresses, not map-of-values alone. -/
theorem physical_owner (before after : List Nat) (width dot : Nat) (valid : dot ≤ width)
    (store : Nat → Option Nat) :
    ∃ writes, fillBlocks 0 0 (before ++ width :: after) = some writes ∧
      writeAll writes store (cells before + dot) = some before.length := by
  obtain ⟨writes, read, own, count⟩ := fill_blocks_exists (before ++ width :: after) 0 0
  refine ⟨writes, read, ?_⟩
  have dense : writes.map Prod.fst = List.range' 0 writes.length := by
    rw [count, List.range'_eq_map_range]
    exact fill_addresses _ _ _ _ read
  rw [dense_write writes 0 dense store, own]
  have inside := (source_bounds before after width dot valid).2.2.1
  have indexed := rows_at (owners 0 (before ++ width :: after)) 0 (cells before + dot) store inside
  simpa using indexed.trans (owner_at_split before after width dot 0 valid)
#print axioms physical_owner
end ParserEngine.ItemLayout.Source

namespace ParserEngine.ItemLayout.Source

theorem index_addresses (widths : List Nat) (id offset : Nat) :
    (indexPass id offset widths).map Prod.fst = List.range' id (widths.length + 1) := by
  induction widths generalizing id offset with
  | nil => simp [indexPass]
  | cons width rest ih => simp [indexPass, List.range'_succ, ih]

theorem physical_offset (before after : List Nat) (store : Nat → Option Nat) :
    writeAll (indexPass 0 0 (before ++ after)) store before.length = some (cells before) := by
  have count := congrArg List.length (index_values (before ++ after) 0 0)
  simp only [List.length_map, offsets_length] at count
  have dense : (indexPass 0 0 (before ++ after)).map Prod.fst =
      List.range' 0 (indexPass 0 0 (before ++ after)).length := by
    rw [count]; exact index_addresses _ _ _
  rw [dense_write _ 0 dense store, index_values]
  have inside : before.length < (offsets 0 (before ++ after)).length := by
    simp only [offsets_length, List.length_append]; omega
  have indexed := rows_at (offsets 0 (before ++ after)) 0 before.length store inside
  simpa using indexed.trans (offset_at_split before after 0)
#print axioms physical_offset
end ParserEngine.ItemLayout.Source

namespace ParserEngine.ItemLayoutWitness
open WalkSource ItemLayout
/-- These complete declarations connect the model's projections to native API
syntax. Record reads and nonnegative integer/vector primitives are host premises. -/
theorem codec_sources :
    encodeSource = app "def" [l [s "make-core-item", s "production-id", s "dot", s "layout"],
      app "+" [s "dot", app "vector-ref" [app "lr-item-layout-offsets" [s "layout"], s "production-id"]]] ∧
    ownerSource = app "def" [l [s "core-item-production-id", s "item", s "layout"],
      app "vector-ref" [app "lr-item-layout-owners" [s "layout"], s "item"]] ∧
    dotSource = app "def" [l [s "core-item-dot", s "item", s "layout"],
      app "-" [s "item", app "vector-ref" [app "lr-item-layout-offsets" [s "layout"],
        app "core-item-production-id" [s "item", s "layout"]]]] ∧
    countSource = app "def" [l [s "core-item-count", s "layout"],
      app "vector-length" [app "lr-item-layout-owners" [s "layout"]]] := by decide +kernel

/-- Composed construction, physical storage and coordinate proof. No width or
production-count enumeration, allocation contents, or supplied trace is assumed. -/
theorem physical_coordinates (before after : List Nat) (width dot : Nat)
    (valid : dot ≤ width) (oldOffsets oldOwners : Nat → Option Nat) :
    Source.execute layoutSource (before ++ width :: after) =
      some (offsets 0 (before ++ width :: after), owners 0 (before ++ width :: after)) ∧
    Source.writeAll (Source.indexPass 0 0 (before ++ width :: after)) oldOffsets before.length =
      some (cells before) ∧
    ∃ writes, Source.fillBlocks 0 0 (before ++ width :: after) = some writes ∧
      Source.writeAll writes oldOwners (cells before + dot) = some before.length :=
  ⟨complete_layout _, Source.physical_offset before (width :: after) oldOffsets,
    Source.physical_owner before after width dot valid oldOwners⟩
#print axioms codec_sources
#print axioms physical_coordinates
end ParserEngine.ItemLayoutWitness

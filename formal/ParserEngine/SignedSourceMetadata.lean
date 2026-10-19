import ParserEngine.LoweringEmitClosure

/-! Exact signed metadata is distinct from natural production IDs and sizes.
Use the canonical Int constructors; there is no negative-zero representation. -/
namespace ParserEngine.SignedSourceMetadata
open WalkSource SchemeTableEvaluation LoweringProductionEvaluation

def encode : Int → Datum
  | .ofNat value => .integer value
  | .negSucc magnitude => .negative magnitude

def decode : Datum → Option Int
  | .integer value => some (.ofNat value)
  | .negative magnitude => some (.negSucc magnitude)
  | _ => none

theorem roundtrip (value : Int) : decode (encode value) = some value := by cases value <;> rfl

theorem injective (left right : Int) (same : encode left = encode right) : left = right := by
  have result := congrArg decode same
  simpa [roundtrip] using result

/-- The renderer's (- -1 value) is exactly the canonical negative magnitude. -/
theorem renderer_magnitude (magnitude : Nat) :
    (-1 : Int) - Int.negSucc magnitude = (magnitude : Int) := by omega

/-- Signed metadata does not accidentally become a mutable cell reference. -/
theorem resolves (read : env name = some (datumValue (encode rank))) :
    resolve env heap name = some (datumValue (encode rank)) := resolve_datum read

/-- The actual native constructor transports either sign without changing
its metadata or reinterpreting it as a production identifier. -/
theorem constructor (listPrimitive : env "list" = none) (id : Nat)
    (lhs direction : String) (rhs : List Value) (action : Datum) (rank : Int) :
    Invoke heap (function env)
      [.natural id, .symbol lhs, .list rhs, datumValue action,
        datumValue (.list [.symbol direction, encode rank])]
      (record id ⟨lhs, rhs, action, .list [.symbol direction, encode rank]⟩) heap :=
  invoke listPrimitive id ⟨lhs, rhs, action, .list [.symbol direction, encode rank]⟩

end ParserEngine.SignedSourceMetadata

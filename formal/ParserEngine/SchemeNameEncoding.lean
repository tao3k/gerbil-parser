import ParserEngine.ConstructionEncoding
import ParserEngine.LoweringClaimEvaluation

/-! Operational name-id lookup over canonical symbol identities. This connects
the native loop to the existing checked directory encoding; it does not infer
Scheme identity from arbitrary printed names or admit uninterned source names. -/
namespace ParserEngine.SchemeNameEncoding
open WalkSource SchemeTableEvaluation LoweringClaimEvaluation

def body : Datum := app "cond" [
  .list [app "null?" [s "rest"], app "error" [.text "missing grammar name", s "name"]],
  .list [app "eq?" [s "name", app "car" [s "rest"]], s "index"],
  .list [s "else", app "loop" [app "cdr" [s "rest"], app "+" [s "index", n 1]]]]
def definition : Procedure := ⟨[s "rest", s "index"], [body]⟩
def sourceBody : Datum := app "let" [s "loop", .list [
  .list [s "rest", s "names"], .list [s "index", n 0]], body]

theorem extracted : procedure source = some ("name-id", ⟨[s "name"], [sourceBody]⟩) := rfl
def environment (captured : Environment) (keys : List String) (index : Nat) : Environment :=
  extend (extend (tie [("loop", definition)] captured) "rest" (.list (keys.map Value.symbol))) "index" (.natural index)

def Prelude (env : Environment) : Prop :=
  env "null?" = none ∧ env "eq?" = none ∧ env "car" = none ∧ env "cdr" = none ∧ env "+" = none

theorem scan_eval (name : captured "name" = some (.symbol query)) (prelude : Prelude captured)
    (found : ConstructionEncoding.lookup keys query = some offset) :
    Eval (environment captured keys start) heap body (.natural (start + offset)) heap := by
  induction keys generalizing start offset with
  | nil => simp [ConstructionEncoding.lookup] at found
  | cons first rest ih =>
    have notEmpty : Eval (environment captured (first :: rest) start) heap
        (app "null?" [s "rest"]) (.boolean false) heap := by
      apply Eval.apply (values := [.list ((first :: rest).map Value.symbol)])
      · exact .cons (.variable (by simp [environment, extend, resolve])) .nil
      · rfl
      · simpa [environment, extend, tie, List.lookup] using prelude.1
    have firstRead : Eval (environment captured (first :: rest) start) heap
        (app "car" [s "rest"]) (.symbol first) heap := by
      apply Eval.apply (values := [.list ((first :: rest).map Value.symbol)])
      · exact .cons (.variable (by simp [environment, extend, resolve])) .nil
      · rfl
      · simpa [environment, extend, tie, List.lookup] using prelude.2.2.1
    have queryRead : Eval (environment captured (first :: rest) start) heap (s "name") (.symbol query) heap := by
      apply Eval.variable
      simp [environment, extend, tie, List.lookup, resolve, name]
    have compared : Eval (environment captured (first :: rest) start) heap
        (app "eq?" [s "name", app "car" [s "rest"]]) (.boolean (query == first)) heap :=
      .apply (.cons queryRead (.cons firstRead .nil)) rfl
        (by simpa [environment, extend, tie, List.lookup] using prelude.2.1)
    apply Eval.cond rfl
    apply Eval.conditionalFalse notEmpty rfl
    by_cases same : first = query
    · subst first
      have zero : offset = 0 := by simpa [ConstructionEncoding.lookup, List.idxOf?_cons] using found.symm
      subst offset
      apply Eval.conditionalTrue compared (by simp [truthy])
      exact .begin (.single (.variable (by simp [environment, extend, resolve])))
    · have miss : (first == query) = false := Bool.eq_false_iff.mpr (fun equal => same (eq_of_beq equal))
      have backwards : (query == first) = false := Bool.eq_false_iff.mpr (fun equal => same (eq_of_beq equal).symm)
      simp only [ConstructionEncoding.lookup, List.idxOf?_cons, miss] at found
      obtain ⟨next, present, equal⟩ := Option.map_eq_some_iff.mp found
      have indexEq : next + 1 = offset := equal
      subst offset
      apply Eval.conditionalFalse compared (by simp [backwards, truthy])
      apply Eval.begin
      apply Sequence.single
      apply Eval.call (function := .recursive [("loop", definition)] "loop" captured)
        (values := [.list (rest.map Value.symbol), .natural (start + 1)]) rfl
        (.variable (by simp [environment, extend, tie, List.lookup, resolve]))
      · refine .cons (.apply (values := [.list ((first :: rest).map Value.symbol)])
          (.cons (.variable (by simp [environment, extend, resolve])) .nil) rfl ?_)
          (.cons (.apply (values := [.natural start, .natural 1])
            (.cons (.variable (by simp [environment, extend, resolve])) (.cons .natural .nil)) rfl ?_) .nil)
        · simpa [environment, extend, tie, List.lookup] using prelude.2.2.2.1
        · simpa [environment, extend, tie, List.lookup] using prelude.2.2.2.2
      · apply Invoke.recursive (definition := definition) rfl rfl
        apply Sequence.single
        simpa only [Nat.add_assoc, Nat.add_comm 1 next, environment] using ih present (start := start + 1)

theorem source_eval (names : captured "names" = some (.list (keys.map Value.symbol)))
    (name : captured "name" = some (.symbol query)) (prelude : Prelude captured)
    (found : ConstructionEncoding.lookup keys query = some offset) :
    Eval captured heap sourceBody (.natural offset) heap := by
  apply Eval.namedLoop (names := ["rest", "index"]) (values := [.list (keys.map Value.symbol), .natural 0]) rfl
  · exact .cons (.variable (by simp [resolve, names])) (.cons .natural .nil)
  · rfl
  · exact .single (by simpa [environment, definition] using scan_eval name prelude found (start := 0))

theorem code_preserves_identity (keys : List String) (left : ConstructionEncoding.lookup keys first = some code)
    (right : ConstructionEncoding.lookup keys second = some code) : first = second :=
  ConstructionEncoding.lookup_injective keys first second code left right

theorem every_return (names : captured "names" = some (.list (keys.map Value.symbol)))
    (name : captured "name" = some (.symbol query)) (prelude : Prelude captured)
    (found : ConstructionEncoding.lookup keys query = some offset)
    (returned : Eval captured heap sourceBody value after) : value = .natural offset ∧ after = heap :=
  returned.deterministic (source_eval names name prelude found)

end ParserEngine.SchemeNameEncoding

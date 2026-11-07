import ParserEngine.BuildEnvironment

/-! Sequential snapshot hashing may retain one scratch buffer, while each file
starts a fresh digest state. The update consumes exactly the valid read prefix.
Digest implementation and synchronous byte consumption remain host premises. -/
namespace ParserEngine.PreparationScratch

def refill (scratch chunk : List Byte) : List Byte := chunk ++ scratch.drop chunk.length

theorem read_prefix (scratch chunk : List Byte) : (refill scratch chunk).take chunk.length = chunk := by
  simp [refill]

theorem refill_length (fits : chunk.length ≤ scratch.length) :
    (refill scratch chunk).length = scratch.length := by
  simp only [refill, List.length_append, List.length_drop]
  omega

def stream (update : Digest → List Byte → Digest) (state : Digest) (scratch : List Byte) :
    List (List Byte) → Digest × List Byte
  | [] => (state, scratch)
  | chunk :: chunks =>
      let next := refill scratch chunk
      stream update (update state (next.take chunk.length)) next chunks

/-- All chunk boundaries and contents, including empty and short final reads. -/
theorem stream_digest (update : Digest → List Byte → Digest) (state : Digest)
    (scratch : List Byte) (chunks : List (List Byte)) :
    (stream update state scratch chunks).1 = chunks.foldl update state := by
  induction chunks generalizing state scratch with
  | nil => rfl
  | cons chunk chunks ih => simpa [stream, read_prefix] using ih (update state chunk) (refill scratch chunk)

theorem stream_capacity (update : Digest → List Byte → Digest) (state : Digest)
    (sized : scratch.length = capacity) (fits : ∀ chunk ∈ chunks, chunk.length ≤ capacity) :
    (stream update state scratch chunks).2.length = capacity := by
  induction chunks generalizing state scratch with
  | nil => exact sized
  | cons chunk chunks ih =>
      apply ih
      · exact (refill_length (by simpa [sized] using fits chunk (by simp))).trans sized
      · exact fun item member => fits item (by simp [member])

def batch (update : Digest → List Byte → Digest) (fresh : Digest) (scratch : List Byte) :
    List (List (List Byte)) → List Digest × List Byte
  | [] => ([], scratch)
  | file :: files =>
      let current := stream update fresh scratch file
      let rest := batch update fresh current.2 files
      (current.1 :: rest.1, rest.2)

/-- Scratch history cannot concatenate files or change their ordered digests.
Fresh digest state is essential; scratch bytes themselves need not be cleared. -/
theorem batch_digests (update : Digest → List Byte → Digest) (fresh : Digest)
    (scratch : List Byte) (files : List (List (List Byte))) :
    (batch update fresh scratch files).1 = files.map (fun chunks => chunks.foldl update fresh) := by
  induction files generalizing scratch with
  | nil => rfl
  | cons file files ih => simp [batch, stream_digest, ih]

end ParserEngine.PreparationScratch

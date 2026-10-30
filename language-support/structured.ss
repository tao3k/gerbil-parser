;;; Language recipes declare values; the engine binds and validates their behavior.
(import (only-in :gerbil-parser/src/runtime/structured
 StructuredProofPolicy. StructuredProofPolicyContract bind-structured-proof-policy))
(export StructuredProofPolicy. StructuredProofPolicyContract bind-structured-proof-policy)

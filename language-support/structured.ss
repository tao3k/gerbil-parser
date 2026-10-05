;;; Language recipes declare values; the engine binds and validates their behavior.
(import (only-in :gerbil-parser/src/runtime/structured
 StructuredLexemeProfile. StructuredLexemeProfileContract deflanguage-module-scanner
 StructuredProofPolicy. StructuredProofPolicyContract bind-structured-proof-policy))
(export StructuredLexemeProfile. StructuredLexemeProfileContract deflanguage-module-scanner
 StructuredProofPolicy. StructuredProofPolicyContract bind-structured-proof-policy)

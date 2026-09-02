;;; -*- Gerbil -*-
;;; Version-pinned native HCL files embedded by declarative macro expansion.

(import ../reference/hcl-v2-24
        ./fixture)
(export hcl-v2-24-representative-fixture
        hcl-v2-24-invalid-fixtures)

(defsyntax-fixture hcl-v2-24-representative-fixture
  (identity "hcl/v2.24.0/representative"
            "HCL native syntax"
            "v2.24.0"
            +hcl-syntax-contract-v1+)
  (source "../../t/corpus/hcl/v2.24.0/representative.hcl")
  (expect accepted HclFile
          (Block Attribute TupleExpression ObjectExpression
                 TraversalExpression)))

(defsyntax-fixture hcl-v2-24-missing-equals-fixture
  (identity "hcl/v2.24.0/invalid/missing-equals"
            "HCL native syntax"
            "v2.24.0"
            +hcl-syntax-contract-v1+)
  (source "../../t/corpus/hcl/v2.24.0/invalid/missing-equals.hcl")
  (expect rejected #f ()))

(defsyntax-fixture hcl-v2-24-unterminated-label-fixture
  (identity "hcl/v2.24.0/invalid/unterminated-label"
            "HCL native syntax"
            "v2.24.0"
            +hcl-syntax-contract-v1+)
  (source "../../t/corpus/hcl/v2.24.0/invalid/unterminated-label.hcl")
  (expect rejected #f ()))

(defsyntax-fixture hcl-v2-24-malformed-tuple-fixture
  (identity "hcl/v2.24.0/invalid/malformed-tuple"
            "HCL native syntax"
            "v2.24.0"
            +hcl-syntax-contract-v1+)
  (source "../../t/corpus/hcl/v2.24.0/invalid/malformed-tuple.hcl")
  (expect rejected #f ()))

(def hcl-v2-24-invalid-fixtures
  (list hcl-v2-24-missing-equals-fixture
        hcl-v2-24-unterminated-label-fixture
        hcl-v2-24-malformed-tuple-fixture))

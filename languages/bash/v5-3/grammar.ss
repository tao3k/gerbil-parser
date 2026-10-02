;;; -*- Gerbil -*-
;;; Bash 5.3 source syntax identity and stateful scanner ownership.

(import (only-in :gerbil-parser/src/language/source
                 declare-source-language)
        (only-in ./parser-core parse-bash-core)
        (only-in ./scanner bash-scan))
(export +bash-v5-3-version+
        +bash-v5-3-syntax-contract+
        bash-v5-3-source-language)

(def +bash-v5-3-version+ "5.3")
(def +bash-v5-3-syntax-contract+ "bash-5.3-structured-source.v1")

(def bash-v5-3-source-language
  (declare-source-language
   "bash" +bash-v5-3-version+ +bash-v5-3-syntax-contract+
   bash-scan parse-bash-core))

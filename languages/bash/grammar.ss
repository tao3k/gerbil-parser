;;; -*- Gerbil -*-
;;; Bash 5.3 source syntax identity and stateful scanner ownership.

(import (only-in :gerbil-parser/language-support
                 declare-source-language)
        (only-in ./parser-core parse-bash-core)
        (only-in ./scanner bash-scan))
(export +bash-version+
        +bash-syntax-contract+
        bash-source-language)

(def +bash-version+ "5.3")
(def +bash-syntax-contract+ "bash-5.3-structured-source.v1")

(def bash-source-language
  (declare-source-language
   "bash" +bash-version+ +bash-syntax-contract+
   bash-scan parse-bash-core))

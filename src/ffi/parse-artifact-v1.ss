;;; -*- Gerbil -*-
;;; Builtin convenience facade; downstream packs use language-artifact-codec.
(import (prefix-in ./language-artifact-codec codec-)
        (only-in ../../languages/gql/parser
                 gql-language-grammar parse-gql)
        (only-in ../../languages/cypher/parser
                 opencypher-language-grammar parse-opencypher))
(export native-abi-version native-error-payload
        native-descriptor-payload native-parse-binary-payload)
(def native-abi-version codec-native-abi-version)
(def native-error-payload codec-native-error-payload)
(def +gql-native-language+
  (delay
    (codec-make-native-language-context "gql" gql-language-grammar
                                  parse-gql)))

(def +cypher-native-language+
  (delay
    (codec-make-native-language-context "cypher" opencypher-language-grammar
                                  parse-opencypher)))

(def (resolve-native-language language)
  (cond
   ((string=? language "gql") (force +gql-native-language+))
   ((string=? language "cypher") (force +cypher-native-language+))
   (else (error "unsupported native parser language" language))))


(def (native-descriptor-payload language-id)
  (codec-native-descriptor-payload (resolve-native-language language-id)))
(def (native-parse-binary-payload language-id source)
  (codec-native-parse-binary-payload (resolve-native-language language-id) source))

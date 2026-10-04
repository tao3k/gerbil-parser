;;; -*- Gerbil -*-
;;; Public admission boundary for compiled language packs and native ABI v2.
(import (only-in ./src/ffi/language-handles register-native-language! release-native-language!))
(export register-native-language! release-native-language!)

(import (only-in ./src/compiler/native-language generate-native-language-pack))
(export generate-native-language-pack)

;;; -*- Gerbil -*-
;;; Public admission boundary for compiled language packs and the native language ABI.
(import (only-in ./src/ffi/language-handles register-language! release-language!))
(export register-language! release-language!)

(import (only-in ./src/compiler/language-pack generate-language-pack))
(export generate-language-pack)

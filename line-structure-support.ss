;;; -*- Gerbil -*-
;;; Public AOT support for Scheme-authored contextual line grammars.

(import (only-in ./src/compiler/line-structure-rowan
                 generate-line-structure-rowan-module))
(export generate-line-structure-rowan-module)

;;; -*- Gerbil -*-
;;; Public Scheme-owned event algorithm boundary for Rust/Rowan AOT.

(import (only-in ./src/compiler/event-fold-aot
                 define-event-fold-parser run-event-fold
                 event-fold-ir-json))
(export define-event-fold-parser run-event-fold event-fold-ir-json)

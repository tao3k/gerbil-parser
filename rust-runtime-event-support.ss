;;; -*- Gerbil -*-
;;; Public Scheme-owned event algorithm boundary for Rust AOT.

(import (only-in ./src/compiler/event-fold-aot
                 define-event-fold-parser run-event-fold
                 event-fold-ir-json)
        (only-in ./src/modules/parser/source-fragment-objects
                 make-source-delimited-fragment make-source-first-split
                 make-source-reference-scan)
        (only-in ./src/modules/parser/source-fragment-types
                 source-delimited-fragment? source-first-split?
                 source-reference-scan?)
        (only-in ./src/modules/parser/source-fragment-funs
                 source-delimited-fragment-initial
                 source-delimited-fragment-forms
                 source-first-split-initial source-first-split-forms
                 source-reference-scan-initial source-reference-scan-forms))
(export define-event-fold-parser run-event-fold event-fold-ir-json
        make-source-delimited-fragment source-delimited-fragment?
        source-delimited-fragment-initial source-delimited-fragment-forms
        make-source-first-split source-first-split?
        source-first-split-initial source-first-split-forms
        make-source-reference-scan source-reference-scan?
        source-reference-scan-initial source-reference-scan-forms)

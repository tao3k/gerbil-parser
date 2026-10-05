;;; -*- Gerbil -*-
;;; Minimal public grammar-authoring facade for the Rust/Rowan AOT product.

(import (only-in ./src/grammar/lexical-algebra deftext-profile)
        (only-in ./src/language/grammar deflanguage))
(export deftext-profile
        deflanguage)

;;; Internal compiler-product inspection for engine tests and Rust generators.
;;; These are derived from the admitted source vocabulary, never author profiles.
(import (only-in :clan/poo/object .ref)
        (only-in :gerbil-parser/languages/bash/grammar bash-syntax)
        (only-in :gerbil-parser/languages/bash/parser bash-source-language)
        (only-in :gerbil-parser/src/language/source source-syntax-strategy))
(export bash-source-language bash-word-regions bash-command-scanner bash-results
        bash-parts bash-commands bash-simple-binding bash-parameter-binding bash-assignment-binding)
(def products (source-syntax-strategy bash-syntax))
(def bash-word-regions (.ref products 'regions))
(def bash-command-scanner (.ref products 'scanner))
(def bash-results (.ref products 'results))
(def bash-parts (.ref products 'parts))
(def bash-commands (.ref products 'commands))
(def bash-simple-binding (cdr (assq 'simple (.ref bash-parts 'bindings))))
(def bash-parameter-binding (cdr (assq 'parameter (.ref bash-parts 'bindings))))
(def bash-assignment-binding (cdr (assq 'assignment (.ref bash-parts 'bindings))))

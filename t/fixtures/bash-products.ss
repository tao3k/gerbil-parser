;;; Parser and compiler products share the same admitted Bash declarations.
(import (only-in :gerbil-parser/languages/bash/parser bash-source-language)
        (only-in ./bash-compiler-products
                 bash-word-regions bash-command-scanner bash-results
                 bash-parts bash-commands bash-simple-binding
                 bash-parameter-binding bash-assignment-binding))
(export bash-source-language bash-word-regions bash-command-scanner bash-results
        bash-parts bash-commands bash-simple-binding
        bash-parameter-binding bash-assignment-binding)

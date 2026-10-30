;;; Public Scheme grammar frontend without pinned-source adapter dependencies.
(import (only-in ../src/language/grammar deflanguage defgrammar-syntax)
        (only-in ../src/grammar/lexical-algebra deftext-profile))
(export deflanguage defgrammar-syntax deftext-profile)

;;; Public grammar objects, contextual declarations and descriptor identity.
(import ../src/declarations
        (only-in ../src/language/descriptor language-grammar-with-identity
                 language-grammar-ir language-grammar-machine))
(export (import: ../src/declarations)
        language-grammar-with-identity language-grammar-ir language-grammar-machine)

;;; Public Scheme grammar frontend without pinned-source adapter dependencies.
(import (only-in ../src/language/grammar deflanguage deflanguage-grammar defgrammar-syntax)
        (only-in ../src/grammar/lexical-algebra deftext-profile))
(export deflanguage deflanguage-grammar defgrammar-syntax deftext-profile)

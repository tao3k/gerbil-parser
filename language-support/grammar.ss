;;; Public Scheme grammar frontend without pinned-source adapter dependencies.
(import (only-in ../src/language/grammar deflanguage defgrammar-syntax)
        (only-in ../src/grammar/lexical-algebra deftext-profile))
(export deflanguage defgrammar-syntax deftext-profile)

(import (only-in :clan/poo/object .o)
        (only-in "./parser.ss" tla-plus-core-language))
(export LanguageScope)

(def LanguageScope (.o loader: tla-plus-core-language))

(import (only-in :clan/poo/object .o)
        (only-in "./parser.ss" bash-language))
(export LanguageScope)

(def LanguageScope (.o loader: bash-language))

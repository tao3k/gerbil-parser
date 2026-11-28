(import (only-in :clan/poo/object .o)
        (only-in "./parser.ss" arithmetic-language))
(export LanguageScope)

(def LanguageScope (.o loader: arithmetic-language))

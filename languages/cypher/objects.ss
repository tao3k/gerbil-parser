(import (only-in :clan/poo/object .o)
        (only-in "./parser.ss" opencypher-language))
(export LanguageScope)

(def LanguageScope (.o loader: opencypher-language))

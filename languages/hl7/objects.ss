(import (only-in :clan/poo/object .o)
        (only-in "./parser.ss" hl7-language))
(export LanguageScope)

(def LanguageScope (.o loader: hl7-language))

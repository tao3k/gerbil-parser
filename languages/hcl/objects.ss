(import (only-in :clan/poo/object .o)
        (only-in "./parser.ss" hcl-language))
(export LanguageScope)

(def LanguageScope (.o loader: hcl-language))

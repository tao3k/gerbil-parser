(import (only-in :clan/poo/object .o)
        (only-in "./parser.ss" fhirpath-language))
(export LanguageScope)

(def LanguageScope (.o loader: fhirpath-language))

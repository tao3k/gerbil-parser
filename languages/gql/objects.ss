(import (only-in :clan/poo/object .o)
        (only-in "./parser.ss" gql-language))
(export LanguageScope)

(def LanguageScope (.o loader: gql-language))

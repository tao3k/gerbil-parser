;;; Public source parser services; author declarations use deflanguage.
(import (only-in ./src/language/source
                 deflanguage-parser-receipt
                 source-language? source-language-language source-language-version
                 source-language-contract source-language-digest source-language-result-catalog))
(export deflanguage-parser-receipt
        source-language? source-language-language source-language-version
        source-language-contract source-language-digest source-language-result-catalog)

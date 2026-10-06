;;; -*- Gerbil -*-
;;; Checked public entry for the composed canonical language package.
(import "list-language"
        (only-in "../../../src/language/entry" deflanguage-parser-loader LanguageLoader.))
(export (import: "list-language") list-composed-language parse-list-composed
        list-single-language parse-list-single
        native-list-language parse-native-list native-single-language parse-native-single)
(deflanguage-parser-loader (list-composed-language :: self LanguageLoader.)
  (grammar list-study-language-grammar)
  (parse parse-list-composed))

(deflanguage-parser-loader (list-single-language :: self LanguageLoader.)
  (grammar list-single-language-grammar)
  (parse parse-list-single))

(deflanguage-parser-loader (native-list-language :: self LanguageLoader.)
  (grammar native-list-language-grammar) (parse parse-native-list))
(deflanguage-parser-loader (native-single-language :: self LanguageLoader.)
  (grammar native-single-language-grammar) (parse parse-native-single))

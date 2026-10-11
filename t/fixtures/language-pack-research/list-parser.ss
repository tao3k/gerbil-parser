;;; -*- Gerbil -*-
;;; Checked public entry for the composed canonical language package.
(import "list-language"
        (only-in "../../../language-support/development" deflanguage-development-loader LanguageDevelopmentLoader.))
(export (import: "list-language") list-composed-language parse-list-composed
        list-single-language parse-list-single
        native-list-language parse-native-list native-single-language parse-native-single)
(deflanguage-development-loader (list-composed-language :: self LanguageDevelopmentLoader.)
  (grammar list-study-language-grammar)
  (parse parse-list-composed))

(deflanguage-development-loader (list-single-language :: self LanguageDevelopmentLoader.)
  (grammar list-single-language-grammar)
  (parse parse-list-single))

(deflanguage-development-loader (native-list-language :: self LanguageDevelopmentLoader.)
  (grammar native-list-language-grammar) (parse parse-native-list))
(deflanguage-development-loader (native-single-language :: self LanguageDevelopmentLoader.)
  (grammar native-single-language-grammar) (parse parse-native-single))

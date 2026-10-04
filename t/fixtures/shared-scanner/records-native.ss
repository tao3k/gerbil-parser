;;; @generated native language C entry; compile through the normal package builder.
(import :gerbil-parser/src/ffi/language-v2-native
 (only-in :gerbil-parser/src/ffi/language-handles register-native-language!)
 (only-in :gerbil-parser/t/fixtures/shared-scanner/records records-language-grammar records-contextual-product))
(export create-native-language-handle)
(def (create-native-language-handle) (register-native-language! records-language-grammar records-contextual-product))
(begin-foreign
 (namespace ("gerbil-parser/t/fixtures/shared-scanner/records-native#" native-create-callback))
 (c-declare "#include <gerbil-parser/language-v2.h>\n___U64 records_language_create_impl(void);\nuint64_t records_language_create(void) { return gerbil_parser_language_is_owner_thread() ? records_language_create_impl() : 0; }")
 (c-define (native-create-callback) () unsigned-int64 "records_language_create_impl" "extern"
  (with-exception-catcher (lambda (_) 0) (lambda () (gerbil-parser/t/fixtures/shared-scanner/records-native#create-native-language-handle)))))

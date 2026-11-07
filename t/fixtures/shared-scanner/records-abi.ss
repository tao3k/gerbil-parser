;;; @generated language C entry; compile through the normal package builder.
(import :gerbil-parser/src/ffi/language-abi
 (only-in :gerbil-parser/src/ffi/language-handles register-language!)
 (only-in :gerbil-parser/t/fixtures/shared-scanner/records records-language-grammar records-contextual-product))
(export create-language-handle)
(def (create-language-handle) (register-language! records-language-grammar records-contextual-product))
(begin-foreign
 (namespace ("gerbil-parser/t/fixtures/shared-scanner/records-abi#" create-callback))
 (c-declare "#include <gerbil-parser/language.h>\n___U64 records_language_create_impl(void);\nuint64_t records_language_create(void) { return gerbil_parser_language_is_owner_thread() ? records_language_create_impl() : 0; }")
 (c-define (create-callback) () unsigned-int64 "records_language_create_impl" "extern"
  (with-exception-catcher (lambda (_) 0) (lambda () (gerbil-parser/t/fixtures/shared-scanner/records-abi#create-language-handle)))))

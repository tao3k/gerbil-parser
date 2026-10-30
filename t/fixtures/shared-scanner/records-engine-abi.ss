;;; Test-owned paired factory; production language entry remains generated.
(import :gerbil-parser/t/fixtures/shared-scanner/records-abi
        (only-in :gerbil-parser/src/ffi/language-handles register-language-context!)
        (only-in :gerbil-parser/t/fixtures/shared-scanner/records-engine-control make-records-engine-control))
(export create-engine-control-handle)
(def (create-engine-control-handle) (register-language-context! (make-records-engine-control)))
(begin-foreign
 (namespace ("gerbil-parser/t/fixtures/shared-scanner/records-engine-abi#" control-callback))
 (c-declare "#include <gerbil-parser/language.h>\n___U64 records_engine_control_create_impl(void);\nuint64_t records_engine_control_create(void) { return gerbil_parser_language_is_owner_thread() ? records_engine_control_create_impl() : 0; }")
 (c-define (control-callback) () unsigned-int64 "records_engine_control_create_impl" "extern"
  (with-exception-catcher (lambda (_) 0) (lambda () (gerbil-parser/t/fixtures/shared-scanner/records-engine-abi#create-engine-control-handle)))))

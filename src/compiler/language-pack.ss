;;; -*- Gerbil -*-
;;; Language pack authoring generates a public C entry without core dispatch edits.
(export generate-language-pack)
(def (module-name? value)
  (and (string? value) (> (string-length value) 0)
       (andmap (lambda (ch) (or (char-alphabetic? ch) (char-numeric? ch)
                               (memv ch '(#\- #\_ #\/)))) (string->list value))))
(def (c-name? value)
  (and (string? value) (> (string-length value) 0)
       (let (first (string-ref value 0))
         (or (char=? first #\_) (char<=? #\a first #\z) (char<=? #\A first #\Z)))
       (andmap (lambda (ch) (or (char=? ch #\_) (char<=? #\a ch #\z)
                               (char<=? #\A ch #\Z) (char<=? #\0 ch #\9)))
               (string->list value))))
(def (generate-language-pack source-path header-path module-id grammar-module descriptor-export c-prefix
                                   (contextual-export #f))
  (unless (and (module-name? module-id) (module-name? grammar-module)
               (symbol? descriptor-export) (module-name? (symbol->string descriptor-export))
               (or (not contextual-export) (and (symbol? contextual-export)
                                                (module-name? (symbol->string contextual-export))))
               (c-name? c-prefix))
    (error "invalid language pack identity"))
  (let* ((source
          (string-append
           ";;; @generated language C entry; compile through the normal package builder.\n"
           "(import :gerbil-parser/src/ffi/language-abi\n"
           " (only-in :gerbil-parser/src/ffi/language-handles register-language!)\n"
           " (only-in :" grammar-module " " (symbol->string descriptor-export)
           (if contextual-export (string-append " " (symbol->string contextual-export)) "") "))\n"
           "(export create-language-handle)\n"
           "(def (create-language-handle) (register-language! " (symbol->string descriptor-export)
           (if contextual-export (string-append " " (symbol->string contextual-export)) "") "))\n"
           "(begin-foreign\n (namespace (\"" module-id "#\" create-callback))\n"
           " (c-declare \"#include <gerbil-parser/language.h>\\n___U64 " c-prefix "_language_create_impl(void);\\nuint64_t " c-prefix "_language_create(void) { return gerbil_parser_language_is_owner_thread() ? " c-prefix "_language_create_impl() : 0; }\")\n"
           " (c-define (create-callback) () unsigned-int64 \"" c-prefix "_language_create_impl\" \"extern\"\n"
           "  (with-exception-catcher (lambda (_) 0) (lambda () (" module-id "#create-language-handle)))))\n"))
         (header
          (string-append "/* @generated language entry; runtime must already be initialized on its owner thread. */\n"
                         "#pragma once\n#include <gerbil-parser/language.h>\n#ifdef __cplusplus\nextern \"C\" {\n#endif\n"
                         "/* Creates a distinct handle. Zero signals admission failure. Release with the generic API. */\n"
                         "gerbil_parser_language " c-prefix "_language_create(void);\n#ifdef __cplusplus\n}\n#endif\n")))
    (create-directory* (path-directory source-path))
    (create-directory* (path-directory header-path))
    (call-with-output-file source-path (lambda (port) (display source port)))
    (call-with-output-file header-path (lambda (port) (display header port)))
    source-path))

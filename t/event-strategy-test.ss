(import :gerbil-parser/language-test-support)
;;; -*- Gerbil -*-
;;; Parser-event algorithms execute in Scheme before AOT and stay AST-backed.

(import (only-in :std/io/tempfile make-temporary-file-name)
        (only-in :std/test check check-exception test-case test-suite)
        (only-in "event-strategy-fixture.ss"
                 event-lines-language-grammar parse-event-lines parse_event_lines)
        (only-in :gerbil-parser/src/compiler/event-strategy-aot
                 compile-line-event-parser generate-line-event-module)
        (only-in :gerbil-parser/src/compiler/rust-syntax
                 rust-line-event-function-form-digest)
        (only-in "rust-aot-test-syntax.ss" check-rust-event-strategy))


(deflanguage-parser-tests event-strategy-test "Scheme parser event strategy AOT"
  (engine)
    (property "one Scheme body owns headings and UTF-8 source byte ranges"
      (bindings)
      (equal (parse-event-lines "* α\r\nbody\n")
              '((start Document)
                  (start Heading) (token Line 0 6) (finish)
                  (start Text) (token Line 6 11) (finish)
                  (finish)))
      (equal (parse-event-lines "")
              '((start Document) (finish)))
      (equal (parse-event-lines "plain\r* β")
              '((start Document)
                  (start Text) (token Line 0 6) (finish)
                  (start Heading) (token Line 6 10) (finish)
                  (finish))))
    (property "line extents retain NUL and all UTF-8 widths across CRLF and CR"
      (bindings (prefix (list->string
                    (map integer->char '(0 127 128 2047 2048 55295 57344 65535 65536 1114111)))))
      (equal (parse-event-lines (string-append prefix "\r\nx\rY"))
                '((start Document)
                    (start Text) (token Line 0 28) (finish)
                    (start Text) (token Line 28 30) (finish)
                    (start Text) (token Line 30 31) (finish)
                    (finish))))
    (scenario "AOT is a structured parser function, not source assembly"
      (verify (check-rust-event-strategy parse_event_lines
                                 'parse_event_lines 0))
      (equal (string-prefix?
              "sha256:" (rust-line-event-function-form-digest parse_event_lines))
              #t)
      (equal (equal?
              (rust-line-event-function-form-digest parse_event_lines)
              (rust-line-event-function-form-digest
               (compile-line-event-parser
                'parse_event_lines event-lines-language-grammar
                'Document '(line start end)
                '(event-node Text (event-token Line start end)))))
              #f))
    (scenario "compiled event strategy materializes its Rust module"
      (scope ((path (make-temporary-file-name "gerbil-parser-event-strategy")))
        (protect (body
         (group
           (act (generate-line-event-module path parse_event_lines))
           (equal (file-exists? path)  #t))
         ) (cleanup
          (when (file-exists? path) (act (delete-file path)))))))
    (scenario "AOT admits only declared event kinds and pure expressions"
      (raises
       (compile-line-event-parser
        'invalid event-lines-language-grammar 'Document '(line start end)
        '(event-node Missing (event-token Line start end)))
       true)
      (raises
       (compile-line-event-parser
        'invalid event-lines-language-grammar 'Document '(line start end)
        '(begin (display "side effect") (event-token Line start end)))
       true)))

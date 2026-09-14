;;; -*- Gerbil -*-
;;; Boundary: the versioned package publishes one descriptor-backed parser;
;;; grammar compilation remains owned by grammar.ss at expansion time.
(import :gerbil-parser/src/language/entry ./grammar)
(export (import: ./grammar) tla-plus-v1-language parse-tla-plus-v1)
(deflanguage-parser tla-plus-v1-language
  (grammar tla-plus-v1-language-grammar)
  (parse parse-tla-plus-v1))

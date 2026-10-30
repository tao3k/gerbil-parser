(import (only-in :clan/poo/object .def .get)
        "./types.ss"
        (prefix-in "./lists.ss" lists.)
        (prefix-in "./strings.ss" strings.))
(export Utilities.)
(.def (Utilities. @ (.get UtilitiesScopeContract proto))
  list-operations-for: lists.make-list-operations
  ascii-whitespace?: strings.ascii-whitespace? string-prefix-at?: strings.string-prefix-at?
  string-index-from: strings.string-index-from text-lines: strings.text-lines)

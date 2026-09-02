;;; Thin execution boundary over an AOT-generated lexer machine.

(import ../compiler/machine)
(export lex-source)

(def (lex-source machine source)
  ((parser-machine-lex machine) source))

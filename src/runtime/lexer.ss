;;; Thin execution boundary over an AOT-generated lexer machine.

(import (only-in ../compiler/machine parser-machine-lex call-with-parser-machine-source))
(export lex-source
        lex-source-from
        scan-source-token)

;; : (-> ParserMachine String (List Token))
(def (lex-source machine source)
  (call-with-parser-machine-source machine source
   (lambda () ((parser-machine-lex machine) source))))

;; : (-> ParserMachine String Nat Nat (List Token))
(def (lex-source-from machine source character-offset byte-offset)
  (call-with-parser-machine-source machine source
   (lambda () ((parser-machine-lex machine) source character-offset byte-offset))))

;; : (-> ParserMachine String Nat Nat LexicalMode (Values Token Nat))
(def (scan-source-token machine source character-offset byte-offset mode)
  ((parser-machine-lex machine)
   source character-offset byte-offset mode))

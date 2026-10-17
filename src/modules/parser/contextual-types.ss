;;; -*- Gerbil -*-
;;; Contract for compile-time contextual roles. Generated machines contain
;;; only the role's normalized rows, never this object or its prototype.

(import (only-in :clan/poo/object .o)
        (only-in :clan/poo/mop define-type)
        (only-in :core/types PooFlowNativeObjectContract.)
        (only-in ./types ParserSymbol ParserList))
(export +contextual-role-kind+ ContextualRoleContract)

(def +contextual-role-kind+ 'gerbil-parser-contextual-role)

(def (contextual-empty-prototype) (.o))

(define-type (ContextualRoleContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/contextual-role
  proto: (contextual-empty-prototype)
  responsibilities:
  (.o kind: ParserSymbol
      name: ParserSymbol
      methods: ParserList))

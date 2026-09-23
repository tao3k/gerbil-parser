;;; -*- Gerbil -*-
;;; POO contracts for bounded contextual line parsers.

(import (only-in :clan/poo/object .o .ref)
        (only-in :clan/poo/mop define-type)
        (only-in :poo-flow/src/module-system/types
                 PooFlowContract. PooFlowNativeObjectContract.
                 poo-flow-contract-admit
                 poo-flow-validation-evidence-accepted?
                 poo-flow-classification-evidence)
        (only-in ./types ParserSymbol ParserList))
(export +line-structure-schema+
        +line-structure-kind+
        +heading-line-kind+
        +block-line-kind+
        +text-line-kind+
        LineMarker LineDelimiter LineBoolean
        BlockRecovery
        LineStructureContract
        HeadingLineContract
        BlockLineContract
        TextLineContract)

(def +line-structure-schema+ "gerbil-parser.line-structure.v1")
(def +line-structure-kind+ 'gerbil-parser-line-structure)
(def +heading-line-kind+ 'gerbil-parser-heading-line)
(def +block-line-kind+ 'gerbil-parser-block-line)
(def +text-line-kind+ 'gerbil-parser-text-line)

(def (line-classify identity predicate candidate context)
  (let (accepted? (predicate candidate))
    (poo-flow-classification-evidence
     identity candidate accepted?
     (if accepted? '() (list (list 'expected identity)))
     context)))

(def (ascii-text? value)
  (and (string? value)
       (> (string-length value) 0)
       (let loop ((index 0))
         (or (= index (string-length value))
             (and (< (char->integer (string-ref value index)) 128)
                  (loop (+ index 1)))))))

(define-type (LineMarker @ PooFlowContract.)
  identity: 'gerbil-parser/line-marker
  .classify: (lambda (candidate context)
               (line-classify 'gerbil-parser/line-marker
                              (lambda (value)
                                (and (ascii-text? value)
                                     (= (string-length value) 1)))
                              candidate context)))

(define-type (LineDelimiter @ PooFlowContract.)
  identity: 'gerbil-parser/line-delimiter
  .classify: (lambda (candidate context)
               (line-classify 'gerbil-parser/line-delimiter
                              ascii-text? candidate context)))

(define-type (LineBoolean @ PooFlowContract.)
  identity: 'gerbil-parser/line-boolean
  .classify: (lambda (candidate context)
               (line-classify 'gerbil-parser/line-boolean
                              boolean? candidate context)))

(define-type (BlockRecovery @ PooFlowContract.)
  identity: 'gerbil-parser/block-recovery
  .classify: (lambda (candidate context)
               (line-classify
                'gerbil-parser/block-recovery
                (lambda (value)
                  (memq value '(close-at-eof recover-as-text)))
                candidate context)))

(define-type (LineStructureSchema @ PooFlowContract.)
  identity: 'gerbil-parser/line-structure-schema
  .classify: (lambda (candidate context)
               (line-classify 'gerbil-parser/line-structure-schema
                              (lambda (value)
                                (equal? value +line-structure-schema+))
                              candidate context)))

(def (line-kind-contract identity expected)
  (lambda (candidate context)
    (line-classify identity
                   (lambda (value) (eq? value expected))
                   candidate context)))

(define-type (HeadingLineKind @ PooFlowContract.)
  identity: 'gerbil-parser/heading-line-kind
  .classify: (line-kind-contract 'gerbil-parser/heading-line-kind
                                 +heading-line-kind+))

(define-type (BlockLineKind @ PooFlowContract.)
  identity: 'gerbil-parser/block-line-kind
  .classify: (line-kind-contract 'gerbil-parser/block-line-kind
                                 +block-line-kind+))

(define-type (TextLineKind @ PooFlowContract.)
  identity: 'gerbil-parser/text-line-kind
  .classify: (line-kind-contract 'gerbil-parser/text-line-kind
                                 +text-line-kind+))

(define-type (LineStructureKind @ PooFlowContract.)
  identity: 'gerbil-parser/line-structure-kind
  .classify: (line-kind-contract 'gerbil-parser/line-structure-kind
                                 +line-structure-kind+))

(def (empty-prototype) (.o))

(define-type (HeadingLineContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/heading-line
  proto: (empty-prototype)
  responsibilities:
  (.o kind: HeadingLineKind
      marker: LineMarker
      separator: LineMarker
      section-node: ParserSymbol
      heading-node: ParserSymbol
      heading-token: ParserSymbol))

(define-type (BlockLineContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/block-line
  proto: (empty-prototype)
  responsibilities:
  (.o kind: BlockLineKind
      opening: LineDelimiter
      closing: LineDelimiter
      case-insensitive: LineBoolean
      indent: LineBoolean
      block-node: ParserSymbol
      begin-token: ParserSymbol
      body-token: ParserSymbol
      end-token: ParserSymbol
      unclosed: BlockRecovery
      heading-bound: LineBoolean))

(define-type (TextLineContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/text-line
  proto: (empty-prototype)
  responsibilities:
  (.o kind: TextLineKind
      text-node: ParserSymbol
      text-token: ParserSymbol))

(def (line-structure-obligations candidate _context)
  (if (andmap (lambda (block)
                (poo-flow-validation-evidence-accepted?
                 (poo-flow-contract-admit BlockLineContract block #f)))
              (.ref candidate 'blocks))
    '()
    '(invalid-block-rule)))

(define-type (LineStructureContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/line-structure
  proto: (empty-prototype)
  responsibilities:
  (.o kind: LineStructureKind
      schema: LineStructureSchema
      heading: HeadingLineContract
      blocks: ParserList
      text: TextLineContract)
  .obligations: line-structure-obligations)

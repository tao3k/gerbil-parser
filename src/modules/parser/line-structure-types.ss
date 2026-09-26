;;; -*- Gerbil -*-
;;; POO contracts for bounded contextual line parsers.

(import (only-in :clan/poo/object .o .ref)
        (only-in :clan/poo/mop define-type)
        (only-in :poo-flow-foundation/module-system/types
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
        +key-value-line-kind+
        +block-header-kind+
        +inline-link-kind+
        +heading-fields-kind+
        +table-line-kind+
        +list-line-kind+
        +key-line-kind+
        LineMarker LineDelimiter LineBoolean
        BlockRecovery
        BlockContents
        BlockOpeningMode
        KeyValueLineContract BlockBodyContract BlockHeaderContract
        InlineLinkContract HeadingFieldsContract
        TableLineContract OptionalTableLineContract
        ListLineContract OptionalListLineContract
        ListTabWidth
        KeyLineContract KeyLineKeys KeyLineSeparator AsciiPrefix
        OptionalParagraphNodeContract
        LineStructureContract
        HeadingLineContract
        BlockLineContract
        TextLineContract)

(def +line-structure-schema+ "gerbil-parser.line-structure.v1")
(def +line-structure-kind+ 'gerbil-parser-line-structure)
(def +heading-line-kind+ 'gerbil-parser-heading-line)
(def +block-line-kind+ 'gerbil-parser-block-line)
(def +text-line-kind+ 'gerbil-parser-text-line)
(def +key-value-line-kind+ 'gerbil-parser-key-value-line)
(def +block-header-kind+ 'gerbil-parser-block-header)
(def +inline-link-kind+ 'gerbil-parser-inline-link)
(def +heading-fields-kind+ 'gerbil-parser-heading-fields)
(def +table-line-kind+ 'gerbil-parser-table-line)
(def +list-line-kind+ 'gerbil-parser-list-line)
(def +key-line-kind+ 'gerbil-parser-key-line)

(def (empty-prototype) (.o))

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

(define-type (AsciiPrefix @ PooFlowContract.)
  identity: 'gerbil-parser/ascii-prefix
  .classify: (lambda (candidate context)
               (line-classify 'gerbil-parser/ascii-prefix
                              (lambda (value)
                                (and (string? value)
                                     (or (string=? value "") (ascii-text? value))))
                              candidate context)))

(def (key-name? value)
  (and (ascii-text? value)
       (let loop ((index 0))
         (or (= index (string-length value))
             (let (char (string-ref value index))
               (and (or (char-alphabetic? char) (char-numeric? char)
                        (eq? char #\_) (eq? char #\-))
                    (loop (+ index 1))))))))

(define-type (KeyLineKeys @ PooFlowContract.)
  identity: 'gerbil-parser/key-line-keys
  .classify: (lambda (candidate context)
               (line-classify 'gerbil-parser/key-line-keys
                              (lambda (value)
                                (and (list? value) (<= (length value) 16)
                                     (andmap key-name? value)))
                              candidate context)))

(define-type (KeyLineSeparator @ PooFlowContract.)
  identity: 'gerbil-parser/key-line-separator
  .classify: (lambda (candidate context)
               (line-classify
                'gerbil-parser/key-line-separator
                (lambda (value)
                  (and (ascii-text? value)
                       (= (string-length value) 1)
                       (let (char (string-ref value 0))
                         (and (not (char-alphabetic? char))
                              (not (char-numeric? char))
                              (not (char-whitespace? char))))))
                candidate context)))

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

(define-type (BlockContents @ PooFlowContract.)
  identity: 'gerbil-parser/block-contents
  .classify: (lambda (candidate context)
               (line-classify
                'gerbil-parser/block-contents
                (lambda (value) (memq value '(opaque elements)))
                candidate context)))

(define-type (BlockOpeningMode @ PooFlowContract.)
  identity: 'gerbil-parser/block-opening-mode
  .classify: (lambda (candidate context)
               (line-classify
                'gerbil-parser/block-opening-mode
                (lambda (value)
                  (memq value '(literal named-delimited required-named-argument)))
                candidate context)))

(define-type (KeyValueLineKind @ PooFlowContract.)
  identity: 'gerbil-parser/key-value-line-kind
  .classify: (lambda (candidate context)
               (line-classify 'gerbil-parser/key-value-line-kind
                              (lambda (value)
                                (eq? value +key-value-line-kind+))
                              candidate context)))

(define-type (KeyValueLineContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/key-value-line
  proto: (empty-prototype)
  responsibilities:
  (.o kind: KeyValueLineKind
      marker: LineMarker
      node: ParserSymbol
      key-token: ParserSymbol
      value-token: ParserSymbol
      trivia-token: ParserSymbol))

(define-type (BlockBodyContract @ PooFlowContract.)
  identity: 'gerbil-parser/block-body
  .classify: (lambda (candidate context)
               (line-classify
                'gerbil-parser/block-body
                (lambda (value)
                  (or (eq? value #f)
                      (poo-flow-validation-evidence-accepted?
                       (poo-flow-contract-admit KeyValueLineContract value #f))))
                candidate context)))

(define-type (BlockHeaderKind @ PooFlowContract.)
  identity: 'gerbil-parser/block-header-kind
  .classify: (lambda (candidate context)
               (line-classify 'gerbil-parser/block-header-kind
                              (lambda (value) (eq? value +block-header-kind+))
                              candidate context)))

(define-type (BlockHeaderContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/block-header
  proto: (empty-prototype)
  responsibilities:
  (.o kind: BlockHeaderKind
      argument-token: ParserSymbol
      trivia-token: ParserSymbol))

(define-type (InlineLinkKind @ PooFlowContract.)
  identity: 'gerbil-parser/inline-link-kind
  .classify: (lambda (candidate context)
               (line-classify 'gerbil-parser/inline-link-kind
                              (lambda (value) (eq? value +inline-link-kind+))
                              candidate context)))

(define-type (InlineLinkContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/inline-link
  proto: (empty-prototype)
  responsibilities:
  (.o kind: InlineLinkKind
      opening: LineDelimiter
      separator: LineDelimiter
      closing: LineDelimiter
      node: ParserSymbol
      target-token: ParserSymbol
      description-token: ParserSymbol
      trivia-token: ParserSymbol))

(define-type (HeadingFieldsKind @ PooFlowContract.)
  identity: 'gerbil-parser/heading-fields-kind
  .classify: (lambda (candidate context)
               (line-classify 'gerbil-parser/heading-fields-kind
                              (lambda (value) (eq? value +heading-fields-kind+))
                              candidate context)))

(define-type (HeadingFieldsContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/heading-fields
  proto: (empty-prototype)
  responsibilities:
  (.o kind: HeadingFieldsKind
      title-token: ParserSymbol
      trivia-token: ParserSymbol))

(define-type (OptionalHeadingFieldsContract @ PooFlowContract.)
  identity: 'gerbil-parser/optional-heading-fields
  .classify: (lambda (candidate context)
               (line-classify
                'gerbil-parser/optional-heading-fields
                (lambda (value)
                  (or (eq? value #f)
                      (poo-flow-validation-evidence-accepted?
                       (poo-flow-contract-admit HeadingFieldsContract value #f))))
                candidate context)))

(define-type (OptionalInlineLinkContract @ PooFlowContract.)
  identity: 'gerbil-parser/optional-inline-link
  .classify: (lambda (candidate context)
               (line-classify
                'gerbil-parser/optional-inline-link
                (lambda (value)
                  (or (eq? value #f)
                      (poo-flow-validation-evidence-accepted?
                       (poo-flow-contract-admit InlineLinkContract value #f))))
                candidate context)))

(define-type (OptionalBlockHeaderContract @ PooFlowContract.)
  identity: 'gerbil-parser/optional-block-header
  .classify: (lambda (candidate context)
               (line-classify
                'gerbil-parser/optional-block-header
                (lambda (value)
                  (or (eq? value #f)
                      (poo-flow-validation-evidence-accepted?
                       (poo-flow-contract-admit BlockHeaderContract value #f))))
                candidate context)))

(define-type (OptionalParagraphNodeContract @ PooFlowContract.)
  identity: 'gerbil-parser/optional-paragraph-node
  .classify: (lambda (candidate context)
               (line-classify
                'gerbil-parser/optional-paragraph-node
                (lambda (value)
                  (or (eq? value #f)
                      (poo-flow-validation-evidence-accepted?
                       (poo-flow-contract-admit ParserSymbol value #f))))
                candidate context)))

(define-type (TableLineKind @ PooFlowContract.)
  identity: 'gerbil-parser/table-line-kind
  .classify: (line-kind-contract 'gerbil-parser/table-line-kind
                                 +table-line-kind+))

(define-type (TableLineContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/table-line
  proto: (empty-prototype)
  responsibilities:
  (.o kind: TableLineKind
      delimiter: LineMarker
      table-node: ParserSymbol
      row-node: ParserSymbol
      rule-row-node: ParserSymbol
      cell-node: ParserSymbol
      separator-token: ParserSymbol
      cell-token: ParserSymbol
      trivia-token: ParserSymbol
      rule-token: ParserSymbol))

(define-type (OptionalTableLineContract @ PooFlowContract.)
  identity: 'gerbil-parser/optional-table-line
  .classify: (lambda (candidate context)
               (line-classify
                'gerbil-parser/optional-table-line
                (lambda (value)
                  (or (eq? value #f)
                      (poo-flow-validation-evidence-accepted?
                       (poo-flow-contract-admit TableLineContract value #f))))
                candidate context)))

(define-type (ListLineKind @ PooFlowContract.)
  identity: 'gerbil-parser/list-line-kind
  .classify: (line-kind-contract 'gerbil-parser/list-line-kind
                                 +list-line-kind+))

(define-type (ListTabWidth @ PooFlowContract.)
  identity: 'gerbil-parser/list-tab-width
  .classify: (lambda (candidate context)
               (line-classify 'gerbil-parser/list-tab-width
                              (lambda (value)
                                (and (integer? value) (<= 1 value 16)))
                              candidate context)))

(define-type (ListLineContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/list-line
  proto: (empty-prototype)
  responsibilities:
  (.o kind: ListLineKind
      unordered-markers: LineDelimiter
      ordered: LineBoolean
      tab-width: ListTabWidth
      list-node: ParserSymbol
      item-node: ParserSymbol
      bullet-token: ParserSymbol
      trivia-token: ParserSymbol))

(define-type (OptionalListLineContract @ PooFlowContract.)
  identity: 'gerbil-parser/optional-list-line
  .classify: (lambda (candidate context)
               (line-classify
                'gerbil-parser/optional-list-line
                (lambda (value)
                  (or (eq? value #f)
                      (poo-flow-validation-evidence-accepted?
                       (poo-flow-contract-admit ListLineContract value #f))))
                candidate context)))

(define-type (KeyLineKind @ PooFlowContract.)
  identity: 'gerbil-parser/key-line-kind
  .classify: (line-kind-contract 'gerbil-parser/key-line-kind
                                 +key-line-kind+))

(define-type (KeyLineContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/key-line
  proto: (empty-prototype)
  responsibilities:
  (.o kind: KeyLineKind
      prefix: AsciiPrefix
      keys: KeyLineKeys
      separator: KeyLineSeparator
      case-insensitive: LineBoolean
      indent: LineBoolean
      after-heading: LineBoolean
      repeated: LineBoolean
      node: ParserSymbol
      key-token: ParserSymbol
      value-token: ParserSymbol
      trivia-token: ParserSymbol)
  .obligations: (lambda (candidate _context)
                  (if (or (and (null? (.ref candidate 'keys))
                               (string=? (.ref candidate 'prefix) ""))
                          (and (.ref candidate 'repeated)
                               (null? (.ref candidate 'keys))))
                    '(unbounded-key-line)
                    '())))

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

(define-type (HeadingLineContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/heading-line
  proto: (empty-prototype)
  responsibilities:
  (.o kind: HeadingLineKind
      marker: LineMarker
      separator: LineMarker
      section-node: ParserSymbol
      heading-node: ParserSymbol
      heading-token: ParserSymbol
      fields: OptionalHeadingFieldsContract))

(define-type (BlockLineContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/block-line
  proto: (empty-prototype)
  responsibilities:
  (.o kind: BlockLineKind
      opening: LineDelimiter
      opening-mode: BlockOpeningMode
      closing: LineDelimiter
      case-insensitive: LineBoolean
      indent: LineBoolean
      block-node: ParserSymbol
      begin-token: ParserSymbol
      body-token: ParserSymbol
      end-token: ParserSymbol
      unclosed: BlockRecovery
      heading-bound: LineBoolean
      contents: BlockContents
      body-line: BlockBodyContract
      header: OptionalBlockHeaderContract)
  .obligations: (lambda (candidate _context)
                  (append
                   (if (and (eq? (.ref candidate 'contents) 'elements)
                            (.ref candidate 'body-line))
                     '(recursive-block-cannot-have-key-value-body)
                     '())
                   (if (and (eq? (.ref candidate 'opening-mode)
                                 'named-delimited)
                            (or (not (= (string-length
                                         (.ref candidate 'opening)) 1))
                                (not (.ref candidate 'header))))
                     '(named-block-requires-delimiter-and-header)
                     '())
                   (if (and (eq? (.ref candidate 'opening-mode)
                                 'required-named-argument)
                            (not (.ref candidate 'header)))
                     '(named-argument-requires-header)
                     '()))))

(define-type (TextLineContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/text-line
  proto: (empty-prototype)
  responsibilities:
  (.o kind: TextLineKind
      text-node: ParserSymbol
      text-token: ParserSymbol
      paragraph-node: OptionalParagraphNodeContract
      inline-link: OptionalInlineLinkContract))

(def (line-structure-obligations candidate _context)
  (append
   (if (andmap (lambda (block)
                 (poo-flow-validation-evidence-accepted?
                  (poo-flow-contract-admit BlockLineContract block #f)))
               (.ref candidate 'blocks))
     '()
     '(invalid-block-rule))
   (if (andmap (lambda (rule)
                 (poo-flow-validation-evidence-accepted?
                  (poo-flow-contract-admit KeyLineContract rule #f)))
               (.ref candidate 'key-lines))
     '()
     '(invalid-key-line-rule))))

(define-type (LineStructureContract @ PooFlowNativeObjectContract.)
  identity: 'gerbil-parser/line-structure
  proto: (empty-prototype)
  responsibilities:
  (.o kind: LineStructureKind
      schema: LineStructureSchema
      heading: HeadingLineContract
      blocks: ParserList
      text: TextLineContract
      table: OptionalTableLineContract
      list: OptionalListLineContract
      key-lines: ParserList)
  .obligations: line-structure-obligations)

;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later
;;; POO contracts for language-neutral UTF-8 byte-span structural edits.

(import (only-in :clan/poo/object .o .ref .cc)
        (only-in :clan/poo/type define-type element? Type. Record String Bool
                 Exactly OrFalse Any)
        (only-in :clan/poo/number UInt Integer)
        (only-in :tao3k/yuanxu/types/contracts YuanxuContract.))
(export +span-edit-kind+ +span-edit-result-kind+
        SpanEditFields SpanEdit SpanEditContract
        SpanEditResult SpanEditResultContract
        span-edit? span-edit-result?)

(def +span-edit-kind+ 'gerbil-parser-span-edit)
(def +span-edit-result-kind+ 'gerbil-parser-span-edit-result)

(def SpanEditFields
  (Record kind: [(Exactly +span-edit-kind+)]
          start: [UInt]
          end: [UInt]
          replacement: [String]
          label: [(OrFalse String)]))

(define-type (SpanEdit @ SpanEditFields)
  .element?: (lambda (v)
               (and (element? SpanEditFields v)
                    (<= (.ref v 'start) (.ref v 'end)))))

(def SpanEditContract
  (.cc YuanxuContract.
       identity: 'gerbil-parser/span-edit
       proto: (.o)
       value-contract: SpanEdit))

(define-type SpanEditResult
  (Record kind: [(Exactly +span-edit-result-kind+)]
          source: [String]
          applied-count: [UInt]
          byte-delta: [Integer]
          verified?: [Bool]
          parsed: [Any]))

(def SpanEditResultContract
  (.cc YuanxuContract.
       identity: 'gerbil-parser/span-edit-result
       proto: (.o)
       value-contract: SpanEditResult))

(def (span-edit? value)
  (element? SpanEdit value))

(def (span-edit-result? value)
  (element? SpanEditResult value))

;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later
;;; Public POO span-edit surface for language-owned parser-verified rewriting.

(import (only-in ../src/graph/span-edit-types
                 +span-edit-kind+ +span-edit-result-kind+
                 SpanEdit SpanEditContract
                 SpanEditResult SpanEditResultContract
                 span-edit? span-edit-result?)
        (only-in ../src/graph/span-edit-funs
                 make-span-edit
                 span-edit-insert
                 span-edit-delete
                 span-edit-replace
                 span-edit-start
                 span-edit-end
                 span-edit-replacement
                 span-edit-label
                 utf8-byte-slice
                 span-edits-normalize
                 apply-span-edits
                 apply-span-edits/verified))
(export +span-edit-kind+ +span-edit-result-kind+
        SpanEdit SpanEditContract
        SpanEditResult SpanEditResultContract
        span-edit? span-edit-result?
        make-span-edit
        span-edit-insert
        span-edit-delete
        span-edit-replace
        span-edit-start
        span-edit-end
        span-edit-replacement
        span-edit-label
        utf8-byte-slice
        span-edits-normalize
        apply-span-edits
        apply-span-edits/verified)

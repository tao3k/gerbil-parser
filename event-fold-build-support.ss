;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later
;;; Native Fold source emission and compilation; no strategy catalogue loading.
(import (only-in ./src/compiler/event-fold-scheme-modules event-fold-scheme-module-sources)
        (only-in ./src/compiler/event-fold-scheme-build compile-event-fold-scheme-units))
(export event-fold-scheme-module-sources compile-event-fold-scheme-units)

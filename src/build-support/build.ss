;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import (only-in :clan/poo/object .def .o)
        (only-in "./compiler-profile.ss" initialize-compiler-build!))
(export Build)

(.def Build
  defbuild-script: (.o spec: '())
  .prepare: (lambda (_) (initialize-compiler-build!)))

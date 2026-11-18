#!/usr/bin/env gxi
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later
(import (only-in :clan/poo/object .get)
        (only-in :std/source this-source-file)
        (only-in :std/build-script defbuild-script)
        (only-in :tao3k/yuanxu/module-system/building/build-script defmodule-build-script)
        (only-in "./package-spec.ss" gerbil-parser.package.exports))

(defmodule-build-script
  (defbuild-script (.get gerbil-parser.package.exports std-make-spec))
  (.get gerbil-parser.package.exports build-actions))

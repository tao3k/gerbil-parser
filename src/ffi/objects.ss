;;; Selection exposes ABI metadata. C initialization belongs to compiled consumers.
(import (only-in :clan/poo/object .def .get)
        "./types.ss"
        (prefix-in (only-in "../schema.ss" +language-abi-version+ +artifact-abi-version+ +event-abi-version+ +rust-aot-abi-version+) schema.))
(export ForeignInterface.)
(.def (ForeignInterface. @ (.get ForeignInterfaceScopeContract proto))
  language-abi-version: schema.+language-abi-version+
  artifact-abi-version: schema.+artifact-abi-version+
  event-abi-version: schema.+event-abi-version+
  rust-aot-abi-version: schema.+rust-aot-abi-version+)

(import (only-in :clan/poo/object .def .get)
        "./types.ss"
        (prefix-in (only-in "./state.ss" lr-runtime?) leaf.))
(export LrParser.)

(.def (LrParser. @ (.get LrParserScopeContract proto))
  lr-runtime?: leaf.lr-runtime?)

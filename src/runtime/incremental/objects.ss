(import (only-in :clan/poo/object .def .get)
        "./types.ss"
        (prefix-in (only-in "./state.ss" incremental-session?) leaf.))
(export Incremental.)

(.def (Incremental. @ (.get IncrementalScopeContract proto))
  incremental-session?: leaf.incremental-session?)

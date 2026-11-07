;;; Nominal machine execution stays in its existing compiled public API.
(import (only-in :clan/poo/object .def .get)
        "./types.ss"
        (prefix-in (only-in "./artifact.ss" parse-artifact-valid?
                           parse-artifact-success? parse-artifact-roundtrip) artifact.))
(export Runtime.)
(.def (Runtime. @ (.get RuntimeScopeContract proto))
  parse-artifact-valid?: artifact.parse-artifact-valid?
  parse-artifact-success?: artifact.parse-artifact-success?
  parse-artifact-roundtrip: artifact.parse-artifact-roundtrip)

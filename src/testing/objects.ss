;;; Publish existing services without executing them during namespace discovery.
(import (only-in :clan/poo/object .def .get)
        "./types.ss"
        (prefix-in (only-in "./contracts.ss" parser-test-form-admitted?) api0.))
(export Testing.)
(.def (Testing. @ (.get TestingScopeContract proto))
  parser-test-form-admitted?: api0.parser-test-form-admitted?)

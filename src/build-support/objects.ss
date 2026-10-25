;;; Publish existing services without executing them during namespace discovery.
(import (only-in :clan/poo/object .def .get)
        "./types.ss"
        (prefix-in (only-in "./compiler-profile.ss" initialize-compiler-build!) host.)
        (prefix-in (only-in "./compiler-profile.ss" compiler-gsc-options) api0.)
        (prefix-in (only-in "./compiler-profile.ss" compiler-build-spec) api1.)
        (prefix-in (only-in "./native-links.ss" native-module-link-options) api2.))
(export BuildSupport.)
(.def (BuildSupport. @ (.get BuildSupportScopeContract proto))
  initialize-build!: host.initialize-compiler-build!
  compiler-gsc-options: api0.compiler-gsc-options
  compiler-build-spec: api1.compiler-build-spec
  native-module-link-options: api2.native-module-link-options)

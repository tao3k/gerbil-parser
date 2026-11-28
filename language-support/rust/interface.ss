(import (only-in :clan/poo/object .def .o)
        (only-in "./types.ss" RustScopeContract)
        (only-in "./objects.ss" Rust.))
(.def Rust
  identity: 'rust
  core: (.o scope-contract: RustScopeContract exports: Rust.)
  features: (.o))

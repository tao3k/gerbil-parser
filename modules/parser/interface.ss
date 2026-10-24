;;; Library selection publishes source membership; it does not execute parsers.
(.def Parser
  identity: 'parser
  core: (.o requires: (.o) scope-contract: ParserScopeContract
            exports: (.o catalog: LibraryCatalog))
  features: (.o))

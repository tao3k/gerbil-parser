;;; Select the existing pure service prototype; no process state is created.
(import (only-in "./objects.ss" ParserModules.))
(export Config)
(def Config ParserModules.)

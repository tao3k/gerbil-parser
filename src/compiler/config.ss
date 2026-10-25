;;; Select the existing pure service prototype; no process state is created.
(import (only-in "./objects.ss" Compiler.))
(export Config)
(def Config Compiler.)

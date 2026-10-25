;;; Select the existing pure service prototype; no process state is created.
(import (only-in "./objects.ss" Grammar.))
(export Config)
(def Config Grammar.)

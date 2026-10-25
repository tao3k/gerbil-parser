;;; Select the existing pure service prototype; no process state is created.
(import (only-in "./objects.ss" Utilities.))
(export Config)
(def Config Utilities.)

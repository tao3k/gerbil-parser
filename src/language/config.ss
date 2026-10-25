;;; Select the existing pure service prototype; no process state is created.
(import (only-in "./objects.ss" Language.))
(export Config)
(def Config Language.)

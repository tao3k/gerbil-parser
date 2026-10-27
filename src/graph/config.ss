;;; Select the existing pure service prototype; no process state is created.
(import (only-in "./objects.ss" Graph.))
(export Config)
(def Config Graph.)

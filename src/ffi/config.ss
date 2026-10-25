;;; Select the existing pure service prototype; no process state is created.
(import (only-in "./objects.ss" ForeignInterface.))
(export Config)
(def Config ForeignInterface.)

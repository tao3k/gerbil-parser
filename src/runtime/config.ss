;;; Select the existing pure service prototype; no process state is created.
(import (only-in "./objects.ss" Runtime.))
(export Config)
(def Config Runtime.)

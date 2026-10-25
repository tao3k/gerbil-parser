;;; Select the existing pure service prototype; no process state is created.
(import (only-in "./objects.ss" Testing.))
(export Config)
(def Config Testing.)

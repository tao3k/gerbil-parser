;;; Select the existing pure service prototype; no process state is created.
(import (only-in "./objects.ss" BuildSupport.))
(export Config)
(def Config BuildSupport.)

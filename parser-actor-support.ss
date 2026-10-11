;;; -*- Gerbil -*-
;;; Optional public facade for application-owned local parser actors.
(import (only-in ./src/runtime/actor
                 spawn-parser-actor parser-actor-parse parser-actor-stop!))
(export spawn-parser-actor parser-actor-parse parser-actor-stop!)

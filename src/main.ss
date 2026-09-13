;;; -*- Gerbil -*-
;;; Native AOT command entrypoint for the thin gparse command surface.
;;; Parsing and compilation remain library-owned; this module only maps commands to owners.

(import (only-in :std/getopt argument call-with-getopt command)
        (only-in :std/sugar let-hash)
        (only-in ./cli
                 gparse-build gparse-check gparse-inspect gparse-test))
(export main)

;; : (-> String ... Void)
(def (main . args)
  (def build-command
    (command 'build help: "emit the canonical reference Parser IR"))
  (def inspect-command
    (command 'inspect help: "inspect the canonical reference Parser IR"))
  (def check-command
    (command 'check help: "parse source and emit a typed ParseArtifact"
      (argument 'source help: "source text to parse")))
  (def test-command
    (command 'test help: "run the installed CLI smoke parse"))
  (call-with-getopt gparse-main args
    program: "gparse"
    help: "Pure Gerbil programmable parser compiler and runtime"
    build-command
    inspect-command
    check-command
    test-command))

;; : (-> Symbol HashTable Void)
(def (gparse-main command-name options)
  (let-hash options
    (exit
     (case command-name
       ((build) (gparse-build))
       ((inspect) (gparse-inspect))
       ((check) (gparse-check .source))
       ((test) (gparse-test))
       (else 64)))))

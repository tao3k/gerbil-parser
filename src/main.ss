;;; -*- Gerbil -*-
;;; Native AOT command entrypoint.

(import :std/getopt
        :std/sugar
        ./cli)
(export main)

(def (main . args)
  (def build-command
    (command 'build help: "emit the canonical reference Parser IR"))
  (def inspect-command
    (command 'inspect help: "inspect the canonical reference Parser IR"))
  (def check-command
    (command 'check help: "parse source and emit a typed receipt"
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

(def (gparse-main command-name options)
  (let-hash options
    (exit
     (case command-name
       ((build) (gparse-build))
       ((inspect) (gparse-inspect))
       ((check) (gparse-check .source))
       ((test) (gparse-test))
       (else 64)))))

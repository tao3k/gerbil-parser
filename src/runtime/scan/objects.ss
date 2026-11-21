(import (only-in :clan/poo/object .def .get)
        "./types.ss"
        (prefix-in (only-in "./basic.ss" scan-line) leaf.))
(export Scanner.)

(.def (Scanner. @ (.get ScannerScopeContract proto))
  scan-line: leaf.scan-line)

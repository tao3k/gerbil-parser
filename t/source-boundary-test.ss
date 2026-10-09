;;; Non-Org native recovery controls through admitted POO declarations.
(import :std/test
        (only-in :gerbil-parser/src/modules/parser/interface
                 make-heading-line make-block-line make-source-block-boundary
                 make-source-named-boundary make-source-boundary-parent
                 source-boundary-query? source-boundary-parent?
                 source-container-boundary-condition)
        (only-in :gerbil-parser/src/compiler/event-fold-runtime run-event-fold))
(export source-boundary-test)
(def heading (make-heading-line "*" " " 'Section 'Heading 'Marker))
(def fixed
  (make-source-block-boundary
    (make-block-line "OPEN" "CLOSE" #t #t 'Block 'Begin 'Body 'End
                     'recover-as-text #t #f #f 'elements)
    heading))
(def named (make-source-named-boundary "OPEN:" "CLOSE:" "" heading))
(def parents (list (make-source-boundary-parent 1 "PARENT-END")))
(def named-parents
  (list (make-source-boundary-parent 2 "PARENT:" 'parent-from 'parent-until)))
(def (accepted? query source (parent 0) (bindings parents))
  (let* ((condition (source-container-boundary-condition query bindings 'frames))
         (events
          (run-event-fold source 'Document
            '((done #f) (frames (uint-stack)) (parent-from 0) (parent-until 4))
            `((if (not (state done))
                  ((push-frame frames (uint ,parent))
                   (if ,condition ((token Accepted start end))
                                  ((token Rejected start end)))
                   (set-bool done (bool #t)))
                  ((token Raw start end))))
            '())))
    (and (ormap (lambda (event)
                  (and (eq? (car event) 'token) (eq? (cadr event) 'Accepted)))
                events) #t)))
(def source-boundary-test
  (test-suite "native parent-aware boundary composition"
    (test-case "declarations are admitted POO objects"
      (check (source-boundary-query? fixed) => #t)
      (check (source-boundary-query? named) => #t)
      (check (source-boundary-parent? (car parents)) => #t))
    (test-case "fixed root recovery observes headings, CRLF and EOF"
      (check (accepted? fixed "OPEN\r\nλ\r\nCLOSE") => #t)
      (check (accepted? fixed "OPEN\n* heading\nCLOSE\n") => #f)
      (check (accepted? fixed "OPEN\nbody") => #f))
    (test-case "literal parent stop precedes child close"
      (check (accepted? fixed "OPEN\nPARENT-END\nCLOSE\n" 1) => #f)
      (check (accepted? fixed "OPEN\nCLOSE\nPARENT-END\n" 1) => #t)
      (check (accepted? fixed "OPEN\nCLOSE\n" 7) => #f))
    (test-case "named recovery matches source name, not unrelated closers"
      (check (accepted? named "OPEN:child\nCLOSE:other\nCLOSE:child\n") => #t)
      (check (accepted? named "OPEN:child\nCLOSE:other\n") => #f)
      (check (accepted? named "OPEN:child\nPARENT-END\nCLOSE:child\n" 1) => #f))
    (test-case "named parent boundary uses saved source name"
      (check (accepted? named "OPEN:child\nPARENT:OPEN\nCLOSE:child\n"
                        2 named-parents) => #f)
      (check (accepted? named "OPEN:child\nPARENT:other\nCLOSE:child\n"
                        2 named-parents) => #t))
    (test-case "heading and indentation policies are explicit"
      (check (accepted? named "OPEN:child\n* heading\nCLOSE:child\n") => #f)
      (check (accepted? (make-source-named-boundary "OPEN:" "CLOSE:" "" heading
                                                  #f #t #f #t)
                        "OPEN:child\n* heading\n  CLOSE:CHILD\n") => #t)
      (check (accepted? (make-source-named-boundary "OPEN:" "CLOSE:" "" heading
                                                  #f #f #f #f)
                        "OPEN:child\n  CLOSE:child\n") => #f))
    (test-case "terminated names and suffixes are declaration driven"
      (check (accepted? (make-source-named-boundary "IN{" "OUT{" "}" heading
                                                  "}" #t #f #t)
                        "IN{child}\nOUT{child}\n") => #t))
    (test-case "invalid shapes and ambiguous parent bindings fail admission"
      (check-exception (make-source-named-boundary "" "END" "" heading) true)
      (check-exception (make-source-boundary-parent 0 "END") true)
      (check-exception (make-source-boundary-parent 1 "END" 'from #f) true)
      (check-exception (make-source-boundary-parent 1 "END" #f #f "}") true)
      (check-exception (make-source-boundary-parent 1 "END" #f #f "" #f) true)
      (check-exception
        (source-container-boundary-condition
          (make-source-block-boundary
            (make-block-line "OPEN" "CLOSE" #f #t 'Block 'Begin 'Body 'End
                             'recover-as-text #t #f)
            heading)
          parents 'frames) true)
      (check-exception (source-container-boundary-condition named
                          (append parents parents) 'frames) true)
      (check-exception (source-container-boundary-condition fixed named-parents 'frames) true)
      (check-exception (source-container-boundary-condition #f parents 'frames) true))))

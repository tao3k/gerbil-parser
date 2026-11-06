;;; Native test discovery is owned by init/Core; this Suite launches no runner.
(import (rename-in (only-in :gerbil/tools/gxtest main) (main gxtest-main))
        :std/test
        (only-in :clan/poo/object .get .call)
        (only-in "../../init.ss" gerbil-parser.init.exports))
(extern namespace: gerbil/tools/gxtest collect-files)
(export test-plan-test)
(def test-plan-test
  (test-suite "Core-owned native test selection"
    (test-case "native directories retain actual Suite inputs"
      (let* ((tests (.get gerbil-parser.init.exports tests))
             (files (.get (.call tests .for-files (collect-files ["t/..." "languages/..."])) spec)))
        (check (> (length files) 0) => #t)
        (for-each (lambda (file)
                    (check (file-exists? file) => #t)
                    (check (string-suffix? "-test.ss" file) => #t)) files)
        (check (and (member (path-expand "t/native-function-boundary-test.ss") files) #t) => #t)
        (check (and (member (path-expand "languages/arithmetic/parser-test.ss") files) #t) => #t)))))

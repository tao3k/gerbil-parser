;;; -*- Gerbil -*-
;;; Closed tool fixtures and descriptor/native/build admission test execution.
(import (only-in :std/test check check-exception)
        (only-in :std/misc/process run-process)
        (only-in :std/string/misc string-trim-eol)
        (only-in :clan/poo/object .ref)
        (only-in ../language/entry parse-language-source)
        (only-in ../language/tlc qualify-language-tlc-model tla-plus-model-receipt->alist)
        (only-in ../ffi/language-artifact-codec bind-native-language native-parse-binary-payload/bytes)
        (only-in ../ffi/rust-rowan-aot-v1 native-rust-rowan-source)
        (only-in :std/vector/u8vector little u8vector-u32-ref))
(export language-model-test-receipt check-language-native-entry check-language-portable-rejection)

(def (call-with-tool-test-directory procedure)
  (let* ((template (path-expand "gerbil-parser-model-test.XXXXXX" (getenv "TMPDIR" "/tmp")))
         (directory (string-trim-eol (run-process ["mktemp" "-d" template]))))
    (unwind-protect (procedure directory)
      (when (file-exists? directory) (delete-file-or-directory directory #t)))))

(def (write-test-file path text)
  (call-with-output-file path (lambda (port) (write-string text port))))

;;; Single quote shell data, including embedded quotes. No fixture text is
;;; evaluated as code; stdout and exit status are the entire tool algebra.
(def (shell-quote-data text)
  (call-with-output-string
   (lambda (port)
     (write-char #\' port)
     (string-for-each (lambda (ch)
                        (if (char=? ch #\') (write-string "'\\''" port) (write-char ch port))) text)
     (write-char #\' port))))

(def (language-model-test-receipt loader model source config tool workers)
  (call-with-tool-test-directory
   (lambda (directory)
     (let ((spec (path-expand (string-append model ".tla") directory))
           (cfg (path-expand (string-append model ".cfg") directory))
           (executable (path-expand "tool-fixture" directory)))
       (write-test-file spec source) (write-test-file cfg config)
       (unless (eq? (car tool) 'unresolved)
         (write-test-file executable
                          (string-append "#!/bin/sh\nprintf '%s' " (shell-quote-data (cadr tool))
                                         "\nexit " (number->string (caddr tool)) "\n"))
         (run-process ["chmod" "+x" executable]))
       (tla-plus-model-receipt->alist
        (qualify-language-tlc-model (.ref loader 'descriptor) spec cfg
          tlc: (if (eq? (car tool) 'unresolved) "this-tool-must-not-be-resolved" executable)
          workers: workers))))))

(def (check-language-native-entry loader source artifact accepted?)
  (let* ((descriptor (.ref loader 'descriptor)) (native (bind-native-language descriptor)))
    (check artifact => (parse-language-source descriptor source))
    (check (u8vector-u32-ref (native-parse-binary-payload/bytes native (string->utf8 source)) 8 little)
           => (if accepted? 0 1))))

(def (check-language-portable-rejection module-path message)
  (check-exception (native-rust-rowan-source module-path)
                   (lambda (exception) (equal? (error-message exception) message))))

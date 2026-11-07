;;; One separately compiled encoder factory supplies every measurement caller.
(export call-with-record-sdk-control require-record-sdk-code-identity!)

(def (call-with-record-sdk-control continuation)
  ;; One compiled encoder closure owns its payload and complete expected result.
  ;; Diagnostics may reuse it across phases without replacing native imports.
  (let* ((source (make-string 4096 #\a)) (expected (string->utf8 source)))
    (continuation (lambda () (string->utf8 source)) expected)))

(def (require-record-sdk-code-identity! retained measured)
  (unless (and (##closure? retained) (##closure? measured)
               (eq? (##closure-code retained) (##closure-code measured)))
    (error "SDK measurement callers do not share one encoder entry"))
  (displayln "SDK-ENCODER-ENTRY-IDENTICAL")
  (force-output))

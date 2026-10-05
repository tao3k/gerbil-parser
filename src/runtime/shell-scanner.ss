;;; -*- Gerbil -*-
;;; Bash 5.3 command scanning with immutable deferred here-document state.

(import (only-in :gerbil-parser/src/runtime/source-scanner
                 make-source-scanner source-scanner-tokens)
        (only-in :gerbil-parser/src/runtime/contextual-scanner
                 +empty-delimiter-queue+ delimiter-queue-empty? delimiter-queue-list
                 delimiter-queue-enqueue delimiter-queue-take decode-marker
                 delimiter-obligation? delimiter-obligation-marker
                 delimiter-obligation-quoted? delimiter-obligation-strip-tabs?)
        (only-in :gerbil-parser/src/runtime/region-scanner
                 source-prefix-at? region-plan-operator region-plan-end))
(export make-shell-scanner shell-scan
        parse-heredoc-delimiter
        shell-heredoc? shell-heredoc-delimiter
        shell-heredoc-quoted? shell-heredoc-strip-tabs?
        shell-lex-context? shell-lex-context-pending shell-lex-context-active)

;;; Shell syntax views use the engine obligation/queue representation.
(def shell-heredoc? delimiter-obligation?)
(def shell-heredoc-delimiter delimiter-obligation-marker)
(def shell-heredoc-quoted? delimiter-obligation-quoted?)
(def shell-heredoc-strip-tabs? delimiter-obligation-strip-tabs?)
(defstruct shell-lex-context (queue active expecting) transparent: #t)
(def (shell-lex-context-pending context)
  (delimiter-queue-list (shell-lex-context-queue context)))

(def (line-end source start)
  (let (length (string-length source))
    (let loop ((offset start))
      (cond
       ((= offset length) offset)
       ((char=? (string-ref source offset) #\newline) (fx+ offset 1))
       (else (loop (fx+ offset 1)))))))

(def (line-content-end source start end)
  (if (and (> end start)
           (char=? (string-ref source (fx- end 1)) #\newline))
    (fx- end 1) end))

(def (strip-leading-tabs source start end)
  (let loop ((offset start))
    (if (and (< offset end) (char=? (string-ref source offset) #\tab))
      (loop (fx+ offset 1)) offset)))

;;; Here-document delimiters undergo quote removal, not shell expansion.
(def (parse-heredoc-delimiter word strip-tabs?)
  (decode-marker 'shell-quote-removal word strip-tabs?))

(def (activate-next pending)
  (if (delimiter-queue-empty? pending)
    (values #f pending)
    (delimiter-queue-take pending)))

(def (make-shell-token-scanner regions)
  (lambda (source offset context _mode)
  (let* ((length (string-length source))
         (pending (shell-lex-context-queue context))
         (active (shell-lex-context-active context))
         (expecting (shell-lex-context-expecting context)))
    (cond
     (active
      (when (= offset length)
        (error "unterminated Bash here-document"
               (shell-heredoc-delimiter active)))
      (let* ((end (line-end source offset))
             (content-end (line-content-end source offset end))
             (start (if (shell-heredoc-strip-tabs? active)
                      (strip-leading-tabs source offset content-end)
                      offset)))
        (if (string=? (substring source start content-end)
                      (shell-heredoc-delimiter active))
          (let-values (((next-active rest) (activate-next pending)))
            (values 'heredoc-end end
                    (make-shell-lex-context rest next-active #f)))
          (values 'heredoc-content end context))))
     ((= offset length)
      (when (or (not (delimiter-queue-empty? pending)) expecting)
        (error "unfinished Bash here-document"))
      (values #f offset context))
     ((or (char=? (string-ref source offset) #\space)
          (char=? (string-ref source offset) #\tab))
      (let loop ((end (fx+ offset 1)))
        (if (and (< end length)
                 (or (char=? (string-ref source end) #\space)
                     (char=? (string-ref source end) #\tab)))
          (loop (fx+ end 1))
          (values 'horizontal-whitespace end context))))
     ((char=? (string-ref source offset) #\newline)
      (when expecting (error "missing Bash here-document delimiter"))
      (let-values (((next-active rest) (activate-next pending)))
        (values 'newline (fx+ offset 1)
                (make-shell-lex-context rest next-active #f))))
     ((source-prefix-at? source offset "\\\n")
      (values 'line-continuation (fx+ offset 2) context))
     (expecting
      (let* ((end (region-plan-end regions source offset))
             (word (substring source offset end))
             (heredoc
              (parse-heredoc-delimiter word (string=? expecting "<<-"))))
        (values 'heredoc-marker end
                (make-shell-lex-context
                 (delimiter-queue-enqueue pending heredoc) active #f))))
     ((char=? (string-ref source offset) #\#)
      (values 'comment
              (line-content-end source offset (line-end source offset))
              context))
     ((or (source-prefix-at? source offset "<(")
          (source-prefix-at? source offset ">("))
      (values 'word (region-plan-end regions source offset) context))
     (else
      (let (operator (region-plan-operator regions source offset))
        (if operator
          (values 'operator (+ offset (string-length operator))
                  (if (or (string=? operator "<<")
                          (string=? operator "<<-"))
                    (make-shell-lex-context pending active operator)
                    context))
          (values 'word (region-plan-end regions source offset) context)))))))

)

(def (make-shell-scanner regions source)
  (make-source-scanner source (make-shell-lex-context +empty-delimiter-queue+ #f #f)
                       (make-shell-token-scanner regions)))

(def (shell-scan regions source)
  (source-scanner-tokens (make-shell-scanner regions source) 'command))

;;; -*- Gerbil -*-
;;; Bash 5.3 command scanning with immutable deferred here-document state.

(import (only-in :gerbil-parser/src/runtime/source-scanner
                 make-source-scanner source-scanner-tokens)
        (only-in :gerbil-parser/src/runtime/contextual-scanner
                 +empty-delimiter-queue+ delimiter-queue-empty? delimiter-queue-list
                 delimiter-queue-enqueue delimiter-queue-take decode-marker
                 delimiter-obligation? delimiter-obligation-marker
                 delimiter-obligation-quoted? delimiter-obligation-strip-tabs?)
        (only-in ./grammar bash-word-regions)
        (only-in :gerbil-parser/src/runtime/region-scanner
                 source-prefix-at? region-plan-operator region-plan-end))
(export make-bash-scanner bash-scan
        parse-heredoc-delimiter
        bash-heredoc? bash-heredoc-delimiter
        bash-heredoc-quoted? bash-heredoc-strip-tabs?
        bash-lex-context? bash-lex-context-pending bash-lex-context-active)

;;; Public syntax adapters retain the language API; immutable obligation and
;;; queue execution have one engine owner, shared with closed Scanner IR.
(def bash-heredoc? delimiter-obligation?)
(def bash-heredoc-delimiter delimiter-obligation-marker)
(def bash-heredoc-quoted? delimiter-obligation-quoted?)
(def bash-heredoc-strip-tabs? delimiter-obligation-strip-tabs?)
(defstruct bash-lex-context (queue active expecting) transparent: #t)
(def (bash-lex-context-pending context)
  (delimiter-queue-list (bash-lex-context-queue context)))

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

(def (scan-bash-token source offset context _mode)
  (let* ((length (string-length source))
         (pending (bash-lex-context-queue context))
         (active (bash-lex-context-active context))
         (expecting (bash-lex-context-expecting context)))
    (cond
     (active
      (when (= offset length)
        (error "unterminated Bash here-document"
               (bash-heredoc-delimiter active)))
      (let* ((end (line-end source offset))
             (content-end (line-content-end source offset end))
             (start (if (bash-heredoc-strip-tabs? active)
                      (strip-leading-tabs source offset content-end)
                      offset)))
        (if (string=? (substring source start content-end)
                      (bash-heredoc-delimiter active))
          (let-values (((next-active rest) (activate-next pending)))
            (values 'heredoc-end end
                    (make-bash-lex-context rest next-active #f)))
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
                (make-bash-lex-context rest next-active #f))))
     ((source-prefix-at? source offset "\\\n")
      (values 'line-continuation (fx+ offset 2) context))
     (expecting
      (let* ((end (region-plan-end bash-word-regions source offset))
             (word (substring source offset end))
             (heredoc
              (parse-heredoc-delimiter word (string=? expecting "<<-"))))
        (values 'heredoc-marker end
                (make-bash-lex-context
                 (delimiter-queue-enqueue pending heredoc) active #f))))
     ((char=? (string-ref source offset) #\#)
      (values 'comment
              (line-content-end source offset (line-end source offset))
              context))
     ((or (source-prefix-at? source offset "<(")
          (source-prefix-at? source offset ">("))
      (values 'word (region-plan-end bash-word-regions source offset) context))
     (else
      (let (operator (region-plan-operator bash-word-regions source offset))
        (if operator
          (values 'operator (+ offset (string-length operator))
                  (if (or (string=? operator "<<")
                          (string=? operator "<<-"))
                    (make-bash-lex-context pending active operator)
                    context))
          (values 'word (region-plan-end bash-word-regions source offset) context)))))))

(def (make-bash-scanner source)
  (make-source-scanner source (make-bash-lex-context +empty-delimiter-queue+ #f #f)
                       scan-bash-token))

(def (bash-scan source)
  (source-scanner-tokens (make-bash-scanner source) 'command))

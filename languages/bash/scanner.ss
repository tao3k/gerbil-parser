;;; -*- Gerbil -*-
;;; Bash 5.3 command scanning with immutable deferred here-document state.

(import (only-in :gerbil-parser/src/runtime/source-scanner
                 make-source-scanner source-scanner-tokens)
        (only-in ./scan-words
                 bash-at? bash-operator-at bash-word-end))
(export make-bash-scanner bash-scan
        parse-heredoc-delimiter
        bash-heredoc? bash-heredoc-delimiter
        bash-heredoc-quoted? bash-heredoc-strip-tabs?
        bash-lex-context? bash-lex-context-pending bash-lex-context-active)

(defstruct bash-heredoc (delimiter quoted? strip-tabs?) transparent: #t)
(defstruct bash-lex-context (pending active expecting) transparent: #t)

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
  (let ((length (string-length word)) (quoted? #f))
    (let loop ((offset 0) (quote-mode #f) (characters '()))
      (if (= offset length)
        (begin
          (when quote-mode (error "unterminated here-document delimiter quote"))
          (make-bash-heredoc (list->string (reverse characters))
                            quoted? strip-tabs?))
        (let (character (string-ref word offset))
          (cond
           ((and (not (eq? quote-mode 'single)) (char=? character #\\))
            (set! quoted? #t)
            (if (< (fx+ offset 1) length)
              (loop (fx+ offset 2) quote-mode
                    (cons (string-ref word (fx+ offset 1)) characters))
              (loop (fx+ offset 1) quote-mode (cons character characters))))
           ((and (not (eq? quote-mode 'double)) (char=? character #\'))
            (set! quoted? #t)
            (loop (fx+ offset 1) (if quote-mode #f 'single) characters))
           ((and (not (eq? quote-mode 'single)) (char=? character #\"))
            (set! quoted? #t)
            (loop (fx+ offset 1) (if quote-mode #f 'double) characters))
           (else
            (loop (fx+ offset 1) quote-mode (cons character characters)))))))))

(def (activate-next pending)
  (if (pair? pending)
    (values (car pending) (cdr pending))
    (values #f '())))

(def (scan-bash-token source offset context _mode)
  (let* ((length (string-length source))
         (pending (bash-lex-context-pending context))
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
      (when (or (pair? pending) expecting)
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
     ((bash-at? source offset "\\\n")
      (values 'line-continuation (fx+ offset 2) context))
     (expecting
      (let* ((end (bash-word-end source offset))
             (word (substring source offset end))
             (heredoc
              (parse-heredoc-delimiter word (string=? expecting "<<-"))))
        (values 'heredoc-marker end
                (make-bash-lex-context
                 (append pending (list heredoc)) active #f))))
     ((char=? (string-ref source offset) #\#)
      (values 'comment
              (line-content-end source offset (line-end source offset))
              context))
     ((or (bash-at? source offset "<(")
          (bash-at? source offset ">("))
      (values 'word (bash-word-end source offset) context))
     (else
      (let (operator (bash-operator-at source offset))
        (if operator
          (values 'operator (+ offset (string-length operator))
                  (if (or (string=? operator "<<")
                          (string=? operator "<<-"))
                    (make-bash-lex-context pending active operator)
                    context))
          (values 'word (bash-word-end source offset) context)))))))

(def (make-bash-scanner source)
  (make-source-scanner source (make-bash-lex-context '() #f #f)
                       scan-bash-token))

(def (bash-scan source)
  (source-scanner-tokens (make-bash-scanner source) 'command))

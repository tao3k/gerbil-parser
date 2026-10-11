;;; -*- Gerbil -*-
;;; Build-time oracle produces literal controls for independently compiled code.
(import (only-in :gerbil-parser/t/event-strategy-fixture event-lines-language-grammar)
        (only-in :gerbil-parser/src/compiler/event-fold-scheme event-fold-scheme-source)
        (only-in :gerbil-parser/src/compiler/event-fold-runtime run-event-fold)
        (only-in :gerbil-parser/src/compiler/event-fold-ir event-fold-ir))
(export main)

(def predicates
  (append '((or (line-starts-with "ab") (line-starts-with "a")
                (line-starts-with "é") (line-starts-with "λ"))
            (or (line-starts-with-ascii-ci "ABC") (line-starts-with-ascii-ci "A")
                (line-starts-with-ascii-ci "B") (line-starts-with-ascii-ci "AB"))
            (or (state ready) (line-starts-with "a")
                (line-starts-with "ab") (line-starts-with "é"))
            (bool #t) (state ready)
    (uint-positive? (uint 1)) (uint-equal? (uint 1) (uint 1))
    (uint-not-equal? (uint 1) (uint 2)) (uint-greater? (uint 2) (uint 1))
    (offset-less? start end) (stack-nonempty? frames)
    (not (state ready)) (and (bool #t) (state ready))
    (or (state ready) (bool #t))
    (line-starts-with "a") (line-starts-with-ascii-ci "A")
    (line-prefix-boundary-ascii-ci "a") (line-marker-ascii-ci "a")
    (line-blank?) (line-has-word-after-prefix? "a")
    (line-has-key-after-prefix? "a")
    (line-byte-equal? start 97)
    (line-bytes-all-in? start end (9 10 13 32 97 98))
    (line-bytes-any-in? start end (97 98))
    (line-bytes-in-set? start (line-content-end) ("a" "ab"))
    (source-slices-equal? start end start end)
    (source-slices-equal-ascii-ci? start end start end)
    (future-line-marker-before-boundary? "END" "" "*" " " #t #f "")
    (future-heading-title? "*" " " (uint 1) "END")
    (future-named-line-marker-before-boundary?
     start (line-content-end) "END " "" "" "*" " " #t #f #t)
    (future-named-line-marker-before-boundary?
     start (line-content-end) "END " "" "" "*" " " #t #f #t
     start (line-content-end) "STOP " "" #t))
   (list
    (cons 'and (append (make-list 96 '(state ready)) '((not (state ready)))))
    (cons 'or (append (make-list 96 '(not (state ready))) '((state ready))))
    (cons 'or (make-list 96 '(not (state ready))))
    `(line-bytes-in-set? start (line-content-end)
                        ,(map (lambda (index) (string (integer->char (+ 32 index)))) (iota 95))))))

(def offsets
  '(start end (line-prefix-end "a") (line-marker-end "*" " ")
    (line-skip-horizontal start) (line-scan-word start) (line-scan-key start)
    (line-scan-nonspace-until start ":") (line-scan-until start ":")
    (line-physical-end start) (line-step start) (line-trim-end)
    (line-trim-end-from start) (line-content-end) (state-offset saved)))

(def initial '((ready #f) (count 0) (saved 0) (frames (uint-stack))))
(def helpers
  '((slice ((parameter 0) (ready #f))
      ((start-node Text)
       (if (uint-positive? (state parameter))
           ((token Line start end)) ())
       (if (line-starts-with-ascii-ci "A")
           ((start-node Heading) (finish-node)) ())
       (with-source-bounds start end
         ((if (line-bytes-any-in? start end (97))
              ((token Line start end)) ())))
       (finish-node))
      (parameter))))

(def specifications
  (list
   (list 'native-predicates initial
         (append '((set-bool ready (bool #t)) (push-frame frames (uint 1)))
           (map (lambda (predicate)
                  `(if ,predicate
                     ((start-node Heading) (token Line start end) (finish-node))
                     ((start-node Text) (token Line start end) (finish-node))))
                predicates)
           '((pop-frame frames)))
         '() '())
   (list 'native-offsets initial
         (append '((set-uint saved (offset start)))
                 (map (lambda (offset) `(token Line ,offset end)) offsets)
                 '((for-line-bytes cursor start end
                     ((token Line (line-index cursor) (line-step (line-index cursor)))))))
         '() '())
   (list 'native-states initial
         '((set-uint count (uint-add (state count) (uint 1)))
           (set-uint saved (uint-divide (uint-multiply (uint 2) (uint 3)) (uint 0)))
           (set-uint saved (line-indent-column 8))
           (set-uint saved (line-marker-level "*" " "))
           (open-level frames (state count) Section)
           (if (uint-positive? (stack-top frames)) ((token Line start end)) ())
           (close-through frames (state count))
           (open-level frames (uint 1) Section)
           (close-all frames)
           (push-frame frames (uint 2))
           (close-frame frames 2)
           (push-frame frames (uint 3))
           (close-frames-while frames (uint-positive? (stack-top frames)) 2)
           (push-frame frames (uint 4))
           (close-all-frames frames 2))
         '((if (uint-positive? (state count)) ((start-node Text) (finish-node)) ())) '())
   (list 'native-helpers initial
         '((join-once handled
             ((if (line-starts-with "a")
                  ((set-bool handled (bool #t))
                   (call-source-helper slice start end ((uint 1)))) ()))
             ((with-source-bounds start end
                ((call-source-helper slice start end ((uint 1))))))))
         '() helpers)
   (list 'native-markers
         '((present #f) (column 0) (ordered #f) (bullet-start 0) (bullet-end 0) (content 0))
         '((scan-list-marker "-+" #t 8 present column ordered bullet-start bullet-end content)
           (if (state present)
               ((token Line (state-offset bullet-start) (state-offset content))
                (token Line (state-offset content) end))
               ((token Line start end))))
         '() '())
   (list 'native-saved '((parameter 0) (saved 0) (limit 0))
         '((set-uint saved (offset start)) (set-uint limit (offset end)))
         '((with-source-bounds (state-offset saved) (state-offset limit)
             ((call-source-helper slice start end ((state parameter))))))
         helpers)))

(def corpus
  '("" "a" "ab\n" "a b:\r\nλ" " \t\r\n" "** END\nbody\n"
    "END a\nEND ab\n" "- item\n  12) ordered\n" "é\r\nλ\rtail"))

(def (generation-controls)
  (def (rejected initial line finish helpers)
    (unless
        (with-catch (lambda (_) #t)
          (lambda ()
            (event-fold-scheme-source 'invalid event-lines-language-grammar 'Document
                                      initial line finish helpers)
            #f))
      (error "native compiler admitted invalid declaration")))
  (rejected '() '((unknown-transition)) '() '())
  (rejected '((flag #f)) '((set-uint flag (uint 1))) '() '())
  (rejected '() '((call-source-helper missing start end)) '() '())
  (rejected '() '() '()
            '((recursive () ((call-source-helper recursive start end)))))
  (let (cycle (list '(token Line start end)))
    (set-cdr! cycle cycle)
    (rejected '() cycle '() '()))
  (displayln "GENERATION-OK rejection-controls")
  (force-output))

(def (native-boundary-controls port)
  ;; The interpreted decoder is an independent oracle for borrowed UTF-8
  ;; bounds, including valid empty spans inside a multibyte character.
  (map
   (lambda (spec)
     (let* ((name (car spec)) (line (cadr spec)) (helpers (caddr spec))
            (input "é\n") (initial '((cut 1)))
            (expected
             (with-catch (lambda (_) 'rejected)
               (lambda () (run-event-fold input 'Document initial line '() helpers)))))
       (unless (eq? (eq? expected 'rejected) (cadddr spec))
         (error "UTF-8 boundary oracle disagrees with control" name expected))
       (display (event-fold-scheme-source name event-lines-language-grammar 'Document
                                         initial line '() helpers) port)
       `(begin
          (unless (equal? (with-catch (lambda (_) 'rejected)
                           (lambda () (,name ,input))) ',expected)
            (error "native borrowed UTF-8 boundary mismatch" ',name))
          (displayln "CASE-OK " ',name) (force-output))))
   '((native-split-start
      ((with-source-bounds (state-offset cut) end ((token Line start end)))) () #t)
     (native-split-end
      ((with-source-bounds start (state-offset cut) ((token Line start end)))) () #t)
     (native-split-helper
      ((call-source-helper slice (state-offset cut) end))
      ((slice () ((token Line start end)))) #t)
     (native-empty-midpoint
      ((with-source-bounds (state-offset cut) (state-offset cut) ((token Line start end)))) () #f))))

(def (main output)
  (generation-controls)
  (call-with-output-file output
    (lambda (port)
      (let (controls '())
        (for-each
         (lambda (spec)
           (let* ((name (car spec)) (initial (cadr spec)) (line (caddr spec))
                  (finish (cadddr spec)) (helpers (list-ref spec 4))
                  (digest (hash-get
                           (event-fold-ir name event-lines-language-grammar 'Document
                                          initial line finish helpers)
                           "parser_digest"))
                  (binder (string->symbol (string-append "bind-" (symbol->string name)))))
             (let (source
                   (event-fold-scheme-source
                    name event-lines-language-grammar 'Document initial line finish helpers))
               (unless (equal? source
                         (event-fold-scheme-source
                          name event-lines-language-grammar 'Document initial line finish helpers))
                 (error "native fold source is not deterministic" name))
               (display source port))
             (set! controls (cons `(unless (eq? (,binder ,digest) ,name)
                                     (error "native binding did not return executor"))
                                  controls))
             (set! controls
                   (cons `(unless
                             (with-catch (lambda (_) #t)
                               (lambda () (,binder "wrong-digest") #f))
                             (error "native binding admitted wrong identity"))
                         controls))
             (for-each
              (lambda (source)
                (let (expected (run-event-fold source 'Document initial line finish helpers))
                  (set! controls
                        (cons `(begin
                                  (unless (equal? (,name ,source) ',expected)
                                    (error "native EventFold mismatch" ',name ,source))
                                  (displayln "CASE-OK " ',name)
                                  (force-output))
                              controls))))
              corpus)
             (let (parameter (car (filter
                                   (lambda (entry) (exact-integer? (cadr entry))) initial)))
               (for-each
                (lambda (source)
                  (for-each
                   (lambda (value)
                     (let* ((overrides (list (cons (car parameter) value)))
                            (expected (run-event-fold source 'Document initial
                                                      line finish helpers overrides)))
                       (set! controls
                             (cons `(begin
                                       (unless (equal? (,name ,source ',overrides) ',expected)
                                         (error "native fold override or isolation mismatch" ',name))
                                       (displayln "CASE-OK parameters " ',name)
                                       (force-output))
                                   controls))))
                   '(0 7 0)))
                '("a\n" "")))
             ;; Hold published output while reusing the executor, then mutate
             ;; only its caller-owned spine. Neither later calls nor the
             ;; empty root may share those cells with a prior publication.
             (let ((first-source "é\r\nλ\rtail")
                   (expected (run-event-fold "é\r\nλ\rtail" 'Document initial line finish helpers))
                   (empty (run-event-fold "" 'Document initial line finish helpers)))
               (set! controls
                 (cons `(let* ((held (,name ,first-source)) (other (,name "")))
                          (unless (and (equal? held ',expected) (equal? other ',empty)
                                       (equal? (,name ,first-source) ',expected)
                                       (equal? held ',expected) (not (eq? held other)))
                            (error "native publication retained a request spine" ',name))
                          (set-cdr! held '())
                          (set-cdr! other '())
                          (unless (and (equal? (,name ,first-source) ',expected)
                                       (equal? (,name "") ',empty))
                            (error "native publication shares caller-owned cells" ',name))
                          (displayln "CASE-OK publication " ',name)
                          (force-output))
                       controls)))
             (displayln "GENERATION-OK " name)
             (force-output)))
         specifications)
        (set! controls (append (reverse (native-boundary-controls port)) controls))
        (write '(export main) port) (newline port)
        (write `(def (main . args) ,@(reverse controls) (displayln "OK") (force-output)) port)
        (newline port)))))

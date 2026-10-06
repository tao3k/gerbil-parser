;;; -*- Gerbil -*-
;;; Lossless word decomposition with explicit quote and expansion boundaries.

(import (only-in :gerbil-parser/src/runtime/recognition
                 make-recognition-child prepare-recognition-source
                 recognition-source-token recognition-source-offset)
        (only-in ../language/part-profile part-plan-context part-context-match part-context-literal-end part-plan-literal
                 part-step-operation part-step-kind part-step-opening part-step-closing part-step-delimiter part-plan-name-end)
        (only-in ../language/result-profile result-plan-node result-plan-token)
        (only-in :gerbil-parser/src/runtime/funcs
                 recognition-sequence-append recognition-sequence-concatenate recognition-sequence->list)
        (only-in :gerbil-parser/src/runtime/token token-lexeme)
        (only-in :gerbil-parser/src/runtime/region-scanner
                 source-prefix-at? prepare-region-source region-source-quote-end region-source-pair-end))
(export make-shell-word-parser)

(defstruct shell-word-source (spans text regions))

(def (make-shell-word-parser regions results parts-plan)
(def literal-kind (part-plan-literal parts-plan))
(def word-context (part-plan-context parts-plan 'word))
(def here-context (part-plan-context parts-plan 'HereDocument))
(def (prepare-word token)
  ;; Scanner-created token lexemes are read-only throughout this engine call.
  ;; Result publication owns its admitted copy; region preparation is needed
  ;; only when an actual quote or substitution is encountered.
  (let* ((spans (prepare-recognition-source token)) (text (token-lexeme token)))
    (values (make-shell-word-source spans text #f) text)))

(def (word-regions source)
  (or (shell-word-source-regions source)
      (let (context (prepare-region-source regions (shell-word-source-text source)))
        (shell-word-source-regions-set! source context)
        context)))

(def (slice-token raw kind start end)
  (result-plan-token results
    (recognition-source-token (shell-word-source-spans raw) kind start end)))

(def (child field value)
  (make-recognition-child field value))

(def (node raw kind start end children)
  (let (spans (shell-word-source-spans raw))
    (result-plan-node results kind (recognition-source-offset spans start)
                      (recognition-source-offset spans end) children)))

(def (leaf raw kind start end)
  (let (token (slice-token raw kind start end))
    (values (node raw kind start end (list (child 'text token)))
            (list token))))

(def (parameter-operator text start end)
  (let loop ((operators
             '(":-" ":=" ":+" ":?" "##" "%%" "//"
               "^^" ",," "~~" ":" "-" "=" "+" "?"
               "#" "%" "/" "@" "^" "," "~")))
    (and (pair? operators)
         (if (and (<= (+ start (string-length (car operators))) end)
                  (source-prefix-at? text start (car operators)))
           (car operators)
           (loop (cdr operators))))))

(def (parameter-name-end text start end)
  (if (>= start end) start
    (let (first (string-ref text start))
      (cond
       ((memv first '(#\@ #\* #\? #\# #\$ #\! #\-))
        (fx+ start 1))
       ((char-numeric? first)
        (let loop ((offset (fx+ start 1)))
          (if (and (< offset end)
                   (char-numeric? (string-ref text offset)))
            (loop (fx+ offset 1)) offset)))
       (else
        (let loop ((offset start))
          (if (and (< offset end)
                   (let (character (string-ref text offset))
                     (or (char-alphabetic? character)
                         (char-numeric? character)
                         (char=? character #\_))))
            (loop (fx+ offset 1)) offset)))))))

(def (subscript-end raw text start end)
  (let loop ((offset (fx+ start 1)) (depth 1))
    (when (>= offset end)
      (error "unterminated Bash array subscript" start))
    (let (character (string-ref text offset))
      (cond
       ((char=? character #\\)
        (loop (min end (fx+ offset 2)) depth))
       ((or (char=? character #\') (char=? character #\"))
        (loop (region-source-quote-end (word-regions raw) offset character) depth))
       ((or (source-prefix-at? text offset "${")
            (source-prefix-at? text offset "$("))
        (loop (region-source-pair-end (word-regions raw) offset) depth))
       ((char=? character #\[)
        (loop (fx+ offset 1) (fx+ depth 1)))
       ((char=? character #\])
        (if (= depth 1) (fx+ offset 1)
          (loop (fx+ offset 1) (fx- depth 1))))
       (else (loop (fx+ offset 1) depth))))))

(def (parse-parameter raw text start end)
  (let* ((body-start (fx+ start 2))
         (body-end (fx- end 1))
         (prefix?
          (and (< (fx+ body-start 1) body-end)
               (memv (string-ref text body-start) '(#\# #\!))))
         (name-start (if prefix? (fx+ body-start 1) body-start))
         (parameter-end (parameter-name-end text name-start body-end))
         (subscript-start
          (and (< parameter-end body-end)
               (char=? (string-ref text parameter-end) #\[)
               parameter-end))
         (after-subscript
          (if subscript-start
            (subscript-end raw text subscript-start body-end)
            parameter-end))
         (operator (and (< after-subscript body-end)
                        (parameter-operator text after-subscript body-end)))
         (operator-end (if operator
                         (+ after-subscript (string-length operator))
                         after-subscript))
         (open (slice-token raw 'parameter-open start body-start)))
    (let-values (((operand-children operand-tokens)
                  (parse-parts raw text operator-end body-end #f)))
      (let-values (((subscript-node subscript-tokens)
                    (if subscript-start
                      (let* ((inner-start (fx+ subscript-start 1))
                             (inner-end (fx- after-subscript 1))
                             (open-bracket
                              (slice-token raw 'subscript-open
                                           subscript-start inner-start))
                             (close-bracket
                              (slice-token raw 'subscript-close
                                           inner-end after-subscript)))
                        (let-values (((parts tokens)
                                      (parse-parts raw text inner-start
                                                   inner-end #f)))
                          (values
                           (node raw 'ArraySubscript
                                 subscript-start after-subscript
                                 (append
                                  (list (child 'open open-bracket))
                                  (map (lambda (part) (child 'index part))
                                       parts)
                                  (list (child 'close close-bracket))))
                           (recognition-sequence-concatenate
                            (list (list open-bracket) tokens (list close-bracket))))))
                      (values #f '()))))
        (let* ((prefix-token
                (and prefix?
                     (slice-token raw 'parameter-prefix
                                  body-start name-start)))
               (name-token
                (and (> parameter-end name-start)
                   (slice-token raw 'parameter-name
                                name-start parameter-end)))
             (operator-token
              (and operator
                   (slice-token raw 'parameter-operator
                                after-subscript operator-end)))
             (close (slice-token raw 'parameter-close body-end end))
             (children
              (append
               (list (child 'open open))
               (if prefix-token (list (child 'prefix prefix-token)) '())
               (if name-token (list (child 'name name-token)) '())
               (if subscript-node
                 (list (child 'subscript subscript-node)) '())
               (if operator-token (list (child 'operator operator-token)) '())
               (map (lambda (part) (child 'operand part)) operand-children)
               (list (child 'close close))))
             (tokens
              (recognition-sequence-concatenate
               (list (list open)
                     (if prefix-token (list prefix-token) '())
                     (if name-token (list name-token) '())
                     subscript-tokens
                     (if operator-token (list operator-token) '())
                     operand-tokens (list close)))))
        (values (node raw 'ParameterExpansion start end children)
                tokens))))))

(def (parse-quoted raw text start end kind opening-length)
  (let* ((body-start (+ start opening-length))
         (body-end (fx- end 1))
         (open (slice-token raw 'quote-open start body-start)))
    (let-values (((parts interior)
                  (parse-parts raw text body-start body-end kind)))
      (let (close (slice-token raw 'quote-close body-end end))
        (values
         (node raw kind start end
               (append (list (child 'open open))
                       (map (lambda (part) (child 'part part)) parts)
                       (list (child 'close close))))
         (recognition-sequence-concatenate (list (list open) interior (list close))))))))

(def (parse-opaque-substitution raw text start end kind opening-length
                                closing-length)
  (let* ((body-start (+ start opening-length))
         (body-end (- end closing-length))
         (open (slice-token raw 'substitution-open start body-start)))
    (let* ((body (and (> body-end body-start)
                      (slice-token raw 'substitution-body
                                   body-start body-end)))
           (close (slice-token raw 'substitution-close body-end end))
           (tokens (append (list open) (if body (list body) '())
                           (list close)))
           (children
            (append (list (child 'open open))
                    (if body (list (child 'body body)) '())
                    (list (child 'close close)))))
      (values (node raw kind start end children) tokens))))

;;; Token publication shares immutable sequence branches across nested results.
;;; Only the public word/assignment/body boundary materializes the ordered list;
;;; nesting never copies a growing token suffix. No relocated child views enter
;;; this token-only sequence, so materialization retains the original tokens.
;;; Returns syntax parts and their nonoverlapping source token sequence.
(def (parse-parts raw text start end context)
  (let (context-plan (cond ((not context) word-context)
                           ((eq? context 'HereDocument) here-context)
                           (else (part-plan-context parts-plan context))))
    (let loop ((offset start) (parts '()) (tokens '()))
      (if (= offset end)
        (values (reverse parts) tokens)
        (let-values
            (((part produced next)
              (let* ((step (part-context-match context-plan text offset end))
                     (operation (and step (part-step-operation step)))
                     (name-end (and (eq? operation 'name)
                                    (part-plan-name-end step text (+ offset 1) end))))
                (cond
                 ((memq operation '(quote quoted-body))
                  (let (after (region-source-quote-end (word-regions raw)
                               (+ offset (- (part-step-opening step) 1)) (part-step-delimiter step)))
                    (let-values (((part produced)
                                  (if (eq? operation 'quote)
                                    (parse-quoted raw text offset after (part-step-kind step) (part-step-opening step))
                                    (parse-opaque-substitution raw text offset after (part-step-kind step)
                                     (part-step-opening step) (part-step-closing step)))))
                      (values part produced after))))
                 ((memq operation '(parameter pair))
                  (let (after (region-source-pair-end (word-regions raw) offset))
                    (let-values (((part produced)
                                  (if (eq? operation 'parameter)
                                    (parse-parameter raw text offset after)
                                    (parse-opaque-substitution raw text offset after (part-step-kind step)
                                     (part-step-opening step) (part-step-closing step)))))
                      (values part produced after))))
                 ((and name-end (> name-end (+ offset 1)))
                  (let-values (((part produced) (leaf raw (part-step-kind step) offset name-end)))
                    (values part produced name-end)))
                 ((eq? operation 'escape)
                  (let (after (min end (+ offset 2)))
                    (let-values (((part produced) (leaf raw (part-step-kind step) offset after)))
                      (values part produced after))))
                 (else
                  (let (after (part-context-literal-end context-plan text offset end))
                    (let-values (((part produced) (leaf raw literal-kind offset after)))
                      (values part produced after))))))))
          (loop next (cons part parts) (recognition-sequence-append tokens produced)))))))

(def (shell-word-components token)
  (let-values (((raw text) (prepare-word token)))
    (let (length (string-length text))
      (let-values (((parts tokens) (parse-parts raw text 0 length #f)))
        (values
         (node raw 'Word 0 length
               (map (lambda (part) (child 'part part)) parts))
         (recognition-sequence->list tokens))))))

;;; Unquoted here-document bodies expand parameters, commands, and arithmetic.
;;; Quote characters remain literal in this context.
(def (shell-here-content-components token)
  (let-values (((raw text) (prepare-word token)))
    (let (length (string-length text))
      (let-values (((parts tokens)
                    (parse-parts raw text 0 length 'HereDocument)))
        (values
         (node raw 'HereDocumentLine 0 length
               (map (lambda (part) (child 'part part)) parts))
         (recognition-sequence->list tokens))))))

(def (assignment-name-end text)
  (let (length (string-length text))
    (if (or (zero? length)
            (not (let (first (string-ref text 0))
                   (or (char-alphabetic? first) (char=? first #\_)))))
      #f
      (let loop ((offset 1))
        (if (and (< offset length)
                 (let (character (string-ref text offset))
                   (or (char-alphabetic? character)
                       (char-numeric? character)
                       (char=? character #\_))))
          (loop (fx+ offset 1)) offset)))))

;;; An assignment is recognized only at a command position by the caller.
;;; It returns #f for ordinary words, or a node and ordered source tokens.
(def (shell-assignment-components token)
  (let* ((text (token-lexeme token))
         (length (string-length text))
         (name-end (assignment-name-end text)))
    (if (and name-end
             (or (source-prefix-at? text name-end "=")
                 (source-prefix-at? text name-end "+=")))
      (let-values (((raw text) (prepare-word token)))
        (let* ((operator-end
                (if (source-prefix-at? text name-end "+=")
                  (fx+ name-end 2) (fx+ name-end 1)))
               (name (slice-token raw 'assignment-name 0 name-end))
               (operator (slice-token raw 'assignment-operator name-end operator-end)))
          (let-values (((parts tokens)
                        (parse-parts raw text operator-end length #f)))
            (values
             (node raw 'Assignment 0 length
                   (append (list (child 'name name) (child 'operator operator))
                           (map (lambda (part) (child 'value part)) parts)))
             (recognition-sequence->list
              (recognition-sequence-append (list name operator) tokens))))))
      (values #f #f))))

  (values shell-word-components shell-assignment-components shell-here-content-components))

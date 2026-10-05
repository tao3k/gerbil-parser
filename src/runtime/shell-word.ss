;;; -*- Gerbil -*-
;;; Lossless word decomposition with explicit quote and expansion boundaries.

(import (only-in :gerbil-parser/src/runtime/recognition
                 make-recognition-child make-recognition-node)
        (only-in :gerbil-parser/src/runtime/token
                 make-token token-end token-lexeme token-start)
        (only-in :gerbil-parser/src/runtime/region-scanner
                 source-prefix-at? region-plan-quote-end region-plan-pair-end))
(export make-shell-word-parser)

(def (make-shell-word-parser regions)
(def (byte-offset raw text character)
  (+ (token-start raw)
     (u8vector-length (string->utf8 (substring text 0 character)))))

(def (slice-token raw text kind start end)
  (make-token kind (substring text start end)
              (byte-offset raw text start)
              (byte-offset raw text end)))

(def (child field value)
  (make-recognition-child field value))

(def (node raw text kind start end children)
  (make-recognition-node kind
                         (byte-offset raw text start)
                         (byte-offset raw text end)
                         children))

(def (leaf raw text kind start end)
  (let (token (slice-token raw text kind start end))
    (values (node raw text kind start end (list (child 'text token)))
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

(def (subscript-end text start end)
  (let loop ((offset (fx+ start 1)) (depth 1))
    (when (>= offset end)
      (error "unterminated Bash array subscript" start))
    (let (character (string-ref text offset))
      (cond
       ((char=? character #\\)
        (loop (min end (fx+ offset 2)) depth))
       ((or (char=? character #\') (char=? character #\"))
        (loop (region-plan-quote-end regions text offset character) depth))
       ((or (source-prefix-at? text offset "${")
            (source-prefix-at? text offset "$("))
        (loop (region-plan-pair-end regions text offset) depth))
       ((char=? character #\[)
        (loop (fx+ offset 1) (fx+ depth 1)))
       ((char=? character #\])
        (if (= depth 1) (fx+ offset 1)
          (loop (fx+ offset 1) (fx- depth 1))))
       (else (loop (fx+ offset 1) depth))))))

(def (dollar-name-end text start end)
  (if (>= start end) start
    (let (first (string-ref text start))
      (if (or (char-numeric? first)
              (memv first '(#\? #\@ #\* #\# #\$ #\! #\- #\_ #\0)))
        (fx+ start 1)
        (let loop ((offset start))
          (if (and (< offset end)
                   (let (character (string-ref text offset))
                     (or (char-alphabetic? character)
                         (char-numeric? character)
                         (char=? character #\_))))
            (loop (fx+ offset 1)) offset))))))

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
            (subscript-end text subscript-start body-end)
            parameter-end))
         (operator (and (< after-subscript body-end)
                        (parameter-operator text after-subscript body-end)))
         (operator-end (if operator
                         (+ after-subscript (string-length operator))
                         after-subscript))
         (open (slice-token raw text 'parameter-open start body-start)))
    (let-values (((operand-children operand-tokens)
                  (parse-parts raw text operator-end body-end #f)))
      (let-values (((subscript-node subscript-tokens)
                    (if subscript-start
                      (let* ((inner-start (fx+ subscript-start 1))
                             (inner-end (fx- after-subscript 1))
                             (open-bracket
                              (slice-token raw text 'subscript-open
                                           subscript-start inner-start))
                             (close-bracket
                              (slice-token raw text 'subscript-close
                                           inner-end after-subscript)))
                        (let-values (((parts tokens)
                                      (parse-parts raw text inner-start
                                                   inner-end #f)))
                          (values
                           (node raw text 'ArraySubscript
                                 subscript-start after-subscript
                                 (append
                                  (list (child 'open open-bracket))
                                  (map (lambda (part) (child 'index part))
                                       parts)
                                  (list (child 'close close-bracket))))
                           (append (list open-bracket) tokens
                                   (list close-bracket)))))
                      (values #f '()))))
        (let* ((prefix-token
                (and prefix?
                     (slice-token raw text 'parameter-prefix
                                  body-start name-start)))
               (name-token
                (and (> parameter-end name-start)
                   (slice-token raw text 'parameter-name
                                name-start parameter-end)))
             (operator-token
              (and operator
                   (slice-token raw text 'parameter-operator
                                after-subscript operator-end)))
             (close (slice-token raw text 'parameter-close body-end end))
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
              (append (list open)
                      (if prefix-token (list prefix-token) '())
                      (if name-token (list name-token) '())
                      subscript-tokens
                      (if operator-token (list operator-token) '())
                      operand-tokens (list close))))
        (values (node raw text 'ParameterExpansion start end children)
                tokens))))))

(def (parse-quoted raw text start end kind opening-length)
  (let* ((body-start (+ start opening-length))
         (body-end (fx- end 1))
         (open (slice-token raw text 'quote-open start body-start)))
    (let-values (((parts interior)
                  (parse-parts raw text body-start body-end kind)))
      (let (close (slice-token raw text 'quote-close body-end end))
        (values
         (node raw text kind start end
               (append (list (child 'open open))
                       (map (lambda (part) (child 'part part)) parts)
                       (list (child 'close close))))
         (append (list open) interior (list close)))))))

(def (parse-opaque-substitution raw text start end kind opening-length
                                closing-length)
  (let* ((body-start (+ start opening-length))
         (body-end (- end closing-length))
         (open (slice-token raw text 'substitution-open start body-start)))
    (let* ((body (and (> body-end body-start)
                      (slice-token raw text 'substitution-body
                                   body-start body-end)))
           (close (slice-token raw text 'substitution-close body-end end))
           (tokens (append (list open) (if body (list body) '())
                           (list close)))
           (children
            (append (list (child 'open open))
                    (if body (list (child 'body body)) '())
                    (list (child 'close close)))))
      (values (node raw text kind start end children) tokens))))

(def (special-start? text offset end context)
  (and (< offset end)
       (let (character (string-ref text offset))
         (or (and (not (eq? context 'SingleQuoted))
                  (char=? character #\\)
                  (or (not (eq? context 'HereDocument))
                      (and (< (fx+ offset 1) end)
                           (memv (string-ref text (fx+ offset 1))
                                 '(#\$ #\` #\\ #\newline)))))
             (and (not context)
                  (or (char=? character #\')
                      (char=? character #\")))
             (and (not (memq context '(SingleQuoted AnsiCString)))
                  (char=? character #\$)
                  (< (fx+ offset 1) end))
             (and (not (memq context '(SingleQuoted AnsiCString)))
                  (char=? character #\`))
             (and (not context)
                  (or (source-prefix-at? text offset "<(")
                      (source-prefix-at? text offset ">(")))))))

(def (literal-end text start end context)
  (let loop ((offset (fx+ start 1)))
    (if (or (= offset end) (special-start? text offset end context))
      offset (loop (fx+ offset 1)))))

;;; Returns syntax parts and their nonoverlapping source tokens in order.
(def (parse-parts raw text start end context)
  (let loop ((offset start) (parts '()) (tokens '()))
    (if (= offset end)
      (values (reverse parts) (reverse tokens))
      (let-values
          (((part produced next)
            (cond
             ((and (not context) (source-prefix-at? text offset "$'"))
              (let (after (region-plan-quote-end regions text (fx+ offset 1) #\'))
                (let-values (((part produced)
                              (parse-quoted raw text offset after
                                            'AnsiCString 2)))
                  (values part produced after))))
             ((and (not context)
                   (char=? (string-ref text offset) #\'))
              (let (after (region-plan-quote-end regions text offset #\'))
                (let-values (((part produced)
                              (parse-quoted raw text offset after
                                            'SingleQuoted 1)))
                  (values part produced after))))
             ((and (not context)
                   (char=? (string-ref text offset) #\"))
              (let (after (region-plan-quote-end regions text offset #\"))
                (let-values (((part produced)
                              (parse-quoted raw text offset after
                                            'DoubleQuoted 1)))
                  (values part produced after))))
             ((and (not (memq context '(SingleQuoted AnsiCString)))
                   (source-prefix-at? text offset "${"))
              (let (after (region-plan-pair-end regions text offset))
                (let-values (((part produced)
                              (parse-parameter raw text offset after)))
                  (values part produced after))))
             ((and (not (memq context '(SingleQuoted AnsiCString)))
                   (source-prefix-at? text offset "$(("))
              (let (after (region-plan-pair-end regions text offset))
                (let-values
                    (((part produced)
                      (parse-opaque-substitution
                       raw text offset after 'ArithmeticExpansion 3 2)))
                  (values part produced after))))
             ((and (not (memq context '(SingleQuoted AnsiCString)))
                   (or (source-prefix-at? text offset "$(")
                       (and (not context)
                            (or (source-prefix-at? text offset "<(")
                                (source-prefix-at? text offset ">(")))))
              (let (after (region-plan-pair-end regions text offset))
                (let-values
                    (((part produced)
                      (parse-opaque-substitution
                       raw text offset after
                       (if (char=? (string-ref text offset) #\$)
                         'CommandSubstitution 'ProcessSubstitution)
                       2 1)))
                  (values part produced after))))
             ((and (not (memq context '(SingleQuoted AnsiCString)))
                   (char=? (string-ref text offset) #\`))
              (let (after (region-plan-quote-end regions text offset #\`))
                (let-values
                    (((part produced)
                      (parse-opaque-substitution
                       raw text offset after 'CommandSubstitution 1 1)))
                  (values part produced after))))
             ((and (not (memq context '(SingleQuoted AnsiCString)))
                   (char=? (string-ref text offset) #\$)
                   (< (fx+ offset 1) end)
                   (> (dollar-name-end text (fx+ offset 1) end)
                      (fx+ offset 1)))
              (let (after (dollar-name-end text (fx+ offset 1) end))
                (let-values (((part produced)
                              (leaf raw text 'SimpleParameter offset after)))
                  (values part produced after))))
             ((and (not (eq? context 'SingleQuoted))
                   (char=? (string-ref text offset) #\\)
                   (or (not (eq? context 'HereDocument))
                       (and (< (fx+ offset 1) end)
                            (memv (string-ref text (fx+ offset 1))
                                  '(#\$ #\` #\\ #\newline)))))
              (let (after (min end (fx+ offset 2)))
                (let-values (((part produced)
                              (leaf raw text 'EscapeSequence offset after)))
                  (values part produced after))))
             (else
              (let (after (literal-end text offset end context))
                (let-values (((part produced)
                              (leaf raw text 'LiteralPart offset after)))
                  (values part produced after)))))))
        (loop next (cons part parts) (append (reverse produced) tokens))))))

(def (shell-word-components raw)
  (let* ((text (token-lexeme raw))
         (length (string-length text)))
    (let-values (((parts tokens) (parse-parts raw text 0 length #f)))
      (values
       (make-recognition-node
        'Word (token-start raw) (token-end raw)
       (map (lambda (part) (child 'part part)) parts))
       tokens))))

;;; Unquoted here-document bodies expand parameters, commands, and arithmetic.
;;; Quote characters remain literal in this context.
(def (shell-here-content-components raw)
  (let* ((text (token-lexeme raw))
         (length (string-length text)))
    (let-values (((parts tokens)
                  (parse-parts raw text 0 length 'HereDocument)))
      (values
       (make-recognition-node
        'HereDocumentLine (token-start raw) (token-end raw)
        (map (lambda (part) (child 'part part)) parts))
       tokens))))

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
(def (shell-assignment-components raw)
  (let* ((text (token-lexeme raw))
         (length (string-length text))
         (name-end (assignment-name-end text)))
    (if (and name-end
             (or (source-prefix-at? text name-end "=")
                 (source-prefix-at? text name-end "+=")))
      (let* ((operator-end
              (if (source-prefix-at? text name-end "+=")
                (fx+ name-end 2) (fx+ name-end 1)))
             (name (slice-token raw text 'assignment-name 0 name-end))
             (operator
              (slice-token raw text 'assignment-operator
                           name-end operator-end)))
        (let-values (((parts tokens)
                      (parse-parts raw text operator-end length #f)))
          (values
           (node raw text 'Assignment 0 length
                 (append (list (child 'name name)
                               (child 'operator operator))
                         (map (lambda (part) (child 'value part)) parts)))
           (append (list name operator) tokens))))
      (values #f #f))))

  (values shell-word-components shell-assignment-components shell-here-content-components))

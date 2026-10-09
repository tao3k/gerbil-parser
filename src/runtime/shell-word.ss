;;; -*- Gerbil -*-
;;; Lossless word decomposition with explicit quote and expansion boundaries.

(import (only-in :gerbil-parser/src/runtime/recognition
                 prepare-recognition-source)
        (only-in ../language/part-profile part-plan-recipe part-plan-context part-context-match part-context-literal-end part-plan-literal
                 part-step-operation part-step-kind part-step-opening part-step-closing part-step-delimiter part-plan-name-end part-plan-binding part-step-binding
                 binding-plan-name-end binding-plan-prefix-end binding-plan-operator-end binding-plan-subscript-at? binding-plan-subscript-width binding-plan-region-plan binding-plan-region-scopes)
        (only-in ../language/result-profile result-plan-projection result-projection-build)
        (only-in :gerbil-parser/src/runtime/funcs
                 recognition-sequence-append recognition-sequence->list)
        (only-in :gerbil-parser/src/runtime/token token-lexeme)
        (only-in :gerbil-parser/src/runtime/region-scanner
                 prepare-region-source prepare-scoped-region-source region-source-quote-end region-source-pair-end))
(export make-shell-word-parser)

(defstruct shell-word-source (spans text regions subscripts))

(def (make-shell-word-parser regions results parts-plan)
(def literal-kind (part-plan-literal parts-plan))
(def word-context (part-plan-context parts-plan 'word))
(def here-context (part-plan-context parts-plan 'HereDocument))
(def assignment-plan (part-plan-binding parts-plan 'assignment))
(def parameter-plan (part-plan-binding parts-plan 'parameter))
(def subscript-regions (and parameter-plan (binding-plan-region-plan parameter-plan regions)))
(def subscript-scopes (and parameter-plan (binding-plan-region-scopes parameter-plan regions)))
(def (prepare-word token)
  ;; Scanner-created token lexemes are read-only throughout this engine call.
  ;; Result publication owns its admitted copy; region preparation is needed
  ;; only when an actual quote or substitution is encountered.
  (let* ((spans (prepare-recognition-source token)) (text (token-lexeme token)))
    (values (make-shell-word-source spans text #f #f) text)))

(def (word-regions source)
  (or (shell-word-source-regions source)
      (let (context (prepare-region-source regions (shell-word-source-text source)))
        (shell-word-source-regions-set! source context)
        context)))

;;; Bind projection signatures once. The grammar owns node/field/token names;
;;; this executor owns only typed recognition captures and character boundaries.
(def projections (make-hash-table-eq))
(def (bind! id signature)
  (hash-put! projections id (result-plan-projection results id signature)))
(bind! 'Word '((parts parts)))
(bind! 'HereDocumentLine '((parts parts)))
(when assignment-plan (bind! 'Assignment '((name span required) (operator span required) (parts parts))))
(bind! literal-kind '((text span required)))
(for-each (lambda (row)
  (let* ((form (caddr row)) (operation (car form))
         (kind (if (eq? operation 'parameter) 'ParameterExpansion (cadr form))))
    (case operation
      ((name escape) (bind! kind '((text span required))))
      ((quote) (bind! kind '((open span required) (parts parts) (close span required))))
      ((pair quoted-body) (bind! kind '((open span required) (body span optional) (close span required))))
      ((parameter)
       (bind! kind '((open span required) (prefix span optional) (name span optional) (subscript node optional) (operator span optional) (parts parts) (close span required)))
       (when subscript-regions (bind! 'ArraySubscript '((open span required) (parts parts) (close span required))))))))
  (cadr (part-plan-recipe parts-plan)))
(def (project raw id start end captures)
  (result-projection-build (hash-get projections id) (shell-word-source-spans raw) start end captures))
(def (leaf raw kind start end)
  (project raw kind start end (vector (cons start end))))

(def (word-subscript-regions source)
  (or (shell-word-source-subscripts source)
      (let (context (prepare-scoped-region-source subscript-regions (shell-word-source-text source) subscript-scopes))
        (shell-word-source-subscripts-set! source context) context)))

(def (parse-parameter raw text start end step)
  (let* ((binding (part-step-binding step))
         (body-start (+ start (part-step-opening step)))
         (body-end (- end (part-step-closing step)))
         (prefix-end (binding-plan-prefix-end binding text body-start body-end))
         (name-start (or prefix-end body-start))
         (parameter-end (binding-plan-name-end binding text name-start body-end))
         (subscript-start (and (< parameter-end body-end)
                              (binding-plan-subscript-at? binding text parameter-end body-end) parameter-end))
         (after-subscript (if subscript-start
                           (region-source-pair-end (word-subscript-regions raw) subscript-start) parameter-end))
         (operator-end (or (binding-plan-operator-end binding text after-subscript body-end) after-subscript)))
    (when (> after-subscript body-end) (error "binding subscript crosses parameter boundary" subscript-start))
    (let-values (((operand-children operand-tokens) (parse-parts raw text operator-end body-end #f)))
      (let (subscript
             (and subscript-start
               (let ((inner-start (+ subscript-start (binding-plan-subscript-width binding)))
                     (inner-end (- after-subscript 1)))
                 (let-values (((parts tokens) (parse-parts raw text inner-start inner-end #f)))
                   (let-values (((value produced)
                     (project raw 'ArraySubscript subscript-start after-subscript
                       (vector (cons subscript-start inner-start) (cons parts tokens) (cons inner-end after-subscript)))))
                     (cons value produced))))))
        (project raw (part-step-kind step) start end
          (vector (cons start body-start)
                  (and prefix-end (cons body-start name-start))
                  (and (> parameter-end name-start) (cons name-start parameter-end))
                  subscript
                  (and (> operator-end after-subscript) (cons after-subscript operator-end))
                  (cons operand-children operand-tokens) (cons body-end end)))))))

(def (parse-quoted raw text start end kind opening-length closing-length)
  (let ((body-start (+ start opening-length)) (body-end (- end closing-length)))
    (let-values (((parts interior) (parse-parts raw text body-start body-end kind)))
      (project raw kind start end
        (vector (cons start body-start) (cons parts interior) (cons body-end end))))))

(def (parse-opaque-substitution raw text start end kind opening-length closing-length)
  (let ((body-start (+ start opening-length)) (body-end (- end closing-length)))
    (project raw kind start end
      (vector (cons start body-start) (and (> body-end body-start) (cons body-start body-end)) (cons body-end end)))))

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
        ;; This spine is private until return; token branches and nested
        ;; result nodes are shared values and are never reversed.
        (values (reverse! parts) tokens)
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
                                    (parse-quoted raw text offset after (part-step-kind step) (part-step-opening step) (part-step-closing step))
                                    (parse-opaque-substitution raw text offset after (part-step-kind step)
                                     (part-step-opening step) (part-step-closing step)))))
                      (values part produced after))))
                 ((memq operation '(parameter pair))
                  (let (after (region-source-pair-end (word-regions raw) offset))
                    (let-values (((part produced)
                                  (if (eq? operation 'parameter)
                                    (parse-parameter raw text offset after step)
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

(def (components token context id)
  (let-values (((raw text) (prepare-word token)))
    (let (length (string-length text))
      (let-values (((parts tokens) (parse-parts raw text 0 length context)))
        (let-values (((value produced) (project raw id 0 length (vector (cons parts tokens)))))
          (values value (recognition-sequence->list produced)))))))
(def (shell-word-components token) (components token #f 'Word))
(def (shell-here-content-components token) (components token 'HereDocument 'HereDocumentLine))

;;; An assignment is recognized only at a command position by the caller.
;;; It returns #f for ordinary words, or a node and ordered source tokens.
(def (shell-assignment-components token)
  (let* ((text (token-lexeme token))
         (length (string-length text))
         (name-end (and assignment-plan (binding-plan-name-end assignment-plan text 0 length)))
         (operator-end (and name-end (> name-end 0)
                            (binding-plan-operator-end assignment-plan text name-end length))))
    (if operator-end
      (let-values (((raw text) (prepare-word token)))
        (let-values (((parts tokens) (parse-parts raw text operator-end length #f)))
          (let-values (((value produced)
                        (project raw 'Assignment 0 length
                          (vector (cons 0 name-end) (cons name-end operator-end) (cons parts tokens)))))
            (values value (recognition-sequence->list produced)))))
      (values #f #f))))

  (values shell-word-components shell-assignment-components shell-here-content-components))

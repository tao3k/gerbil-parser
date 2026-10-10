;;; Source-owned former layout lookup and projection primitive.
(import (only-in :gerbil-parser/src/runtime/lr-action-index
                 lr-action-row-eof lr-action-row-tokens lookup-action-entry
                 lookup-literal-action-entry lookup-casefolded-literal-action-entry
                 lookup-layout-next-action-entry lookup-layout-start-action-entry)
        (only-in :gerbil-parser/src/runtime/layout
                 current-layout-columns layout-marker-eligible? layout-shift-allowed?)
        (only-in :gerbil-parser/src/runtime/token token-kind token-lexeme))
(export reference-lr-action-selector reference-layout-current-action-row)
(def (reference-layout-ordinary-entry entry shift-allowed?)
  (if (or (not entry) shift-allowed?)
    (if (and entry (eq? (cadr entry) 'layout-guard))
      (cons (car entry) (caddr entry)) entry)
    (let (action (cdr entry))
      (case (car action)
        ((layout-guard) (cons (car entry) (caddr action)))
        ((shift) #f)
        ((fork)
         (let (admitted
               (filter (lambda (branch) (not (eq? (car branch) 'shift)))
                       (cdr action)))
           (cond ((null? admitted) #f)
                 ((null? (cdr admitted))
                  (cons (car entry) (car admitted)))
                 (else (cons (car entry) (cons 'fork admitted))))))
        (else entry)))))

(def (reference-layout-mark-action action role)
  (case (car action)
    ((layout-guard) (reference-layout-mark-action (cadr action) role))
    ((shift) (list 'layout-shift role (cadr action)))
    ((fork)
     (cons 'fork
           (map (lambda (branch) (reference-layout-mark-action branch role))
                (cdr action))))
    (else action)))

(def (reference-layout-mark-entry entry role)
  (and entry (cons (car entry)
                   (reference-layout-mark-action (cdr entry) role))))

(def (reference-layout-current-action-row row token case-insensitive?)
  (let* ((lexeme (token-lexeme token))
         (next-entry (lookup-layout-next-action-entry row lexeme))
         (next (and next-entry (layout-marker-eligible? 'layout-next token)
                    (reference-layout-mark-entry next-entry 'layout-next)))
         (start-entry (and (not next) (lookup-layout-start-action-entry row lexeme)))
         (start (and start-entry (layout-marker-eligible? 'layout-start token)
                     (reference-layout-mark-entry start-entry 'layout-start)))
         (ordinary
          (reference-layout-ordinary-entry
           (or (lookup-literal-action-entry row lexeme)
               (and case-insensitive?
                    (lookup-casefolded-literal-action-entry row lexeme))
               (lookup-action-entry (lr-action-row-tokens row) (token-kind token)))
           (layout-shift-allowed? token))))
    (or next start ordinary
        (reference-layout-ordinary-entry next-entry #f)
        (reference-layout-ordinary-entry start-entry #f))))

(def (reference-lr-action-selector _rows index case-insensitive? layout?)
  (lambda (state tokens)
    (let (row (vector-ref index state))
      (if (null? tokens) (lr-action-row-eof row)
        (let (token (car tokens))
          (if (and layout? (current-layout-columns))
            (reference-layout-current-action-row row token case-insensitive?)
            (or (lookup-literal-action-entry row (token-lexeme token))
                (and case-insensitive? (lookup-casefolded-literal-action-entry row (token-lexeme token)))
                (lookup-action-entry (lr-action-row-tokens row) (token-kind token)))))))))

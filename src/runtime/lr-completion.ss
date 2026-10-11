;;; -*- Gerbil -*-
;;; Private completion values and pure selective-GLR ranking policy.

(export make-candidate candidate-root candidate-rest candidate-score
        candidate-ambiguities candidate-winner-reason candidate-completion-count
        select-candidate)

;;; The retained token suffix is immutable. Cache its length at completion,
;;; rather than traverse it again at every comparison in an enclosing fork.
(defstruct lr-completion
  (root rest score ambiguities winner-reason completion-count remaining-count)
  final: #t)

(def (make-candidate root rest score (ambiguities 0)
                     (winner-reason 'unique-completion) (completion-count 1))
  (make-lr-completion root rest score ambiguities winner-reason completion-count
                      (length rest)))

(def candidate-root lr-completion-root)
(def candidate-rest lr-completion-rest)
(def candidate-score lr-completion-score)
(def candidate-ambiguities lr-completion-ambiguities)
(def candidate-winner-reason lr-completion-winner-reason)
(def candidate-completion-count lr-completion-completion-count)

(def (ranked-candidate winner reason count)
  (using (winner : lr-completion)
    (make-lr-completion winner.root winner.rest winner.score winner.ambiguities
                        reason count winner.remaining-count)))

;;; Return the chosen representative and whether equal completed products
;;; merged. Request-local counters belong to the executor, not this policy.
(def (select-candidate current candidate)
  (if (not current)
    (values candidate #f)
    (using (current : lr-completion)
      (using (candidate : lr-completion)
        (let (count (+ current.completion-count candidate.completion-count))
          (cond
           ((> candidate.score current.score)
            (values (ranked-candidate candidate 'dynamic-precedence count) #f))
           ((< candidate.score current.score)
            (values (ranked-candidate current 'dynamic-precedence count) #f))
           ((< candidate.remaining-count current.remaining-count)
            (values (ranked-candidate candidate 'maximal-consumption count) #f))
           ((> candidate.remaining-count current.remaining-count)
            (values (ranked-candidate current 'maximal-consumption count) #f))
           (else
            (let* ((equivalent? (and (equal? candidate.root current.root)
                                    (equal? candidate.rest current.rest)))
                   (ambiguities (+ current.ambiguities candidate.ambiguities
                                   (if equivalent? 0 1))))
              ;; Keep an ambiguous representative: an enclosing dynamic fork
              ;; may outrank the whole set with another completed branch.
              (values
               (make-lr-completion current.root current.rest current.score ambiguities
                                   (if (zero? ambiguities) 'equivalent-merge 'ambiguous)
                                   count current.remaining-count)
               equivalent?)))))))))

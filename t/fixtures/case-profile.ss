;;; Native gxtest module Fixture. Resource policy stays in Core's Case object.
(import (only-in :clan/poo/object .o)
        (only-in :yuanxu/observability/testing-case
                 yuanxu-current-testing-case-profile))
(export test-setup! test-cleanup!)

(def previous-profile #f)
(def (test-setup!)
  (when previous-profile (error "Case profile Fixture is already active"))
  (let (previous (yuanxu-current-testing-case-profile))
    ;; Same three-GiB shared-process ceiling as the former source qualification.
    ;; Duration, growth, sampling, cancellation and failure policy remain Core's.
    (let (selected (.o (:: @ previous) (heap-limit-bytes (* 3 1024 1024 1024))))
      (set! previous-profile previous)
      (yuanxu-current-testing-case-profile selected))))
(def (test-cleanup!)
  (when previous-profile
    (yuanxu-current-testing-case-profile previous-profile)
    (set! previous-profile #f)))

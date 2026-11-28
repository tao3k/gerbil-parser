prelude: :gerbil/core
;;; On-demand language catalog and its public ModuleSource.
(import (only-in :tao3k/yuanxu/module-system/selection/init-syntax yuanxu!))

(yuanxu!
  :load-modules "." mode: auto mount: Languages export: LanguagesSource)

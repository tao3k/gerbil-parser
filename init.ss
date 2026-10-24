;;; One native selection owns the Parser library publication and its build projection.
(import :yuanxu/module-system/selection/init-syntax)
(yuanxu! :load-modules "./modules" mode: explicit (parser))

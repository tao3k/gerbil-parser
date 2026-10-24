;;; The publication must carry the existing complete source catalog.
(import (only-in :clan/poo/object object? .slot? .ref)
        (only-in :yuanxu/object-types/contracts yuanxu-predicate-contract))
(export ParserScopeContract)
(def ParserScopeContract
  (yuanxu-predicate-contract 'gerbil-parser/scope
    (lambda (value)
      (and (object? value) (.slot? value 'catalog)
           (list? (.ref value 'catalog)) (pair? (.ref value 'catalog))
           (andmap string? (.ref value 'catalog))))
    (lambda (_value _context) '())))

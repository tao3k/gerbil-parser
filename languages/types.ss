;;; A Module publishes the already validated production language loader.
(import (only-in :clan/poo/object .cc)
        (only-in :clan/poo/type define-type Object Record))
(export LanguageModuleScopeContract)

(define-type LanguageModuleScopeContract
  (.cc (Record loader: [Object]) identity: 'language-module/scope))

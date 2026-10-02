;;; -*- Gerbil -*-
;;; POO authoring objects for contextual method declarations.

(import (only-in :clan/poo/object .o .ref)
        (only-in :clan/poo/mop .defgeneric element? validate)
        (only-in ./contextual-types
                 +contextual-role-kind+ ContextualRoleContract))
(export ContextualRole.
        make-contextual-role contextual-role? contextual-role-ref
        make-contextual-method contextual-method?
        contextual-method-name contextual-method-mode
        contextual-method-position contextual-method-form
        contextual-method-result)

;;; Method payloads are closed IR identities. =any= is resolved only by the
;;; compiler, after it checks the full axis catalogs.
(defstruct contextual-method (name mode position form result) transparent: #t)

(def ContextualRoleContract. (.ref ContextualRoleContract 'proto))

(.defgeneric (contextual-role-ref role field) slot: .section)

(def ContextualRole.
  (.o (:: self ContextualRoleContract.)
      (.section (lambda (field) (.ref self field)))))

(def (unique-method-names? methods)
  (let loop ((rest methods) (seen '()))
    (or (null? rest)
        (and (not (memq (contextual-method-name (car rest)) seen))
             (loop (cdr rest)
                   (cons (contextual-method-name (car rest)) seen))))))

(def (contextual-method-valid? method)
  (and (contextual-method? method)
       (symbol? (contextual-method-name method))
       (symbol? (contextual-method-mode method))
       (symbol? (contextual-method-position method))
       (symbol? (contextual-method-form method))
       (symbol? (contextual-method-result method))))

(def (make-contextual-role name-value methods-value)
  (unless (and (symbol? name-value) (list? methods-value)
               (andmap contextual-method-valid? methods-value)
               (unique-method-names? methods-value))
    (error "invalid contextual role" name-value methods-value))
  (validate
   ContextualRoleContract
   (.o (:: @ ContextualRole.)
       kind: +contextual-role-kind+
       name: name-value
       methods: methods-value)))

(def (contextual-role? value)
  (element? ContextualRoleContract value))

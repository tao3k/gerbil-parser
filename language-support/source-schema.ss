;;; -*- Gerbil -*-
;;; Declarative source receipt templates; configuration is a value or native POO.
(import (only-in :std/list/list delete-duplicates/hash)
        (only-in :clan/poo/object object? .ref .slot?))
(export source-schema? source-schema-receipt)

(def (source-schema-ref value key)
  (if (object? value)
    (and (.slot? value key) (.ref value key))
    (let (row (assq key value)) (and row (cdr row)))))

(def (nonempty-string? value)
  (and (string? value) (positive? (string-length value))))

(def (source-schema? value)
  (and (or (object? value)
           (and (list? value)
                (every (lambda (row)
                         (and (pair? row) (memq (car row)
                           '(schema namespace sourceVersion upstreamCommit)))) value)
                (= (length value) 4)
                (= (length (delete-duplicates/hash (map car value))) 4)))
       (nonempty-string? (source-schema-ref value 'schema))
       (symbol? (source-schema-ref value 'namespace))
       (nonempty-string? (source-schema-ref value 'sourceVersion))
       (nonempty-string? (source-schema-ref value 'upstreamCommit))))

;;; Only name and optional normalization/disambiguation vary per receipt.
;;; Published records remain ordinary values, independent of POO construction.
(def (source-schema-receipt configuration provenance)
  (unless (source-schema? configuration)
    (error "source schema requires schema, namespace, sourceVersion and upstreamCommit" configuration))
  (let* ((qualified? (and (list? provenance) (= (length provenance) 2)
                         (memq (car provenance) '(normalization disambiguation))
                         (symbol? (cadr provenance))))
         (name (if qualified? (cadr provenance) provenance)))
    (unless (symbol? name)
      (error "source receipt requires an identifier or typed normalization/disambiguation identifier" provenance))
    (append
     (list (cons 'schema (source-schema-ref configuration 'schema))
           (cons 'namespace (source-schema-ref configuration 'namespace))
           (cons 'name name))
     (if qualified? (list (cons 'kind (car provenance))) '())
     (list (cons 'sourceVersion (source-schema-ref configuration 'sourceVersion))
           (cons 'upstreamCommit (source-schema-ref configuration 'upstreamCommit))))))

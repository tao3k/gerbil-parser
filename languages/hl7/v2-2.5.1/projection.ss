;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import (only-in :gerbil-parser/src/runtime/artifact
                 parse-artifact-ref parse-artifact-roundtrip
                 parse-artifact-success? parse-artifact-valid?))
(export hl7v2-adt-a08-patient-projection)

(def (split-character value delimiter)
  (let (length (string-length value))
    (let loop ((start 0) (offset 0) (parts '()))
      (cond
       ((= offset length)
        (reverse (cons (substring value start offset) parts)))
       ((char=? (string-ref value offset) delimiter)
        (loop (+ offset 1) (+ offset 1)
              (cons (substring value start offset) parts)))
       (else (loop start (+ offset 1) parts))))))

(def (message-lines source)
  (filter (lambda (line) (> (string-length line) 0))
          (split-character
           (list->string
            (map (lambda (character)
                   (if (char=? character #\newline) #\return character))
                 (string->list source)))
           #\return)))

(def (required-index values index owner)
  (if (< index (length values))
    (list-ref values index)
    (error "missing required HL7v2 field" owner index)))

(def (line-with-prefix lines prefix)
  (let loop ((rest lines))
    (cond
     ((null? rest) #f)
     ((and (>= (string-length (car rest)) 4)
           (string=? (substring (car rest) 0 4) prefix))
      (car rest))
     (else (loop (cdr rest))))))

(def (hl7v2-adt-a08-patient-projection artifact)
  (unless (and (parse-artifact-valid? artifact)
               (parse-artifact-success? artifact))
    (error "HL7v2 projection requires an accepted ParseArtifact"))
  (let* ((source (parse-artifact-roundtrip artifact))
         (lines (message-lines source))
         (msh-line (required-index lines 0 'MSH))
         (field-separator (string-ref msh-line 3))
         (msh (split-character msh-line field-separator))
         (encoding-characters (required-index msh 1 'MSH-2))
         (component-separator (string-ref encoding-characters 0))
         (subcomponent-separator (string-ref encoding-characters 3))
         (pid-line
          (line-with-prefix lines
                            (string-append "PID" (string field-separator)))))
    (unless (and (string=? (required-index msh 0 'MSH) "MSH")
                 (= (string-length encoding-characters) 4)
                 (equal? (split-character
                          (required-index msh 8 'MSH-9)
                          component-separator)
                         '("ADT" "A08"))
                 (string=? (required-index msh 11 'MSH-12) "2.5.1")
                 pid-line)
      (error "HL7v2 artifact is not an ADT^A08 2.5.1 patient message"))
    (let* ((pid (split-character pid-line field-separator))
           (cx (split-character (required-index pid 3 'PID-3)
                                component-separator))
           (assigning-authority
            (split-character (required-index cx 3 'PID-3.4)
                             subcomponent-separator))
           (name (split-character (required-index pid 5 'PID-5)
                                  component-separator)))
      (list
       (cons 'schema "gerbil-parser.hl7v2-adt-a08-patient.v1")
       (cons 'sourceDigest (parse-artifact-ref artifact 'sourceDigest))
       (cons 'grammarDigest (parse-artifact-ref artifact 'grammarDigest))
       (cons 'sourceInterface 'hl7v2)
       (cons 'sourceMessageType "ADT^A08")
       (cons 'sourceVersion "2.5.1")
       (cons 'identifierSystem
             (string-append "urn:oid:"
                            (required-index assigning-authority 1 'PID-3.4.2)))
       (cons 'identifierValue (required-index cx 0 'PID-3.1))
       (cons 'familyName (required-index name 0 'PID-5.1))
       (cons 'givenNames (list (required-index name 1 'PID-5.2)))))))

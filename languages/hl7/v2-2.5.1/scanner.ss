;;; -*- Gerbil -*-
;;; SPDX-FileCopyrightText: 2026 tao3k team and Contributors
;;;
;;; SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later

(import (only-in :std/misc/list delete-duplicates/hash))

(export scan-hl7v2-segment-id
        scan-hl7v2-data
        scan-hl7v2-segment-terminator
        scan-hl7v2-field-separator
        scan-hl7v2-component-separator
        scan-hl7v2-repetition-separator
        scan-hl7v2-escape-character
        scan-hl7v2-subcomponent-separator)

(def (scan-while source start predicate)
  (let (length (string-length source))
    (let loop ((offset start))
      (if (and (< offset length) (predicate (string-ref source offset)))
        (loop (+ offset 1))
        (and (> offset start) offset)))))

(def (hl7v2-segment-id-character? character)
  (or (and (char>=? character #\A) (char<=? character #\Z))
      (char-numeric? character)))

(def (scan-hl7v2-segment-id source start)
  (let (end (scan-while source start hl7v2-segment-id-character?))
    (and end (= (- end start) 3) end)))

(def (hl7v2-delimiters source)
  (and (>= (string-length source) 8)
       (string=? (substring source 0 3) "MSH")
       (let (delimiters
             (list (string-ref source 3) (string-ref source 4)
                   (string-ref source 5) (string-ref source 6)
                   (string-ref source 7)))
         (and (= (length (delete-duplicates/hash delimiters)) 5)
              (not (ormap (lambda (character)
                            (or (char-alphabetic? character)
                                (char-numeric? character)))
                          delimiters))
              delimiters))))

(def (scan-hl7v2-delimiter source start index)
  (let (delimiters (hl7v2-delimiters source))
    (and delimiters
         (< start (string-length source))
         (char=? (string-ref source start) (list-ref delimiters index))
         (+ start 1))))

(def (scan-hl7v2-field-separator source start)
  (scan-hl7v2-delimiter source start 0))

(def (scan-hl7v2-component-separator source start)
  (scan-hl7v2-delimiter source start 1))

(def (scan-hl7v2-repetition-separator source start)
  (scan-hl7v2-delimiter source start 2))

(def (scan-hl7v2-escape-character source start)
  (scan-hl7v2-delimiter source start 3))

(def (scan-hl7v2-subcomponent-separator source start)
  (scan-hl7v2-delimiter source start 4))

(def (scan-hl7v2-data source start)
  (let (delimiters (hl7v2-delimiters source))
    (and delimiters
         (scan-while
          source start
          (lambda (character)
            (and (not (memv character delimiters))
                 (not (char=? character #\return))
                 (not (char=? character #\newline))))))))

(def (scan-hl7v2-segment-terminator source start)
  (let (length (string-length source))
    (and (< start length)
         (case (string-ref source start)
           ((#\return)
            (if (and (< (+ start 1) length)
                     (char=? (string-ref source (+ start 1)) #\newline))
              (+ start 2)
              (+ start 1)))
           ((#\newline) (+ start 1))
           (else #f)))))

;;; -*- Gerbil -*-
;;; Scan comment owner: original pure lexical algorithms.
(import (only-in "./basic.ss" scan-while identifier-start? identifier-rest? newline? line-end skip-horizontal literal-at?)
        (only-in "./literal.ss" scan-longest-literal))
(export scan-heredoc scan-line-comment scan-block-comment scan-nested-block-comment)

;; scan-heredoc
;;   : (-> String Fixnum Fixnum)
;;   | doc m%
;;       `scan-heredoc` scans a marker-delimited multiline literal.
;;
;;       # Examples
;;
;;       ```scheme
;;       (scan-heredoc "<<EOF\nvalue\nEOF" 0)
;;       ;; => 15
;;       ```
;;     %
(def (scan-heredoc source start)
  (let (length (string-length source))
    (and (literal-at? source start "<<")
         (let* ((marker-start0 (+ start 2))
                (marker-start
                 (if (and (< marker-start0 length)
                          (char=? (string-ref source marker-start0) #\-))
                   (+ marker-start0 1)
                   marker-start0))
                (marker-end
                 (and (< marker-start length)
                      (identifier-start? (string-ref source marker-start))
                      (scan-while source marker-start identifier-rest?))))
           (and marker-end
                (< marker-end length)
                (newline? (string-ref source marker-end))
                (let ((marker (substring source marker-start marker-end))
                      (body-start (+ marker-end 1)))
                  (let loop ((line-start body-start))
                    (and (< line-start length)
                         (let* ((end (line-end source line-start))
                                (content-start
                                 (skip-horizontal source line-start end)))
                           (if (and (literal-at? source content-start marker)
                                    (= (+ content-start
                                          (string-length marker))
                                       end))
                             end
                             (and (< end length)
                                  (loop (+ end 1)))))))))))))

(def (scan-line-comment source start prefixes)
  (let (prefix (scan-longest-literal source start prefixes))
    (and prefix
         (scan-while source (+ start (string-length prefix))
                     (lambda (ch) (not (newline? ch)))))))

;; scan-block-comment
;;   : (-> String Fixnum String String Fixnum)
;;   | doc m%
;;       `scan-block-comment` scans one non-nested delimiter pair.
;;
;;       # Examples
;;
;;       ```scheme
;;       (scan-block-comment "/* value */" 0 "/*" "*/")
;;       ;; => 11
;;       ```
;;     %
(def (scan-block-comment source start opening closing)
  (let ((length (string-length source))
        (closing-length (string-length closing)))
    (and (literal-at? source start opening)
         (let loop ((offset (+ start (string-length opening))))
           (cond
            ((>= offset length) #f)
            ((literal-at? source offset closing)
             (+ offset closing-length))
            (else (loop (+ offset 1))))))))

;; scan-nested-block-comment
;;   : (-> String Fixnum String String Fixnum)
;;   | doc m%
;;       `scan-nested-block-comment` tracks balanced nested delimiters.
;;
;;       # Examples
;;
;;       ```scheme
;;       (scan-nested-block-comment "/* /* */ */" 0 "/*" "*/")
;;       ;; => 11
;;       ```
;;     %
(def (scan-nested-block-comment source start opening closing)
  (let ((length (string-length source))
        (opening-length (string-length opening))
        (closing-length (string-length closing)))
    (and (literal-at? source start opening)
         (let loop ((offset (+ start opening-length)) (depth 1))
           (cond
            ((>= offset length) #f)
            ((literal-at? source offset opening)
             (loop (+ offset opening-length) (+ depth 1)))
            ((literal-at? source offset closing)
             (let ((next (+ offset closing-length))
                   (remaining (- depth 1)))
               (if (zero? remaining) next (loop next remaining))))
            (else (loop (+ offset 1) depth)))))))


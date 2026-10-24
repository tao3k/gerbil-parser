;;; Shared content snapshots for build and conformance publication.
(import (only-in :std/crypto/digest Digest::sha256 digest-update! digest-final!))
(export preparation-files preparation-file-digest preparation-snapshot preparation-resolve preparation-native-object?)

(def (preparation-native-object? path library)
  ;; Static translations and object option records are derived outputs. Their
  ;; SCM source and toolchain/options owners remain admitted inputs.
  (and (string-prefix? (string-append (path-expand "static" library) "/") path)
       (or (string-suffix? ".c" path) (string-suffix? ".o" path)
           (string-suffix? ".o.options.sexp" path))))

(def (preparation-reference path)
  (let (prefix (path-directory (path-expand "receipt" (gerbil-path))))
    (if (string-prefix? prefix path)
      (string-append "gerbil-path:" (substring path (string-length prefix) (string-length path)))
      path)))
(def (preparation-resolve path)
  (if (string-prefix? "gerbil-path:" path)
    (path-expand (substring path 12 (string-length path)) (gerbil-path)) path))

(def (preparation-files root (admit? (lambda (_) #t)))
  ;; Preserve sorted depth-first identity with one accumulator. Report actual
  ;; discovered files, including the post-link admission walk.
  (let ((files '()) (count 0))
    (def (walk path)
      ;; Rejected directories are opaque ownership boundaries. Do not stat or
      ;; enumerate their children merely to discard the whole subtree later.
      (when (and (admit? path) (file-exists? path))
        (if (eq? (file-type path) 'directory)
          (for-each (lambda (name)
                      (unless (string-prefix? "." name)
                        (walk (path-expand name path))))
                    (list-sort string<? (directory-files path)))
          (begin
            (set! files (cons path files))
            (set! count (+ count 1))
            (when (zero? (modulo count 128))
              (displayln "PREPARATION-DISCOVERED files=" count " last=" path)
              (force-output))))))
    (walk root)
    (displayln "PREPARATION-DISCOVERED root=" root " files=" count) (force-output)
    (reverse files)))
(def (make-preparation-file-digester)
  ;; One synchronous owner reuses scratch bytes; each file still gets a fresh
  ;; digest state and is read in full. No digest survives between snapshots.
  (let (buffer (make-u8vector 65536))
    (lambda (path)
      (let (digest (Digest::sha256))
        (call-with-input-file path
          (lambda (port)
            (let loop ()
              (let (count (read-subu8vector buffer 0 (u8vector-length buffer) port))
                (unless (zero? count)
                  (digest-update! digest buffer 0 count)
                  (loop))))))
        (digest-final! digest)))))
(def (preparation-file-digest path)
  ((make-preparation-file-digester) path))
(def (preparation-snapshot files)
  (let ((count 0) (total (length files)) (digest-file (make-preparation-file-digester)))
    (displayln "PREPARATION-INVENTORY files=" total) (force-output)
    (map (lambda (path)
           (let (digest (digest-file path))
             (set! count (+ count 1))
             (when (or (zero? (modulo count 64)) (= count total))
               (displayln "PREPARATION-HASHED files=" count " last=" path) (force-output))
             (list (preparation-reference path) digest (file-info-mode (file-info path))))) files)))

;;; A fresh POSIX session contains the compiler/test child and its descendants.
(import :std/ffi)
(export main)
(C-ffi-macrology)
(C-include "<unistd.h>" "<fcntl.h>" "<sys/time.h>")
;;; These operations have one private consumer: this native entry boundary.
(def-C-lambda blocking-standard-streams! () int
 "int result = 0;
  for (int fd = 0; fd < 3; ++fd) {
    int flags = fcntl(fd, F_GETFL);
    if (flags >= 0 && fcntl(fd, F_SETFL, flags & ~O_NONBLOCK) < 0) result = -1;
  }
  ___result = result;")
(def-C-lambda start-session! () int "___result = setsid();")
;;; Stop inherited Scheme timers before replacing the image; never fork a waiter.
(def-C-lambda replace-process! (nonnull-UTF-8-string nonnull-UTF-8-string-list)
  int
  "struct itimerval stopped = {{0, 0}, {0, 0}};
   if (setitimer(ITIMER_REAL, &stopped, 0) < 0 ||
       setitimer(ITIMER_VIRTUAL, &stopped, 0) < 0 ||
       setitimer(ITIMER_PROF, &stopped, 0) < 0) ___result = -1;
   else ___result = execvp(___arg1, ___arg2);")
(def (main witness loadpath program . arguments)
  (let (owner (start-session!))
    (when (< (blocking-standard-streams!) 0) (error "cannot establish blocking compiler streams"))
    (when (< owner 0) (error "cannot establish owned process session"))
    (call-with-output-file witness (lambda (port) (write owner port))))
  (if (zero? (string-length loadpath)) (setenv "GERBIL_LOADPATH") (setenv "GERBIL_LOADPATH" loadpath))
  ;; Successful exec never returns. The parent observes the target's actual
  ;; wait status through its existing process port, including signal exits.
  (replace-process! program (cons program arguments))
  (error "cannot replace qualification process" program))

;;; Thin POSIX operations; Scheme owns lifecycle and qualification policy.
(import :std/ffi)
(export start-session! blocking-standard-streams! standard-streams-blocking?
        replace-process!)
(C-ffi-macrology)
(C-include "<unistd.h>" "<fcntl.h>" "<sys/time.h>")
(def-C-lambda blocking-standard-streams! () int
 "int result = 0;
  for (int fd = 0; fd < 3; ++fd) {
    int flags = fcntl(fd, F_GETFL);
    if (flags >= 0 && fcntl(fd, F_SETFL, flags & ~O_NONBLOCK) < 0) result = -1;
  }
  ___result = result;")
(def-C-lambda standard-streams-blocking? () bool
 "___result = !(fcntl(1, F_GETFL) & O_NONBLOCK) && !(fcntl(2, F_GETFL) & O_NONBLOCK);")
(def-C-lambda start-session! () int "___result = setsid();")

;;; Replace this GXI image; do not fork or keep a Scheme waiter around.
;;; Gambit owns UTF-8 argv conversion and its NULL-terminated array lifetime.
(def-C-lambda replace-process! (nonnull-UTF-8-string nonnull-UTF-8-string-list)
  int
  "struct itimerval stopped = {{0, 0}, {0, 0}};
   /* exec preserves interval timers, but resets the Runtime's handlers.
      Never deliver a previous Scheme scheduler tick to the native target. */
   if (setitimer(ITIMER_REAL, &stopped, 0) < 0 ||
       setitimer(ITIMER_VIRTUAL, &stopped, 0) < 0 ||
       setitimer(ITIMER_PROF, &stopped, 0) < 0) ___result = -1;
   else ___result = execvp(___arg1, ___arg2);")

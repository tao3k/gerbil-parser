;;; Thin POSIX operations; Scheme owns lifecycle and qualification policy.
(import :std/ffi)
(export start-session! blocking-standard-streams! standard-streams-blocking?)
(C-ffi-macrology)
(C-include "<unistd.h>" "<fcntl.h>")
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

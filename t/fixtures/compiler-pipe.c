#define _POSIX_C_SOURCE 200809L
/* External compilers require blocking stdout/stderr, including under fan-out. */
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <time.h>
int main(int argc, char **argv) {
  if ((fcntl(STDOUT_FILENO, F_GETFL) & O_NONBLOCK) ||
      (fcntl(STDERR_FILENO, F_GETFL) & O_NONBLOCK)) {
    fprintf(stderr, "compiler inherited a nonblocking output descriptor\n");
    return 42;
  }
  if (argc == 2 && !strcmp(argv[1], "--burst")) {
    char bytes[4096];
    memset(bytes, 'x', sizeof bytes);
    for (int i = 0; i < 64; ++i)
      if (fwrite(bytes, 1, sizeof bytes, stdout) != sizeof bytes) return 43;
    return fflush(stdout) ? 44 : 0;
  }
  if (argc == 2 && !strcmp(argv[1], "--fragment")) {
    fputs("fragment", stdout); fflush(stdout); fputs("diagnostic", stderr); return 0;
  }
  if (argc == 2 && !strcmp(argv[1], "--bytes")) {
    const unsigned char bytes[] = {0, 255, 195, 128, 10};
    return fwrite(bytes, 1, sizeof bytes, stdout) == sizeof bytes ? 0 : 45;
  }
  if (argc == 2 && !strcmp(argv[1], "--utf8-fragment")) {
    const struct timespec delay = {0, 20000000};
    if (write(STDOUT_FILENO, "\xce", 1) != 1) return 46;
    nanosleep(&delay, NULL);
    return write(STDERR_FILENO, "\xbb", 1) == 1 ? 0 : 47;
  }
  if (argc == 2 && !strcmp(argv[1], "--failure")) return 17;
  puts("COMPILER-PIPE-OK");

  return 0;
}

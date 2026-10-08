/* External compilers require blocking stdout/stderr, including under fan-out. */
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
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
  puts("COMPILER-PIPE-OK");

  return 0;
}

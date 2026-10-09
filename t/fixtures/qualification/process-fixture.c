/* Native child behavior; Scheme owns deadlines, receipts and cancellation. */
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

static void mark(const char *name) {
  FILE *file = fopen(name, "w");
  if (!file) exit(3);
  fputs("finished\n", file);
  fclose(file);
}

int main(int argc, char **argv) {
  if (argc < 2) return 2;
  if (strcmp(argv[1], "unrelated") &&
      ((fcntl(1, F_GETFL) & O_NONBLOCK) ||
       (fcntl(2, F_GETFL) & O_NONBLOCK))) return 43;
  if (!strcmp(argv[1], "identity")) {
    /* The target must replace the acknowledged session leader, not be its
       child. The previous waiting-GXI topology fails this assertion. */
    if (getpid() != getsid(0)) return 45;
    puts("NATIVE-EXEC-IDENTITY-OK");
    return 0;
  }
  if (!strcmp(argv[1], "arguments")) {
    if (argc != 3 || strcmp(argv[2], "λ a;$(not-a-command)")) return 46;
    puts("NATIVE-EXEC-ARGUMENTS-OK");
    return 0;
  }
  if (!strcmp(argv[1], "flood")) {
    if (argc != 3) return 2;
    mark(argv[2]);
    for (int i = 0; i < 2000; i++) {
      fputs("PIPE-PROGRESS ", stdout);
      for (int j = 0; j < 1000; j++) fputs("\xce\xbb", stdout);
      fputc('\n', stdout);
    }
    puts("OK");
    return fflush(stdout) ? 44 : 0;
  }
  if (!strcmp(argv[1], "early-error")) {
    puts("*** ERROR controlled failure");
    fflush(stdout);
    sleep(4);
    return 0;
  }
  if (!strcmp(argv[1], "signal")) { raise(SIGTERM); return 5; }
  if (!strcmp(argv[1], "silent")) { sleep(4); return 0; }
  if (!strcmp(argv[1], "continuous")) {
    for (int i = 0; i < 100; i++) {
      puts("CHILD-PROGRESS");
      fflush(stdout);
      usleep(20000);
    }
    return 0;
  }
  if (!strcmp(argv[1], "unrelated")) {
    if (argc != 3) return 2;
    sleep(2);
    mark(argv[2]);
    return 0;
  }
  if (!strcmp(argv[1], "escaped")) {
    if (argc != 3) return 2;
    pid_t child = fork();
    if (child < 0) return 3;
    if (!child) {
      if (setsid() < 0) return 4;
      printf("ESCAPED-SESSION %d\n", getpid());
      fflush(stdout);
      sleep(2);
      mark(argv[2]);
      _exit(0);
    }
    int result;
    return waitpid(child, &result, 0) < 0 ? 6 : 0;
  }
  if (!strcmp(argv[1], "closed-output")) {
    close(1);
    close(2);
    sleep(4);
    return 0;
  }
  if (!strcmp(argv[1], "failure")) { puts("OK"); return 42; }
  if (!strcmp(argv[1], "error")) { puts("ERROR swallowed"); return 0; }
  return 2;
}

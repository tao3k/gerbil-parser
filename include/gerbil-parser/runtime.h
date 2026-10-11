#ifndef GERBIL_PARSER_RUNTIME_H
#define GERBIL_PARSER_RUNTIME_H
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif
/* One standalone runtime bundle per process. Initialization chooses its owner
 * OS thread and is allowed once, before any other Gambit VM is initialized.
 * The bundle and all language modules remain loaded through shutdown.
 * 0 success; -1 wrong thread; -2 live handles/results; -3 lifecycle state;
 * -4 SDK setup failure (poisoned, no reinitialization).
 * Shutdown retains a ready runtime on -2, so resources can be released and
 * shutdown retried. Successful shutdown disables language callbacks and calls
 * the SDK's normal cleanup. Restart and duplicate shutdown return -3. */
int32_t gerbil_parser_runtime_init(void);
int32_t gerbil_parser_runtime_shutdown(void);
#ifdef __cplusplus
}
#endif
#endif

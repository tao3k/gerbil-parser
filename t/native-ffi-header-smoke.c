#include <gerbil-parser/runtime.h>
/*
 * SPDX-FileCopyrightText: 2026 tao3k team and Contributors
 * SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later
 */

#include <gerbil-parser/language.h>
#include <gerbil-parser/rust-runtime-aot.h>

int main(void) {
  gerbil_parser_runtime_result runtime_result;
  gerbil_parser_runtime_result_init(&runtime_result);
  (void)gerbil_parser_runtime_compile("grammar.ss", &runtime_result);
  gerbil_parser_runtime_result_release(&runtime_result);
  gerbil_parser_result language_result;
  gerbil_parser_result_init(&language_result);
  (void)gerbil_parser_language_descriptor(1, &language_result);
  (void)gerbil_parser_language_parse(1, (const uint8_t *)"a=1", 3, &language_result);
  (void)gerbil_parser_language_release(1);
  gerbil_parser_result_release(&language_result);
  return 0;
}

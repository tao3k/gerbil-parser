/*
 * SPDX-FileCopyrightText: 2026 tao3k team and Contributors
 * SPDX-License-Identifier: Apache-2.0 AND LGPL-2.1-or-later
 */

#include <gerbil-parser/parse-artifact-v1.h>
#include <gerbil-parser/language-v2.h>
#include <gerbil-parser/rust-rowan-aot-v1.h>

int main(void) {
  gerbil_parser_result_v1 parse_result;
  gerbil_parser_rowan_result_v1 rowan_result;
  gerbil_parser_result_v1_init(&parse_result);
  gerbil_parser_rowan_result_v1_init(&rowan_result);
  (void)gerbil_parser_native_descriptor("gql", &parse_result);
  (void)gerbil_parser_native_parse("gql", "RETURN 1", &parse_result);
  (void)gerbil_parser_rowan_compile("grammar.ss", &rowan_result);
  gerbil_parser_result_v1_release(&parse_result);
  gerbil_parser_rowan_result_v1_release(&rowan_result);
  gerbil_parser_result_v2 language_result;
  gerbil_parser_result_v2_init(&language_result);
  (void)gerbil_parser_language_descriptor(1, &language_result);
  (void)gerbil_parser_language_parse(1, (const uint8_t *)"a=1", 3, &language_result);
  (void)gerbil_parser_language_release(1);
  gerbil_parser_result_v2_release(&language_result);
  return 0;
}

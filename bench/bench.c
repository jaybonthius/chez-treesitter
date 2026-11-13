#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "tree_sitter/api.h"

const TSLanguage *tree_sitter_json(void);
const TSLanguage *tree_sitter_python(void);

static double now(void) {
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return t.tv_sec + t.tv_nsec / 1e9;
}

static char *read_file(const char *path, uint32_t *length) {
  FILE *f = fopen(path, "rb");
  if (!f) {
    perror(path);
    exit(1);
  }
  fseek(f, 0, SEEK_END);
  long size = ftell(f);
  fseek(f, 0, SEEK_SET);
  char *bytes = malloc(size + 1);
  if (fread(bytes, 1, size, f) != (size_t)size) {
    perror(path);
    exit(1);
  }
  fclose(f);
  bytes[size] = 0;
  *length = size;
  return bytes;
}

#define MEASURE(result, body) \
  do { \
    double start_ = now(); \
    long n_ = 0; \
    do { body; n_++; } while (now() - start_ < 1.0); \
    result = (now() - start_) / n_; \
  } while (0)

static TSPoint point_at(const char *bytes, uint32_t offset) {
  TSPoint p = {0, 0};
  for (uint32_t i = 0; i < offset; i++) {
    if (bytes[i] == '\n') {
      p.row++;
      p.column = 0;
    } else {
      p.column++;
    }
  }
  return p;
}

static long walk(TSTree *tree) {
  TSTreeCursor c = ts_tree_cursor_new(ts_tree_root_node(tree));
  long count = 1;
  for (;;) {
    if (ts_tree_cursor_goto_first_child(&c)) {
      count++;
      continue;
    }
    for (;;) {
      if (ts_tree_cursor_goto_next_sibling(&c)) {
        count++;
        break;
      }
      if (!ts_tree_cursor_goto_parent(&c)) {
        ts_tree_cursor_delete(&c);
        return count;
      }
    }
  }
}

static long captures(TSQuery *query, TSTree *tree) {
  TSQueryCursor *c = ts_query_cursor_new();
  ts_query_cursor_exec(c, query, ts_tree_root_node(tree));
  TSQueryMatch match;
  uint32_t index;
  long count = 0;
  while (ts_query_cursor_next_capture(c, &match, &index)) count++;
  ts_query_cursor_delete(c);
  return count;
}

int main(int argc, char **argv) {
  if (argc != 8) {
    fprintf(stderr, "usage: %s name json|python source offset a b query\n", argv[0]);
    return 2;
  }
  const char *name = argv[1];
  const TSLanguage *language = strcmp(argv[2], "json") == 0 ? tree_sitter_json() : tree_sitter_python();
  uint32_t length, query_length;
  char *source = read_file(argv[3], &length);
  uint32_t offset = atoi(argv[4]);
  char values[2] = {argv[5][0], argv[6][0]};
  char *query_source = read_file(argv[7], &query_length);

  TSParser *parser = ts_parser_new();
  ts_parser_set_language(parser, language);
  double seconds;

  MEASURE(seconds, ts_tree_delete(ts_parser_parse_string(parser, NULL, source, length)));
  printf("%s parse %.9f\n", name, seconds);

  TSPoint point = point_at(source, offset);
  TSPoint after = {point.row, point.column + 1};
  TSInputEdit edit = {offset, offset + 1, offset + 1, point, after, after};
  TSTree *tree = ts_parser_parse_string(parser, NULL, source, length);
  int which = 0;
  MEASURE(seconds, {
    which = !which;
    source[offset] = values[which];
    ts_tree_edit(tree, &edit);
    TSTree *next = ts_parser_parse_string(parser, tree, source, length);
    ts_tree_delete(tree);
    tree = next;
  });
  printf("%s reparse %.9f\n", name, seconds);

  MEASURE(seconds, {
    which = !which;
    source[offset] = values[which];
    TSTree *copy = ts_tree_copy(tree);
    ts_tree_edit(copy, &edit);
    TSTree *next = ts_parser_parse_string(parser, copy, source, length);
    ts_tree_delete(copy);
    ts_tree_delete(tree);
    tree = next;
  });
  printf("%s reparse-copy %.9f\n", name, seconds);

  long nodes = 0;
  MEASURE(seconds, nodes = walk(tree));
  printf("%s walk %.9f %ld\n", name, seconds, nodes);

  uint32_t error_offset;
  TSQueryError error_type;
  TSQuery *query = ts_query_new(language, query_source, query_length, &error_offset, &error_type);
  if (!query) {
    fprintf(stderr, "query error %d at %u\n", error_type, error_offset);
    return 1;
  }
  long count = 0;
  MEASURE(seconds, count = captures(query, tree));
  printf("%s captures %.9f %ld\n", name, seconds, count);

  ts_query_delete(query);
  ts_tree_delete(tree);
  ts_parser_delete(parser);
  free(source);
  free(query_source);
  return 0;
}

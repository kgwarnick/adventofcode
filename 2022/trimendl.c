#include <string.h>
#include <ctype.h>

void TrimLineEndings (int numlines, char **lines) {
  for (int i = 0; i < numlines; i++) {
    size_t vorher = strlen (lines[i]);
    char *c = strchr(lines[i], '\0');
    while (c > lines[i] && isspace (*(c-1))) { *(c-1) = '\0'; c--; }
  }
}

/// \file
/// \brief File reading

#include <stdio.h>
#include <stdlib.h>

ssize_t readlines (const char *filename, size_t *maxlines, size_t *numlines, char ***lines) {
  ssize_t charsread = 0;
  FILE *inputfile = fopen (filename, "rt");
  if (inputfile == NULL)  return -1;
  *numlines = 0;
  while (feof (inputfile) == 0) {
    // Let getline allocate the buffer and read the line
    size_t linelen = 0;
    char *s = NULL;
    ssize_t nread = getline (&s, &linelen, inputfile);
    // End of file or error? Then stop reading
    if (nread <= 0 || feof (inputfile)) { if (s != NULL) free (s); break; }
    // Increase the array size if necessary
    if (*numlines >= *maxlines) {
      char **newarr = reallocarray (*lines, *maxlines + 100, sizeof (char*));
      if (newarr == NULL)  return -1;
      *maxlines += 100;
      *lines = newarr;
    }
    // Store the line that was successfully read
    (*lines)[*numlines] = s;
    (*numlines)++;
    charsread += nread;
  }
  fclose (inputfile);
  return charsread;
}

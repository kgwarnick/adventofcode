// Advent of Code 2022 Day 8: Treetop Tree House
// https://adventofcode.com/2022/day/8

#include "freadlns.h"

#include <ctype.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>


const char* ExampleLines[] = {
  "30373",
  "25512",
  "65332",
  "33549",
  "35390"
};


bool VisibleFromLeft (int x, int y, int numx, int numy, const char **trees) {
  for (int i = 0; i < x; i++)
    if (trees[y][x] <= trees[y][i])  return false;
  return true;
}

bool VisibleFromRight (int x, int y, int numx, int numy, const char **trees) {
  for (int i = x + 1; i < numx; i++)
    if (trees[y][x] <= trees[y][i])  return false;
  return true;
}

bool VisibleFromTop (int x, int y, int numx, int numy, const char **trees) {
  for (int j = 0; j < y; j++)
    if (trees[y][x] <= trees[j][x])  return false;
  return true;
}

bool VisibleFromBottom (int x, int y, int numx, int numy, const char **trees) {
  for (int j = y + 1; j < numy; j++)
    if (trees[y][x] <= trees[j][x])  return false;
  return true;
}

bool VisibleFromAny (int x, int y, int numx, int numy, const char **trees) {
  return (VisibleFromLeft (x, y, numx, numy, trees) ||
          VisibleFromRight (x, y, numx, numy, trees) ||
          VisibleFromTop (x, y, numx, numy, trees) ||
          VisibleFromBottom (x, y, numx, numy, trees));
}

unsigned int CountVisibleTrees (int numx, int numy, const char **trees) {
  unsigned int n = 2 * (numx - 1 + numy - 1);
  for (int i = 1; i < numx - 1; i++) {
    for (int j = 1; j < numy - 1; j++) {
      printf ("Testing %d %d\n", i, j);
      if (VisibleFromAny (i, j, numx, numy, trees))  n++;
    }
  }
  return n;
}


void TrimLineEndings (int numlines, char **lines) {
  for (int i = 0; i < numlines; i++) {
    size_t vorher = strlen (lines[i]);
    char *c = strchr(lines[i], '\0');
    while (c > lines[i] && isspace (*(c-1))) { *(c-1) = '\0'; c--; }
    // printf ("line len = %zd -> %zd\n", vorher, strlen (lines[i]));
  }
}


int main () {
  printf ("--- Example ---\n");
  printf ("%zu x %zd trees\n", strlen (ExampleLines[0]),
    sizeof (ExampleLines) / sizeof (ExampleLines[0]));
  unsigned int numvisible = CountVisibleTrees (
    strlen (ExampleLines[0]),
    sizeof (ExampleLines) / sizeof (ExampleLines[0]), ExampleLines);
  printf ("* Visible trees: %u *\n", numvisible);
  printf ("\n");

  printf ("--- Puzzle 1: Visible trees ---\n");
  size_t maxlines = 500;
  size_t numlines;
  char **inputlines = (char**) calloc (500, sizeof (char*));
  ssize_t numchars = readlines ("08-treetop-tree-house-input.txt",
    &maxlines, &numlines, &inputlines);
  printf ("%zd characters on %zu lines read\n", numchars, numlines);
  TrimLineEndings (numlines, inputlines);
  numvisible = CountVisibleTrees (numlines, 99, inputlines);
  printf ("*** Visible trees: %u ***\n", numvisible);
}

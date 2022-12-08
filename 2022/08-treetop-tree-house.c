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


/// \brief Is the tree at position x, y visible from the left edge?
//
bool VisibleFromLeft (int x, int y, int numx, int numy, const char **trees) {
  for (int i = 0; i < x; i++)
    if (trees[y][x] <= trees[y][i])  return false;
  return true;
}

/// \brief Is the tree at position x, y visible from the right edge?
//
bool VisibleFromRight (int x, int y, int numx, int numy, const char **trees) {
  for (int i = x + 1; i < numx; i++)
    if (trees[y][x] <= trees[y][i])  return false;
  return true;
}

/// \brief Is the tree at position x, y visible from the upper edge?
//
bool VisibleFromTop (int x, int y, int numx, int numy, const char **trees) {
  for (int j = 0; j < y; j++)
    if (trees[y][x] <= trees[j][x])  return false;
  return true;
}

/// \brief Is the tree at position x, y visible from the lower edge?
//
bool VisibleFromBottom (int x, int y, int numx, int numy, const char **trees) {
  for (int j = y + 1; j < numy; j++)
    if (trees[y][x] <= trees[j][x])  return false;
  return true;
}

/// \brief Is the tree at position x, y visible from any edge?
//
bool VisibleFromAny (int x, int y, int numx, int numy, const char **trees) {
  return (VisibleFromLeft (x, y, numx, numy, trees) ||
          VisibleFromRight (x, y, numx, numy, trees) ||
          VisibleFromTop (x, y, numx, numy, trees) ||
          VisibleFromBottom (x, y, numx, numy, trees));
}

/// \brief How far can one look to the left from a secific position
//
unsigned int ViewLeft (int x, int y, int numx, int numy, const char **trees) {
  for (int i = x - 1; i >= 0; i--)
    if (trees[y][x] <= trees[y][i])  return x - i;
  // for (int i = 1; i <= x; i++)
  //   if (trees[y][x] <= trees[y][x-i])  return i;
  // No blocking tree, all tree between the position and the edge are visible
  return x;
}

/// \brief How far can one look to the right from a secific position
//
unsigned int ViewRight (int x, int y, int numx, int numy, const char **trees) {
  for (int i = x + 1; i < numx; i++)
    if (trees[y][x] <= trees[y][i])  return i - x;
  // for (int i = 1; i < numx - x; i++)
  //   if (trees[y][x] <= trees[y][x+i])  return i;
  // No blocking tree, all tree between the position and the edge are visible
  return numx - x - 1;
}

/// \brief How far can one look up from a secific position
//
unsigned int ViewUp (int x, int y, int numx, int numy, const char **trees) {
  for (int j = y - 1; j >= 0; j--)
    if (trees[y][x] <= trees[j][x])  return y - j;
  // No blocking tree, all tree between the position and the edge are visible
  return y;
}

/// \brief How far can one look down from a secific position
//
unsigned int ViewDown (int x, int y, int numx, int numy, const char **trees) {
  for (int j = y + 1; j < numy; j++)
    if (trees[y][x] <= trees[j][x])  return j - y;
  // No blocking tree, all tree between the position and the edge are visible
  return numy - y - 1;
}

/// \brief Determine the scenic score for a specific position
//
unsigned int GetScenicScore (int x, int y, int numx, int numy, const char **trees) {
  return ViewLeft (x, y, numx, numy, trees) *
    ViewRight (x, y, numx, numy, trees) *
    ViewUp (x, y, numx, numy, trees) *
    ViewDown (x, y, numx, numy, trees);
}

/// \brief How many trees are visible from any edge
//
unsigned int CountVisibleTrees (int numx, int numy, const char **trees) {
  unsigned int n = 2 * (numx - 1 + numy - 1);
  for (int i = 1; i < numx - 1; i++)
    for (int j = 1; j < numy - 1; j++)
      if (VisibleFromAny (i, j, numx, numy, trees))  n++;
  return n;
}

/// \brief Find the best scenic score of all trees
//
unsigned int GetBestScenicScore (int *posx, int *posy,
    int numx, int numy, const char **trees) {
  unsigned int bestscore = 0;
  for (int i = 0; i < numx; i++) {
    for (int j = 0; j < numx; j++) {
      unsigned int currscore = GetScenicScore (i, j, numx, numy, trees);
      if (currscore > bestscore) {
        *posx = i;  *posy = j;
        bestscore = currscore;
      }
    }
  }
  return bestscore;
}


/// \brief Remove spaces from the end of the lines
//
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
  unsigned int numvisible = CountVisibleTrees (strlen (ExampleLines[0]),
    sizeof (ExampleLines) / sizeof (ExampleLines[0]), ExampleLines);
  printf ("* Visible trees: %u *\n", numvisible);

  int bestx, besty;
  unsigned int bestscenicscore = GetBestScenicScore (&bestx, &besty,
    strlen (ExampleLines[0]),
    sizeof (ExampleLines) / sizeof (ExampleLines[0]), ExampleLines);
  printf ("* Best scenic score for tree (%d, %d): %u *\n",
    bestx, besty, bestscenicscore);
  printf ("\n");

  printf ("--- Puzzle 1: Visible trees ---\n");
  size_t maxlines = 500;
  size_t numlines;
  char **inputlines = (char**) calloc (500, sizeof (char*));
  ssize_t numchars = readlines ("08-treetop-tree-house-input.txt",
    &maxlines, &numlines, &inputlines);
  printf ("%zd characters on %zu lines read\n", numchars, numlines);
  TrimLineEndings (numlines, inputlines);
  printf ("%zu x %zu trees\n", strlen(inputlines[0]), numlines);
  numvisible = CountVisibleTrees (numlines, strlen (inputlines[0]),
    (const char**)inputlines);
  printf ("*** Visible trees: %u ***\n", numvisible);
  printf ("\n");

  printf ("--- Puzzle 2: Best spot for tree house ---\n");
  bestscenicscore = GetBestScenicScore (&bestx, &besty,
    strlen (inputlines[0]), numlines, (const char**)inputlines);
  printf ("*** Best scenic score for tree (%d, %d): %u ***\n",
    bestx, besty, bestscenicscore);

  for (int i = 0; i < numlines; i++)  free (inputlines[i]);
  free (inputlines);
}

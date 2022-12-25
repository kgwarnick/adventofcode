// Advent of Code 2022 Day 22: Monkey Map
// https://adventofcode.com/2022/day/22

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "freadlns.h"


enum Direction { Right = 0, Down = 1, Left = 2, Up = 3 };


/// \brief Find end of map and line with path description
///   (also remove trailing space of all lines)
/// \param numlines Number of input lines
/// \param lines    Complete input
/// \param[output] nummaplines  Number of lines describing the map
/// \param[output] pathdesc  Line with the description of the path
//
void PrepareInput (size_t numlines, char **lines,
    size_t *nummaplines, char **pathdesc) {
  for (size_t i = 0; i < numlines; i++) {
    char *c = strchr (lines[i], '\0');
    while (c > lines[i] && isspace (*(c-1))) { *(--c) = '\0'; }
    if (c == lines[i]) {   // empty line, end of the map
      *nummaplines = i;
      *pathdesc = lines[i+1];
    }
  }
}


/// \brief Get next x position after moving left n units,
///   blocking the path or wrapping around as necessary
//
long GetXMovedLeft (int x, int n, const char *tiles) {
  for (int i = 0; i < n; i++) {
    if (x > 0) {
      if (tiles[x-1] == '.') { x--; continue; }   // Path clear, move there
      else if (tiles[x-1] == '#')  break;   // Path blocked, stop moving
    }
    // Outside the map, wrap to the right edge
    int tempx = x;
    while (tempx < (int) strlen (tiles) - 1 && (tiles[tempx+1] == '.' || tiles[tempx+1] == '#'))  tempx++;
    if (tiles[tempx] == '.') {
      x = tempx;
      // printf ("- Line: %s, Wrapped to right edge: %d\n", tiles, x);
    }
    else {
      // printf ("- Wrapped right edge is blocked: %d\n", x);
      break;
    }
  }
  // Modified x is new x position
  return x;
}

/// \brief Get next x position after moving right n units,
///   blocking the path or wrapping around as necessary
//
long GetXMovedRight (int x, int n, const char *tiles) {
  for (int i = 0; i < n; i++) {
    if (x < (int) strlen (tiles) - 1) {
      if (tiles[x+1] == '.') { x++; continue; }   // Path clear, move there
      else if (tiles[x+1] == '#')  break;   // Path blocked, stop moving
    }
    // Outside the map, wrap to the left edge
    int tempx = x;
    while (tempx > 0 && (tiles[tempx-1] == '.' || tiles[tempx-1] == '#'))  tempx--;
    if (tiles[tempx] == '.') {
      x = tempx;
      // printf ("- Line: %s, Wrapped to left edge: %d\n", tiles, x);
    }
    else {
      // printf ("- Wrapped left edge is blocked: %d\n", x);
      break;
    }
  }
  // Modified x is new x position
  return x;
}

/// \brief Get next y position after moving up n units,
///   blocking the path or wrapping around as necessary
//
long GetYMovedUp (int x, int y, int n, size_t mapheight, const char **map) {
  for (int i = 0; i < n; i++) {
    if (y > 0) {
      if (map[y-1][x] == '.') { y--; continue; }   // Path clear, move there
      else if (map[y-1][x] == '#')  break;   // Path blocked, stop moving
    }
    // Outside the map, wrap to the lower edge
    int tempy = y;
    while (tempy < (int) mapheight - 1 && (map[tempy+1][x] == '.' || map[tempy+1][x] == '#'))  tempy++;
    if (map[tempy][x] == '.') {
      y = tempy;
      // printf ("- Wrapped to lower edge: (%d, %d)\n", x, y);
    }
    else {
      // printf ("- Wrapped lower edge is blocked: (%d, %d)\n", x, y);
      break;
    }
  }
  // Modified y is new y position
  return y;
}

/// \brief Get next y position after moving down n units,
///   blocking the path or wrapping around as necessary
//
long GetYMovedDown (int x, int y, int n, size_t mapheight, const char **map) {
  for (int i = 0; i < n; i++) {
    // printf ("down from (%d, %d) ?\n", x, y);
    if (y < (int) mapheight - 1) {
      if (map[y+1][x] == '.')  { y++; continue; }  // Path clear, move there
      else if (map[y+1][x] == '#')  break;   // Path blocked, stop moving
    }
    // Outside the map, wrap to the upper edge
    int tempy = y;
    while (tempy > 0 && (map[tempy-1][x] == '.' || map[tempy-1][x] == '#'))  tempy--;
    if (map[tempy][x] == '.') {
      y = tempy;
      // printf ("- Wrapped to upper edge: (%d, %d)\n", x, y);
    }
    else {
      // printf ("- Wrapped upper edge is blocked: (%d, %d)\n", x, y);
      break;
    }
  }
  // Modified y is new y position
  return y;
}


long TracePath (size_t mapheight, const char **map, const char *path) {
  // Start position is upper left map tile, facing right
  int y = 0;
  int x = strchr (map[0], '.') - map[0];
  enum Direction face = Right;
  // "Instruction pointer" within the path description
  const char *ip = path;
  char *endpath = strchr (path, '\0');
  // Follow instructions
  size_t actions = 0;
  printf ("Start position (%d, %d), facing %d, path length: %ld characters\n",
    x, y, face, endpath - path);
  while (ip < endpath) {
    // printf ("[%4lu] Position (%d, %d), facing %d;  instruction pointer: %ld\n",
    //   actions, x, y, face, ip - path);
    // Read a movement distance or a direction change instruction
    if (isdigit (*ip)) {
      char *nextip;
      int n = strtol (ip, &nextip, 10);
      // printf ("Movement distance: %d, next instructions: %.10s\n", n, nextip);
      if (face == Right)  x = GetXMovedRight (x, n, map[y]);
      else if (face == Left)  x = GetXMovedLeft (x, n, map[y]);
      else if (face == Down)  y = GetYMovedDown (x, y, n, mapheight, map);
      else if (face == Up)  y = GetYMovedUp (x, y, n, mapheight, map);
      else printf ("Error: Unsupported move direction ignored: %d\n", face);
      ip = nextip;
    } else {
      if (*ip == 'L')  face = (face + 3) % 4;
      else if (*ip == 'R')  face = (face + 1) % 4;
      else fprintf (stderr, "Invalid rotation spec: %c\n", *ip);
      ip++;
      // printf ("New direction: %d, next instructions: %.10s\n", face, ip);
    }
    actions++;
  }
  printf ("End position after %zu actions: (%d, %d), facing: %d\n",
    actions, x, y, face);
  return (y + 1) * 1000 + (x + 1) * 4 + face;
}


char *examplelines[] = {
  "        ...#",
  "        .#..",
  "        #...",
  "        ....",
  "...#.......#",
  "........#...",
  "..#....#....",
  "..........#.",
  "        ...#....",
  "        .....#..",
  "        .#......",
  "        ......#.",
  "",
  "10R5L5R10L4R5L5"
};

int main () {
  printf ("--- Example ---\n");
  size_t mapheight;
  char *mappath;
  PrepareInput (sizeof (examplelines) / sizeof (examplelines[0]),
    examplelines, &mapheight, &mappath);
  printf ("Map height: %zu, path description: %s\n", mapheight, mappath);
  long password = TracePath (mapheight, (const char**)examplelines, mappath);
  printf ("* Password: %ld *\n", password);
  printf ("\n");

  printf ("--- Puzzle 1: Find end position ---\n");
  size_t maxlines = 250, numlines = 0;
  char **inputlines = (char**) malloc (maxlines * sizeof (char*));
  ssize_t numchars = readlines ("22-monkey-map-input.txt",
    &maxlines, &numlines, &inputlines);
  printf ("Read %zu lines, %zd characters\n", numlines, numchars);
  PrepareInput (numlines, inputlines, &mapheight, &mappath);
  printf ("Last two line lengths: %zu, %zu\n",
    strlen (inputlines[numlines-2]), strlen (inputlines[numlines-1]));
  // printf ("Map height: %zu, path description: %s\n", mapheight, mappath);
  password = TracePath (mapheight, (const char**)inputlines, mappath);
  printf ("*** Password: %ld ***\n", password);
  return 0;
}

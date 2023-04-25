// Advent of Code 2022 Day 22: Monkey Map
// https://adventofcode.com/2022/day/22

#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "freadlns.h"

int min (int a, int b) { return a < b ? a : b; }
int max (int a, int b) { return a > b ? a : b; }

/// \brief Facing which direction
enum Direction { Right = 0, Down = 1, Left = 2, Up = 3 };

/// \brief Transition around a cube edge
///
/// If the cube mesh is left at a certain edge in a certain direction,
/// where will the mesh be entered again and in what direction
//
typedef struct CubeEdge_st {
  int fromx1;   ///< leaving edge, x coordinate of edge start point
  int fromy1;   ///< leaving edge, y coordinate of edge start point
  int fromx2;   ///< leaving edge, x coordinate of edge end point
  int fromy2;   ///< leaving edge, y coordinate of edge end point
  enum Direction fromface;   ///< leave the edge facing in this direction
  int tox1;   ///< enter edge, x coordinate of edge start point
  int toy1;   ///< enter edge, y coordinate of edge start point
  int tox2;   ///< enter edge, x coordinate of edge end point
  int toy2;   ///< enter edge, y coordinate of edge end point
  enum Direction toface;   ///< enter the edge facing in this direction
} CubeEdge;


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
    while (tempy < (int) mapheight - 1 && x < (int) strlen (map[tempy+1]) &&
      (map[tempy+1][x] == '.' || map[tempy+1][x] == '#'))  tempy++;
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
    if (y < (int) mapheight - 1 && x < (int) strlen (map[y+1])) {
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


/// \brief Calculate position to enter the cube mesh according to the edge transition
/// \param[output] *x  New x position
/// \param[output] *y  New y position
/// \param[input]   n  Offset along the edge where to enter
/// \param[input] edge Cube edge to use for calculating the position where to enter
/// \return  true if a new position was successfully calculated, false on error
//
bool EnterEdge (int *x, int *y, int n, const CubeEdge edge) {
  if (edge.tox1 < edge.tox2 && edge.toy1 == edge.toy2) {
    *x = edge.tox1 + n;  *y = edge.toy1;  return true; }
  else if (edge.tox1 > edge.tox2 && edge.toy1 == edge.toy2) {
    *x = edge.tox1 - n;  *y = edge.toy1;  return true; }
  else if (edge.toy1 < edge.toy2 && edge.tox1 == edge.tox2) {
    *x = edge.tox1;  *y = edge.toy1 + n;  return true; }
  else if (edge.toy1 > edge.toy2 && edge.tox1 == edge.tox2) {
    *x = edge.tox1;  *y = edge.toy1 - n;  return true; }
  else {
    printf ("Error: EnterEdge: Invalid edge (%d, %d, %d, %d)\n",
      edge.tox1, edge.toy1, edge.tox2, edge.toy2);
  }
  return false;
}


/// \brief Calculate new x, y position and facing when wrapping around the edge
/// \param *x  x position, may be changed by this function
/// \param *y  y position, may be changed by this function
/// \param *face  New direction, may be changed by this function
/// \param[input] edge  Cube edge to use for moving from edge to edge
/// \return true if the position could be wrapped around the edge successfully,
///   false if this edge was not applicable to the input position and facing
//
bool WrapAroundEdge (int *x, int *y, enum Direction *face, CubeEdge edge) {
  // printf ("Info: WrapAroundEdge: (x, y, face) = (%d, %d, %d),  "
  //   "Edge from (%d, %d, %d, %d, face %d) to (%d, %d, %d, %d, face %d)\n",
  //   *x, *y, *face,  edge.fromx1, edge.fromy1, edge.fromx2, edge.fromy2, edge.fromface,
  //   edge.tox1, edge.toy1, edge.tox2, edge.toy2, edge.toface);
  // Leave vertical edge to the left or right
  if (((*face == Left && edge.fromface == Left) ||
       (*face == Right && edge.fromface == Right)) && *x == edge.fromx1 && *x == edge.fromx2) {
    // Vertical edge from top to bottom
    if (*y >= edge.fromy1 && *y <= edge.fromy2) {
      int dy = *y - edge.fromy1;
      if (EnterEdge (x, y, dy, edge)) { *face = edge.toface; return true; }
      else { printf (
        "Error: WrapAroundEdge: Failed to enter at edge (%d, %d, %d, %d)\n",
	edge.tox1, edge.toy1, edge.tox2, edge.toy2);
      }
    }
    // Vertical edge from bottom to top
    else if (*y <= edge.fromy1 && *y >= edge.fromy2) {
      int dy = edge.fromy1 - *y;
      if (EnterEdge (x, y, dy, edge)) { *face = edge.toface; return true; }
      else { printf (
        "Error: WrapAroundEdge: Failed to enter at edge (%d, %d, %d, %d)\n",
	edge.tox1, edge.toy1, edge.tox2, edge.toy2);
      }
    }
    else {
      // printf ("Error: WrapAroundEdge: Invalid vertical edge: (%d, %d, %d, %d)\n",
      //   edge.fromx1, edge.fromy1, edge.fromx2, edge.fromy2);
      return false;
    }
  }
  // Leave horizontal edge up or down
  else if (((*face == Up && edge.fromface == Up) ||
       (*face == Down && edge.fromface == Down)) && *y == edge.fromy1 && *y == edge.fromy2) {
    // Horizontal edge from left to right
    if (*x >= edge.fromx1 && *x <= edge.fromx2) {
      int dx = *x - edge.fromx1;
      if (EnterEdge (x, y, dx, edge)) { *face = edge.toface; return true; }
      else { printf (
        "Error: WrapAroundEdge: Failed to enter at edge (%d, %d, %d, %d)\n",
	edge.tox1, edge.toy1, edge.tox2, edge.toy2);
      }
    }
    // Horizontal edge from right to left
    else if (*x <= edge.fromx1 && *x >= edge.fromx2) {
      int dx = edge.fromx1 - *x;
      if (EnterEdge (x, y, dx, edge)) { *face = edge.toface; return true; }
      else { printf (
        "Error: WrapAroundEdge: Failed to enter at edge (%d, %d, %d, %d)\n",
	edge.tox1, edge.toy1, edge.tox2, edge.toy2);
      }
    }
    else {
      // printf ("Error: WrapAroundEdge: Invalid horizontal edge: (%d, %d, %d, %d)\n",
      //   edge.fromx1, edge.fromy1, edge.fromx2, edge.fromy2);
      return false;
    }
  }
  return false;
}


/// \brief Perform a movement step, edges are wrapped as if moving on a cube surface
/// \param *x  position, x coordinate
/// \param *y  position, y coordinate
/// \param *face  direction facing
/// \apram n   steps to move
/// \param mapheight  number of lines in the map
/// \param **map  array of lines of the map
/// \param numedges  number of edges describing the cube surface
/// \param *edges  edges of the cube
//
void PerformCubeMove (int *x, int *y, enum Direction *face, int n,
    size_t mapheight, const char **map, size_t numedges, const CubeEdge *edges) {
  for (int i = 0; i < n; i++) {
    // Target position
    int tx = *x + (*face == Right ? 1 : (*face == Left ? -1 : 0));
    int ty = *y + (*face == Down  ? 1 : (*face == Up   ? -1 : 0));
    // Target position inside the map
    if (ty >= 0 && ty < (int) mapheight && tx >= 0 && tx < (int) strlen (map[ty])) {
      // Target position is inside the map, move there if not blocked
      if (map[ty][tx] == '.') { *x = tx;  *y = ty;  continue; }
      // If blocked, stop moving (discard remaining movements)
      if (map[ty][tx] == '#') { break; }
    }
    // Target lies outside the map, search an appropriate edge to wrap around
    bool wrapped = false;
    for (size_t e = 0; e < numedges; e++) {
      int newx = *x, newy = *y;  enum Direction newface = *face;
      if (WrapAroundEdge (&newx, &newy, &newface, edges[e])) {
	if (map[newy][newx] == '.') {
#ifdef DEBUG
          printf ("Info: PerformCubeMove: Wrapping (%d, %d, face %d) to "
	    "(%d, %d, face %d)\n", *x, *y, *face, newx, newy, newface);
#endif
	  *x = newx;  *y = newy;  *face = newface;
	  wrapped = true;
          break;
	}
	else if (map[newy][newx] == '#') {
#ifdef DEBUG
          printf ("Info: PerformCubeMove: Staying at (%d, %d, face %d) because "
            "(%d, %d, face %d) is blocked\n", *x, *y, *face, newx, newy, newface);
#endif
	  return;
	}
	else {
	  printf ("Error: PerformCubeMove: Invalid target map tile: (%d, %d) = %c\n",
	    newx, newy, map[newy][newx]);
	}
      }
    }
    // Loop exit without successful wrap-around?
    if (!wrapped) printf (
      "Error: PerformCubeMove: Could not find an edge to wrap "
        "around position (%d, %d, face %d)\n", *x, *y, *face);
  }
}


/// \brief Follow the path around the map, wrapping around edges in-line/in-column
///   or cube-like if the cube edges are also passed
//
long TracePath (size_t mapheight, const char **map, const char *path,
    size_t numedges, const CubeEdge *edges) {
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
#ifdef DEBUG
    printf ("[%4lu] Position (%d, %d), facing %d;  instruction pointer: %ld -> %s\n",
      actions, x, y, face, ip - path, ip);
#endif
    // Read a movement distance or a direction change instruction
    if (isdigit (*ip)) {
      char *nextip;
      int n = strtol (ip, &nextip, 10);
      // printf ("Movement distance: %d, next instructions: %.10s\n", n, nextip);
      if (numedges > 0)   // Cube wrap-around
        PerformCubeMove (&x, &y, &face, n, mapheight, map, numedges, edges);
      else {   // In-line or in-column wrap-around
        if (face == Right)  x = GetXMovedRight (x, n, map[y]);
        else if (face == Left)  x = GetXMovedLeft (x, n, map[y]);
        else if (face == Down)  y = GetYMovedDown (x, y, n, mapheight, map);
        else if (face == Up)  y = GetYMovedUp (x, y, n, mapheight, map);
        else printf ("Error: Unsupported move direction ignored: %d\n", face);
      }
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


/// \brief Draw a visualisation of the map, optionally marking edges
//
void DrawMap (size_t mapheight, const char **map,
    size_t numedges, const CubeEdge *edges) {
  // Duplicate map to s, so the original is not modified
  char **s = malloc (mapheight * sizeof (char*));
  for (size_t y = 0; y < mapheight; y++)  s[y] = strdup (map[y]);
  printf ("- \"From\" edges (leaving the mesh)\n");
  printf ("numedges = %zu\n", numedges);
  for (size_t i = 0; i < numedges; i++) {
    int dx = edges[i].fromx1 > edges[i].fromx2 ? -1 :
             edges[i].fromx1 < edges[i].fromx2 ?  1 :  0;
    int dy = edges[i].fromy1 > edges[i].fromy2 ? -1 :
             edges[i].fromy1 < edges[i].fromy2 ?  1 :  0;
    int numpoints = max (abs (edges[i].fromx2 - edges[i].fromx1),
                         abs (edges[i].fromy2 - edges[i].fromy1)) + 1;
    for (int n = 0, x = edges[i].fromx1, y = edges[i].fromy1;
         n < numpoints;  n++, x += dx, y += dy) {
      s[y][x] = 'a' + i;
    }
  }
  for (size_t y = 0; y < mapheight; y++) {
    printf ("%4zu|%s\n", y, s[y]);
  }
  printf ("- \"To\" edges (entering the mesh)\n");
  for (size_t i = 0; i < numedges; i++) {
    int dx = edges[i].tox1 > edges[i].tox2 ? -1 :
             edges[i].tox1 < edges[i].tox2 ?  1 :  0;
    int dy = edges[i].toy1 > edges[i].toy2 ? -1 :
             edges[i].toy1 < edges[i].toy2 ?  1 :  0;
    int numpoints = max (abs (edges[i].tox2 - edges[i].tox1),
                         abs (edges[i].toy2 - edges[i].toy1)) + 1;
    for (int n = 0, x = edges[i].tox1, y = edges[i].toy1;
         n < numpoints;  n++, x += dx, y += dy) {
      s[y][x] = 'A' + i;
    }
  }
  for (size_t y = 0; y < mapheight; y++) {
    printf ("%4zu|%s\n", y, s[y]);
    free (s[y]);
  }
  free (s);
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

/// \brief Edges of example cube
/*    A  *
 *  BCD  *
 *    EF */
const CubeEdge exampleedges[] = {
  {  8,  0, 11,  0, Up,     3,  4,  0,  4, Down },    // A -> B
  {  8,  0,  8,  3, Left,   7,  4,  4,  4, Down },    // A -> C
  {  0,  4,  3,  4, Up,    11,  0,  8,  0, Down },    // B -> A
  {  4,  4,  7,  4, Up,     8,  0,  8,  3, Right },   // C -> A
  { 11,  0, 11,  3, Right, 15, 11, 15,  8, Left },    // A -> F
  { 11,  4, 11,  7, Right, 15,  8, 12,  8, Down },    // D -> F
  { 12,  8, 15,  8, Up,    11,  7, 11,  4, Left },    // F -> D
  { 15,  8, 15, 11, Right, 11,  3, 11,  0, Left },    // F -> A
  {  0,  4,  0,  7, Left,  15, 11, 12, 11, Up },      // B -> F
  { 12, 11, 15, 11, Down,   0,  7,  0,  4, Right },   // F -> B
  {  0,  7,  3,  7, Down,  11, 11,  8, 11, Up },      // B -> E
  {  8, 11, 11, 11, Down,   3,  7,  0,  7, Up },      // E -> B
  {  4,  7,  7,  7, Down,   8, 11,  8,  8, Right },   // C -> E
  {  8,  8,  8, 11, Left,   7,  7,  4,  7, Up }       // E -> C
};


/// \brief Edges of puzzle input cube
/*   AB *
 *   C  *
 *  DE  *
 *  F   */
const CubeEdge inputedges[] = {
  {  50,   0,  99,   0, Up,      0, 150,   0, 199, Right },   // A -> F
  {   0, 150,   0, 199, Left,   50,   0,  99,   0, Down },    // F -> A
  { 100,   0, 149,   0, Up,      0, 199,  49, 199, Up },      // B -> F
  {   0, 199,  49, 199, Down,  100,   0, 149,   0, Down },    // F -> B
  {  50,   0,  50,  49, Left,    0, 149,   0, 100, Right },   // A -> D
  {   0, 100,   0, 149, Left,   50,  49,  50,   0, Right },   // D -> A
  { 149,   0, 149,  49, Right,  99, 149,  99, 100, Left },    // B -> E
  {  99, 100,  99, 149, Right, 149,  49, 149,   0, Left },    // E -> B
  {  50,  50,  50,  99, Left,    0, 100,  49, 100, Down },    // C -> D
  {   0, 100,  49, 100, Up,     50,  50,  50,  99, Right },   // D -> C
  {  99,  50,  99,  99, Right, 100,  49, 149,  49, Up },      // C -> B
  { 100,  49, 149,  49, Down,   99,  50,  99,  99, Left },    // B -> C
  {  50, 149,  99, 149, Down,   49, 150,  49, 199, Left },    // E -> F
  {  49, 150,  49, 199, Right,  50, 149,  99, 149, Up }       // F -> E
};


int main () {
  printf ("--- Example ---\n");
  size_t mapheight;
  char *mappath;
  PrepareInput (sizeof (examplelines) / sizeof (examplelines[0]),
    examplelines, &mapheight, &mappath);
  // DrawMap (mapheight, (const char**)examplelines, 0, NULL);
  // DrawMap (mapheight, (const char**)examplelines,
  //   sizeof (exampleedges) / sizeof (exampleedges[0]), exampleedges);
  printf ("Map height: %zu, path description: %s\n", mapheight, mappath);
  long password = TracePath (mapheight, (const char**)examplelines, mappath, 0, NULL);
  printf ("* Password: %ld *\n", password);
  printf ("\n");
  printf ("With cube wrap-around\n");
  password = TracePath (mapheight, (const char**)examplelines, mappath,
    sizeof (exampleedges) / sizeof (exampleedges[0]), exampleedges);
  printf ("* Password: %ld *\n", password);
  printf ("\n");

  printf ("--- Puzzle 1: Find end position ---\n");
  size_t maxlines = 250, numlines = 0;
  char **inputlines = (char**) malloc (maxlines * sizeof (char*));
  ssize_t numchars = readlines ("22-monkey-map-input.txt",
    &maxlines, &numlines, &inputlines);
  printf ("Read %zu lines, %zd characters\n", numlines, numchars);
  PrepareInput (numlines, inputlines, &mapheight, &mappath);
  // DrawMap (mapheight, (const char**)inputlines,
  //   sizeof (inputedges) / sizeof (inputedges[0]), inputedges);
  printf ("Last two line lengths: %zu, %zu\n",
    strlen (inputlines[numlines-2]), strlen (inputlines[numlines-1]));
  // printf ("Map height: %zu, path description: %s\n", mapheight, mappath);
  password = TracePath (mapheight, (const char**)inputlines, mappath, 0, NULL);
  printf ("*** Password: %ld ***\n", password);
  printf ("\n");

  printf ("--- Puzzle 2: Find end position with cube-like wrap-around ---\n");
  password = TracePath (mapheight, (const char**)inputlines, mappath,
    sizeof (inputedges) / sizeof (inputedges[0]), inputedges);
  printf ("*** Password: %ld ***\n", password);
  for (size_t i = 0; i < numlines; i++)   free (inputlines[i]);
  free (inputlines);
  return 0;
}

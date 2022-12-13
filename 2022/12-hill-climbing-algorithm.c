// Advent of Code 2022 Day 12 Hill Climbing Algorithm
// https://adventofcode.com/2022/day/12

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "freadlns.h"
#include "trimendl.h"


const char *ExampleField[] = {
  "Sabqponm",
  "abcryxxl",
  "accszExk",
  "acctuvwj",
  "abdefghi"
};


typedef struct Position_struct {
  unsigned short int x;
  unsigned short int y;
} Position;


/// \brief Find start or end point marked by the elevation value specified
//
Position FindStartEndPoint (unsigned short width, unsigned short height,
    const char **terrain, char elevalue) {
  Position startendpt = { -1, -1 };
  for (unsigned short y = 0; y < height; y++)
    for (unsigned short x = 0; x < width; x++)
      if (terrain[y][x] == elevalue) {
        startendpt.x = x;
        startendpt.y = y;
        return startendpt;
      }
  return startendpt;
}


/// \brief Output the path in text form
//
void PrintPath (unsigned short numsteps, const Position *path) {
  for (unsigned short i = 0; i < numsteps; i++) {
    printf (" (%u,%u)", path[i].x, path[i].y);
    if (i % 10 == 9 || i == numsteps - 1)  printf ("\n");
  }
}

/// \brief Visualise the path
//
void DrawPath (unsigned short numsteps, const Position *path,
    unsigned short width, unsigned short height) {
  char *f = (char*) malloc (width * height * sizeof (char));
  memset (f, '.' , width * height * sizeof (char));
  for (unsigned short i = 0; i < numsteps - 1; i++) {
    Position p = path[i], s = path[i+1];
    // Put direction markers along the path
    if      (p.x + 1 == s.x && p.y     == s.y)  f[width * p.y + p.x] = '>';
    else if (p.x - 1 == s.x && p.y     == s.y)  f[width * p.y + p.x] = '<';
    else if (p.x     == s.x && p.y + 1 == s.y)  f[width * p.y + p.x] = 'v';
    else if (p.x     == s.x && p.y - 1 == s.y)  f[width * p.y + p.x] = '^';
    else f[width * p.y + p.x] = '#';
  }
  // Put end marker
  f[width * path[numsteps-1].y + path[numsteps-1].x] = '*';
  // Print all lines
  for (unsigned short y = 0; y < height; y++) {
    for (unsigned short x = 0; x < width; x++) {
      printf ("%c", f[y * width + x]);
    }
    printf ("\n");
  }
  free (f);
}


// Currently not used
//
bool AlreadyVisited (Position pos, unsigned short numsteps, const Position *path) {
  for (int p = 0; p < numsteps; p++)
    if (path[p].x == pos.x && path[p].y == pos.y) {
      // printf ("- already visited: (%u, %u)\n", path[p].x, path[p].y);
      return true;
    }
  return false;
}


/// \brief Test for an allowed step between the two elevation values
//
bool IsValidStep (char currelev, char destelev) {
  if (currelev == 'S')  currelev = 'a';
  if (currelev == 'E')  currelev = 'z';
  if (destelev == 'S')  destelev = 'a';
  if (destelev == 'E')  destelev = 'z';
  return (destelev <= currelev + 1);
}


/// \brief Find a node with the lowest distance value that is not marked as "visited"
///   (Diijkstra's algorithm)
//
Position FindNearestUnvisitedNode (unsigned short width, unsigned short height,
    bool* visits, unsigned short *distances) {

  Position testpos;  testpos.x = (unsigned short)-1;  testpos.y = (unsigned short)-1;
  unsigned short smallestdist = (unsigned short)-1;
  for (int y = 0; y < height; y++)
    for (int x = 0; x < width; x++)
      if (!visits[y * width + x] &&
          (distances[y * width + x] < smallestdist)) {
        smallestdist = distances[y * width + x];
        testpos.x = x;  testpos.y = y;
      }
  return testpos;
}


/// \brief Print the current map of distances according to Dijkstra's algorithm
//
void PrintDistanceTable (unsigned short width, unsigned short height,
    unsigned short *distance, int printwidth) {
  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      printf (" %*u", printwidth, distance[y * width + x]);
    }
    printf ("\n");
  }
}


/// \brief Extract a path by following positions from end to start point
/// \param width    Width of the field
/// \param height   Height of the field
/// \param distmap  Elevation map of the field
/// \param startpt  Start point with low elevation to reach
/// \param endpt    End point with high elevation to start backtracking the path
/// \param numpositions  Pointer to integer with number of positions in the path,
///   including start and end point, so should be one larger than the distance
/// \param path     Pointer to the array of path nodes, shoud already be
///   allocated to have room for all nodes including the start and end point,
///   i.e. one more than the elevation distance from start to end.
///   If it is too small, it will be reallocated
/// The path must be freed by the caller
//
bool RetraceSteps (unsigned short width, unsigned short height,
    unsigned short *distmap, Position startpt, Position endpt,
    unsigned short *numpositions, Position **path) {
  // Elevation of start and end point
  unsigned short startelev = distmap[startpt.y * width + startpt.x];
  unsigned short endelev = distmap[endpt.y * width + endpt.x];
  *numpositions = endelev - startelev + 1;
  // Enough space for the path or reallocate?
  // - Don't reallocate if the solution is longer than necessary or we
  //   destroy the result of recursive calls higher up
  if (path == NULL || *numpositions < endelev - startelev + 1)
    *path = realloc (*path, (endelev - startelev + 1) * sizeof (Position));
  if (path == NULL)  return false;
  if (*numpositions == 0)  return false;   // Should not happen, wrong start point reached?!
  // Stop if the start point was reached (or the distance is zero? should be equivalent)
  if ((startpt.x == endpt.x) && (endpt.y == startpt.y)) {
    // Store the start point as first point of the path
    (*path)[*numpositions-1].x = endpt.x;  (*path)[*numpositions-1].y = endpt.y;
    return true;
  }
  // Follow from end to start by decreasing the distance in every step:
  // Put the current end point as last point of the path
  unsigned short currstep = *numpositions - 1;
  (*path)[currstep].x = endpt.x;  (*path)[currstep].y = endpt.y;
  // Find a suitable neighbour and recursively get the path from there to start
  // - Try left neighbour
  if (endpt.x > 0 && (distmap[endpt.y * width + endpt.x - 1] == endelev - 1)) {
    Position prevpt;  prevpt.x = endpt.x - 1;  prevpt.y = endpt.y;
    if (RetraceSteps (width, height, distmap, startpt, prevpt, &currstep, path))
      return true;
  }
  // - Try right neighbour
  if (endpt.x < width - 1 && (distmap[endpt.y * width + endpt.x + 1] == endelev - 1)) {
    Position prevpt;  prevpt.x = endpt.x + 1;  prevpt.y = endpt.y;
    if (RetraceSteps (width, height, distmap, startpt, prevpt, &currstep, path))
      return true;
  }
  // - Try upper neighbour
  if (endpt.y > 0 && (distmap[(endpt.y - 1) * width + endpt.x] == endelev - 1)) {
    Position prevpt;  prevpt.x = endpt.x;  prevpt.y = endpt.y - 1;
    if (RetraceSteps (width, height, distmap, startpt, prevpt, &currstep, path))
      return true;
  }
  // - Try lower neighbour
  if (endpt.x < width && (distmap[(endpt.y + 1) * width + endpt.x] == endelev - 1)) {
    Position prevpt;  prevpt.x = endpt.x;  prevpt.y = endpt.y + 1;
    if (RetraceSteps (width, height, distmap, startpt, prevpt, &currstep, path))
      return true;
  }
  // No path from any neighbour could be found
  return false;
}


/// \brief Show a path from start to end point in text form and visualised
/// \param width    Width of the field
/// \param height   Height of the field
/// \param distmap  Elevation map of the field
/// \param startpt  Start point with low elevation to reach
/// \param endpt    End point with high elevation to start backtracking from
/// \param numsteps Number of steps from start to end point
//
void ShowRetracedPath (unsigned short width, unsigned short height,
    unsigned short *distmap, Position startpoint, Position endpoint,
    unsigned short numsteps) {
  unsigned short pathsteps = numsteps + 1;
  // Allocate one position more to include start AND end point in the path
  Position *solutionpath = (Position*) malloc ((numsteps + 1) * sizeof (Position));
  if (RetraceSteps (width, height, distmap,
      startpoint, endpoint, &pathsteps, &solutionpath)) {
    PrintPath (pathsteps, solutionpath);
    DrawPath (pathsteps, solutionpath, width, height);
  }
  else {
    printf ("Error: Failed to retrace the path from start to end\n");
  }
  free (solutionpath);
}


/// \brief Find the shortest path to the destination point by Diijkstra's algorithm
//
unsigned short FindShortestPath (Position startpoint,
    unsigned short width, unsigned short height, const char **field, int loglevel) {
  // Visited markers, initialise to false
  bool *visited = (bool*) malloc (width * height * sizeof (bool));
  memset (visited, 0, width * height * sizeof (bool));
  Position currpos;
  if (startpoint.x != (unsigned short)-1 && startpoint.y != (unsigned short)-1)
    currpos = startpoint;
  else {
    currpos = FindStartEndPoint (width, height, field, 'S');
    startpoint = currpos;   // also save starting point
  }
  Position endpoint = FindStartEndPoint (width, height, field, 'E');
  // Distance table, fill with maximum value
  unsigned short *distance = (unsigned short*) malloc (width * height * sizeof (short));
  memset (distance, 0xFF, width * height * sizeof (short));
  // Set distance of starting point to 0
  distance[currpos.y * width + currpos.x] = 0;
  unsigned short mindistance = (unsigned short)-1;
  while (currpos.x != (unsigned short)-1 && currpos.y != (unsigned short)-1) {
    // Set distance of all neighbours to 1 more unless already lower or already visited
    unsigned short currdist = distance[currpos.y * width + currpos.x];
    Position newpos;
    // - up
    if (currpos.y > 0) {
      newpos.x = currpos.x;  newpos.y = currpos.y - 1;
      if (!visited[newpos.y * width + newpos.x] &&
          IsValidStep (field[currpos.y][currpos.x], field[newpos.y][newpos.x])) {
        if (distance[newpos.y * width + newpos.x] > currdist + 1) {
          distance[newpos.y * width + newpos.x] = currdist + 1;
        }
      }
    }
    // - down
    if (currpos.y < height - 1) {
      newpos.x = currpos.x;  newpos.y = currpos.y + 1;
      if (!visited[newpos.y * width + newpos.x] &&
          IsValidStep (field[currpos.y][currpos.x], field[newpos.y][newpos.x])) {
        if (distance[newpos.y * width + newpos.x] > currdist + 1) {
          distance[newpos.y * width + newpos.x] = currdist + 1;
        }
      }
    }
    // - left
    if (currpos.x > 0) {
      newpos.x = currpos.x - 1;  newpos.y = currpos.y;
      if (!visited[newpos.y * width + newpos.x] &&
          IsValidStep (field[currpos.y][currpos.x], field[newpos.y][newpos.x])) {
        if (distance[newpos.y * width + newpos.x] > currdist + 1) {
          distance[newpos.y * width + newpos.x] = currdist + 1;
        }
      }
    }
    // - down
    if (currpos.x < width - 1) {
      newpos.x = currpos.x + 1;  newpos.y = currpos.y;
      if (!visited[newpos.y * width + newpos.x] &&
          IsValidStep (field[currpos.y][currpos.x], field[newpos.y][newpos.x])) {
        if (distance[newpos.y * width + newpos.x] > currdist + 1) {
          distance[newpos.y * width + newpos.x] = currdist + 1;
        }
      }
    }
    // Set current position to "visited"
    visited[currpos.y * width + currpos.x] = true;
    // Print current distance map
    // PrintDistanceTable (width, height, distance, 5);
    // End point reached?
    if (visited[endpoint.y * width + endpoint.x]) {
      mindistance = distance[endpoint.y * width + endpoint.x];
      // printf ("End point (%u, %u) reached in %d steps\n",
      //   endpoint.x, endpoint.y, mindistance);
      // Show the current path
      if (loglevel >= 2)
        ShowRetracedPath (width, height, distance, startpoint, endpoint, mindistance);
      // Stop searching
      break;
    }
    // Find next node to visit: smallest unvisited distance
    currpos = FindNearestUnvisitedNode (width, height, visited, distance);
    // printf ("Next node: %u, %u\n", currpos.x, currpos.y);
  }
  free (visited);
  free (distance);
  return mindistance;
}


/// \brief Find the starting point that leads to the shortest path to the destination
///   from all points with lowest elevation
//
unsigned short FindBestStartingPoint (unsigned short width, unsigned short height,
    const char **terrain, int loglevel) {
  // Position beststart;  beststart.x = (unsigned short)-1;  beststart.y = (unsigned short)-1;
  unsigned short shortestpath = (unsigned short)-1;
  // Find all points with elevation a to calculate the minimum path length
  for (unsigned short y = 0; y < height; y++) {
    for (unsigned short x = 0; x < width; x++) {
      if (terrain[y][x] == 'a') {
        Position testpos;  testpos.x = x;  testpos.y = y;
        unsigned short pathlen = FindShortestPath (testpos, width, height, terrain, loglevel);
        // printf ("Possible starting point at (%u, %u) with path length %u\n",
        //   x, y, pathlen);
        if (pathlen < shortestpath) {
          shortestpath = pathlen;
          // beststart = testpos;
        }
      }
    }
  }
  // printf ("Shortest path from start point (%u, %u) has length %u\n",
  //   beststart.x, beststart.y, shortestpath);
  return shortestpath;
}


int main () {
  printf ("--- Example ---\n");
  Position invalidpoint = { (unsigned short)-1, (unsigned short)-1 };
  unsigned short minsteps = FindShortestPath (invalidpoint,
    strlen (ExampleField[0]), sizeof (ExampleField) / sizeof (ExampleField[0]),
    ExampleField, 2);
  printf ("* Minimum number of steps: %u *\n", minsteps);
  printf ("\n");
  minsteps = FindBestStartingPoint (
    strlen (ExampleField[0]), sizeof (ExampleField) / sizeof (ExampleField[0]),
    ExampleField, 1);
  printf ("* Best starting point has minimum number of steps: %u *\n", minsteps);
  printf ("\n");

  printf ("--- Puzzle 1: Shortest Path ---\n");
  size_t numlines, maxlines;
  maxlines = 50;
  char **inputlines = (char**) calloc (maxlines, sizeof (char*));
  ssize_t numchars = readlines ("12-hill-climbing-algorithm-input.txt",
    &maxlines, &numlines, &inputlines);
  printf ("Lines read: %zd, characters: %zu\n", numlines, numchars);
  TrimLineEndings (numlines, inputlines);
  printf ("Width x Height = %zu x %zu\n", strlen (inputlines[0]), numlines);
  minsteps = FindShortestPath (invalidpoint,
    strlen (inputlines[0]), numlines, (const char**)inputlines, 2);
  printf ("*** Minimum number of steps: %u ***\n", minsteps);
  printf ("\n");

  printf ("--- Puzzle 2: Best starting point ---\n");
  minsteps = FindBestStartingPoint (
    strlen (inputlines[0]), numlines, (const char**)inputlines, 1);
  printf ("*** Best starting point has minimum number of steps: %u ***\n", minsteps);

  for (size_t i = 0; i < numlines; i++)  free (inputlines[i]);
  free (inputlines);
  return 0;
}

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


// Currently not used
//
void PrintPath (unsigned short numsteps, const Position *path) {
  for (unsigned short i = 0; i < numsteps; i++) {
    printf (" (%u,%u)", path[i].x, path[i].y);
    if (i % 10 == 9 || i == numsteps - 1)  printf ("\n");
  }
}


// Currently not used
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
  if (currelev == 'S')  currelev = 'a';  if (currelev == 'E')  currelev = 'z';
  if (destelev == 'S')  destelev = 'a';  if (destelev == 'E')  destelev = 'z';
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


void SetNeighbourValueIfLarger (unsigned short currelev, unsigned short neighelev,
    unsigned short *distanceptr, bool *visitedptr, unsigned short neighx, unsigned short neighy) {
  // TODO
}


/// \brief Find the shortest path to the destination point by Diijkstra's algorithm
//
unsigned short FindShortestPath (Position startpoint,
    unsigned short width, unsigned short height, const char **field) {
  // Visited markers, initialise to false
  bool *visited = (bool*) malloc (width * height * sizeof (bool));
  memset (visited, 0, width * height * sizeof (bool));
  Position currpos;
  if (startpoint.x != (unsigned short)-1 && startpoint.y != (unsigned short)-1)
    currpos = startpoint;
  else
    currpos = FindStartEndPoint (width, height, field, 'S');
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
    const char **terrain) {
  Position beststart;  beststart.x = (unsigned short)-1;  beststart.y = (unsigned short)-1;
  unsigned short shortestpath = (unsigned short)-1;
  // Find all points with elevation a to calculate the minimum path length
  for (unsigned short y = 0; y < height; y++) {
    for (unsigned short x = 0; x < width; x++) {
      if (terrain[y][x] == 'a') {
        Position testpos;  testpos.x = x;  testpos.y = y;
        unsigned short pathlen = FindShortestPath (testpos, width, height, terrain);
        // printf ("Possible starting point at (%u, %u) with path length %u\n",
        //   x, y, pathlen);
        if (pathlen < shortestpath) {
          shortestpath = pathlen;
          beststart = testpos;
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
    ExampleField);
  printf ("* Minimum number of steps: %u *\n", minsteps);
  printf ("\n");
  minsteps = FindBestStartingPoint (
    strlen (ExampleField[0]), sizeof (ExampleField) / sizeof (ExampleField[0]),
    ExampleField);
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
    strlen (inputlines[0]), numlines, (const char**)inputlines);
  printf ("*** Minimum number of steps: %u ***\n", minsteps);
  printf ("\n");

  printf ("--- Puzzle 2: Best starting point ---\n");
  minsteps = FindBestStartingPoint (
    strlen (inputlines[0]), numlines, (const char**)inputlines);
  printf ("*** Best starting point has minimum number of steps: %u ***\n", minsteps);

  for (int i = 0; i < numlines; i++)  free (inputlines[i]);
  free (inputlines);
  return 0;
}

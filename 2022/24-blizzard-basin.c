// Advent of Code 2022 Day 24: Blizzard Basin
// https://adventofcode.com/2022/day/24

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "freadlns.h"
#include "trimendl.h"

/// \brief Maximum number of steps tried
const size_t MaxTimeSteps = 1000;

/// \brief 2-Dimensional position
//
typedef struct Coord2D_st {
  int x;
  int y;
} Coord2D;

/// \brief Bitfield constants for blizzard directions
///
/// No blizzard share the same location and direction, so at every point of
/// the map there can at most be one blizzard in every direction at the same time
//
typedef enum BlizzardDir_enum {
  BlizLeft = 1, BlizRight = 2, BlizUp = 4, BlizDown = 8,
  BlizWall = 16, BlizEmpty = 32, TileReachable = 64
} BlizzardDir;

/// \brief Choose a character depending on how many blizzards share a location
//
char GetBlizzardChar (enum BlizzardDir_enum dirs) {
  if (dirs == BlizEmpty)  return '.';
  if (dirs == BlizWall)  return '#';
  if (dirs == TileReachable)  return '+';
  int numbliz = 0;   // count blizzards
  char oneblizz = '?';   // store a single blizzard character
  if (dirs & BlizLeft) { numbliz++; oneblizz = '<'; }
  if (dirs & BlizRight) { numbliz++; oneblizz = '>'; }
  if (dirs & BlizUp) { numbliz++; oneblizz = '^'; }
  if (dirs & BlizDown) { numbliz++; oneblizz = 'v'; }
  // Return a character for a single blizzard or a number if more than one
  if (numbliz > 1)  return '0' + numbliz;
  else return oneblizz;
}

typedef struct Blizzard_st {
  int px;   ///< x coordinate
  int py;   ///< y coordinate
  int sx;   ///< speed in x direction
  int sy;   ///< speed in y direction
} Blizzard;

/// \brief Start with a blizzard array of this size
#define BLIZZARD_ALLOC_START 1024
/// \brief If reallocation of the blizzard array is necessary, increase
///   memory by this step size
#define BLIZZARD_ALLOC_STEP 256


/// \brief Read all blizzard locations and movement directions from lines
//
void ReadBlizzards (size_t *numbliz, size_t *maxbliz, Blizzard **blizz,
    size_t numlines, const char **lines) {
  *numbliz = 0;
  for (size_t i = 0; i < numlines; i++) {
    for (size_t j = 0; j < strlen (lines[i]); j++) {
      // Translate blizzard characters into speed in x and y direction
      int blx = lines[i][j] == '>' ? 1 : lines[i][j] == '<' ? -1 : 0;
      int bly = lines[i][j] == 'v' ? 1 : lines[i][j] == '^' ? -1 : 0;
      // If a non-zero speed is obtained there is a blizzard here
      if (blx != 0 || bly != 0) {
        // Reallocation necessary?
        if (*numbliz >= *maxbliz) {
          printf ("Info: need to reallocate for more than %zu blizzards\n", *maxbliz);
          Blizzard *newblizz = realloc (*blizz,
            (*maxbliz + BLIZZARD_ALLOC_STEP) * sizeof (Blizzard));
          if (newblizz == NULL) {
            fprintf (stderr, "Failed to reallocate blizzards\n");
            return;
          }
          *maxbliz += BLIZZARD_ALLOC_STEP;
          *blizz = newblizz;
        }
        // Store new blizzard
        (*blizz)[*numbliz].px = j;    (*blizz)[*numbliz].py = i;
        (*blizz)[*numbliz].sx = blx;  (*blizz)[*numbliz].sy = bly;
        (*numbliz)++;
      }
    } // for j
  } // for i
}

/// \brief Move blizzards, wrapping around to the other side when reaching a wall
///
/// Walls are at x = 0, y = 0, x = dimensions.x and y = dimensions.y
//
void MoveBlizzards (size_t numblizzards, Blizzard *blizzards, Coord2D dimensions) {
  for (size_t b = 0; b < numblizzards; b++) {
    blizzards[b].px += blizzards[b].sx;
    if (blizzards[b].px >= dimensions.x)  blizzards[b].px = 1;
    if (blizzards[b].px < 1)  blizzards[b].px = dimensions.x - 1;
    blizzards[b].py += blizzards[b].sy;
    if (blizzards[b].py >= dimensions.y)  blizzards[b].py = 1;
    if (blizzards[b].py < 1)  blizzards[b].py = dimensions.y - 1;
  }
}


/// \brief Create a map with blizzard positions and directions
//
unsigned char **SetUpBlizzardMap (size_t numblizzards, const Blizzard *blizzards,
    Coord2D dimensions, Coord2D entry, Coord2D exit) {
  unsigned char **blizmap =
    (unsigned char**) malloc ((dimensions.y + 1) * sizeof (unsigned char*));
  // Create walls
  for (int i = 0; i <= dimensions.y; i++) {
    blizmap[i] = (unsigned char*) malloc ((dimensions.x + 1) * sizeof (unsigned char));
    for (int j = 0; j <= dimensions.x; j++) {
      if (i == 0 || j == 0 || i == dimensions.y || j == dimensions.x) {
        if ((i == entry.y && j == entry.x) || (i == exit.y && j == exit.x))
          blizmap[i][j] = BlizEmpty;
        else
          blizmap[i][j] = BlizWall;
      }
      else blizmap[i][j] = BlizEmpty;
    }
  }
  // Place blizzards
  for (size_t b = 0; b < numblizzards; b++) {
    const Blizzard *bl = &(blizzards[b]);
    blizmap[bl->py][bl->px] |= bl->sy > 0 ? BlizDown : (bl->sy < 0 ? BlizUp :
      (bl->sx > 0 ? BlizRight : (bl->sx < 0 ? BlizLeft : 0)));
  }
  // Map finished
  return blizmap;
}


/// \brief Create blizzard maps for a number of time steps
//
unsigned char ***SetUpBlizzardMaps (size_t numsteps,
    size_t numblizzards, const Blizzard *blizzards,
    Coord2D dimensions, Coord2D entry, Coord2D exit) {
  unsigned char ***blizmaps =
    (unsigned char***) malloc (numsteps * sizeof (unsigned char**));
  // Copy blizzards to a temporary modifiable array
  Blizzard *bliz = (Blizzard*) malloc (numblizzards * sizeof (Blizzard));
  for (size_t b = 0; b < numblizzards; b++)  bliz[b] = blizzards[b];
  // Calculate blizzard location for given number of time steps
  for (size_t t = 0; t < numsteps; t++) {
    // Create map
    blizmaps[t] = SetUpBlizzardMap (numblizzards, bliz, dimensions, entry, exit);
    // Move blizzards
    MoveBlizzards (numblizzards, bliz, dimensions);
  }
  // Free temporary blizzard array and return map array
  free (bliz);
  return blizmaps;
}

/// \brief Draw map with blizzard positions and directions from tile array
//
void DrawMap (const unsigned char **blmap, Coord2D dimensions) {
  for (int i = 0; i <= dimensions.y; i++) {
    printf ("%4d|", i);
    for (int j = 0; j <= dimensions.x; j++) {
      char c = '?';
      if (blmap[i][j] == BlizWall)  c = '#';
      else if (blmap[i][j] == BlizEmpty)  c = '.';
      else if (blmap[i][j] & TileReachable)  c = '+';
      else {
        c = '0' + ((blmap[i][j] & BlizLeft) > 0 ? 1 : 0) +
          ((blmap[i][j] & BlizRight) > 0 ? 1 : 0) +
          ((blmap[i][j] & BlizUp) > 0 ? 1 : 0) +
          ((blmap[i][j] & BlizDown) > 0 ? 1 : 0);
        c = GetBlizzardChar (blmap[i][j]);
      }
      printf ("%c", c);
    }
    printf ("|\n");
  }
}

/// \brief Draw map with blizzard positions and directions from blizzard array
//
void DrawMapBlizzards (size_t numblizzards, const Blizzard *blizzards,
    Coord2D dimensions, Coord2D entry, Coord2D exit) {
  // Prepare map
  unsigned char **blmap = SetUpBlizzardMap (numblizzards, blizzards,
    dimensions, entry, exit);
  // Draw map
  DrawMap ((const unsigned char**)blmap, dimensions);
  for (int i = 0; i <= dimensions.y; i++)  free (blmap[i]);
  free (blmap);
}


/// \brief Find a path to the destination
///
/// Each step collects all locations reachable from the locations of the
/// previous step (= all previous tiles and their neighbour tiles, provided
/// there is no blizzard in the current step)
//
unsigned int Travel (size_t maxsteps, size_t numblizzards, const Blizzard *blizzards,
    Coord2D dimensions, Coord2D entry, Coord2D exit) {
  // Copy blizzards to a temporary modifiable array
  Blizzard *blizz = (Blizzard*) malloc (numblizzards * sizeof (Blizzard));
  for (size_t b = 0; b < numblizzards; b++)  blizz[b] = blizzards[b];
  // Set up starting blizzard map
  unsigned char **currmap = SetUpBlizzardMap (numblizzards, blizzards,
    dimensions, entry, exit);
  printf ("[Start] Map\n");
  DrawMap ((const unsigned char**)currmap, dimensions);
  // Mark starting tile as only reachable location
  currmap[entry.y][entry.x] = TileReachable;
  // Calculate next step
  size_t timestep;
  for (timestep = 0; timestep < maxsteps; timestep++) {
    // End check: Reached the destination tile?
    if (currmap[exit.y][exit.x] & TileReachable) {
      printf ("[%4zu] Reached the exit\n", timestep);
      break;
    }
    // Move blizzards to set up new map
    MoveBlizzards (numblizzards, blizz, dimensions);
    unsigned char **newmap = SetUpBlizzardMap (numblizzards, blizz,
      dimensions, entry, exit);
    // Mark reachable locations in the map
    for (int y = 0; y <= dimensions.y; y++) {
      for (int x = 0; x <= dimensions.x; x++) {
        if (newmap[y][x] != BlizEmpty)  continue;   // not an empty tile
        // we can stay in this location
        if (currmap[y][x] & TileReachable)  newmap[y][x] |= TileReachable;
        // reachable from left, right, above or below neightbour?
        if ((x > 0) && (currmap[y][x-1] & TileReachable))
          newmap[y][x] |= TileReachable;
        if ((x < dimensions.x) && (currmap[y][x+1] & TileReachable))
          newmap[y][x] |= TileReachable;
        if ((y > 0) && (currmap[y-1][x] & TileReachable))
          newmap[y][x] |= TileReachable;
        if ((y < dimensions.y) && (currmap[y+1][x] & TileReachable))
          newmap[y][x] |= TileReachable;
      }
    }
    // Discard previous map and save new map as current one
    for (int i = 0; i <= dimensions.y; i++)  free (currmap[i]);
    free (currmap);
    currmap = newmap;
#ifdef DEBUG
    printf ("[%4zu] After %zu time steps\n", timestep, timestep + 1);
    DrawMap ((const unsigned char**)currmap, dimensions);
#endif
  }
  // End state
  printf ("[End] After %zu time steps\n", timestep);
  DrawMap ((const unsigned char**)currmap, dimensions);
  // Cleanup
  for (int i = 0; i <= dimensions.y; i++)  free (currmap[i]);
  free (currmap);
  free (blizz);
  // Return number of steps taken
  return timestep;
}


/// \brief Find the way to the exit for the blizzard basin given in the lines
//
unsigned int FindAWay (size_t numlines, const char **lines) {
  size_t maxblizzards = BLIZZARD_ALLOC_START, numblizzards = 0;
  Blizzard *blizzards = (Blizzard*) malloc (maxblizzards * sizeof (Blizzard));
  ReadBlizzards (&numblizzards, &maxblizzards, &blizzards, numlines, lines);
  printf ("Read %zu blizzards (max %zu in array)\n", numblizzards, maxblizzards);
#ifdef DEBUG
  for (size_t i = 0; i < numblizzards; i++)
    printf ("  (%d, %d), speed (%d, %d)\n", blizzards[i].px, blizzards[i].py,
      blizzards[i].sx, blizzards[i].sy);
#endif
  // Determine map dimensions (= maximum x and y positions)
  // Subtract trailing wall '#', for example:
  // a line "#....#" describes positions 0 ... 5 and has dimension.x = 5
  Coord2D mapdim = { strlen (lines[0]) - 1, numlines - 1 };
  // Find start location in the top row
  Coord2D entrypt = { 1, 0 };
  char *c = strchr (lines[0], '.');
  if (c != NULL)  entrypt.x = c - lines[0];
  else 
    fprintf (stderr, "Warning: Fallback to start position (1, 0) as the "
      "real one could not be found in: %s\n", lines[0]);
  // Find destination in the bottom row
  Coord2D exitpt = { mapdim.x - 1, mapdim.y };
  c = strchr (lines[numlines-1], '.');
  if (c != NULL)  exitpt.x = c - lines[numlines-1];
  else 
    fprintf (stderr, "Warning: Fallback to start position (%d, %d) as the "
      "real one could not be found in: %s\n",
      exitpt.x, exitpt.y, lines[numlines-1]);
  printf ("- Start position (%d, %d), end position (%d, %d)\n",
    entrypt.x, entrypt.y, exitpt.x, exitpt.y);
  // Find the way
  printf ("- Dimensions: (%d, %d)\n", mapdim.x, mapdim.y);
  unsigned int needsteps =
    Travel (MaxTimeSteps, numblizzards, blizzards, mapdim, entrypt, exitpt);
  // Cleanup
  free (blizzards);
  return needsteps;
}


const char *smallexample[] = {
  "#.#####",
  "#.....#",
  "#>....#",
  "#.....#",
  "#...v.#",
  "#.....#",
  "#####.#"
};
const size_t numlinessmallex =
  sizeof (smallexample) / sizeof (smallexample[0]);

const char *complexexample[] = {
  "#.######",
  "#>>.<^<#",
  "#.<..<<#",
  "#>v.><>#",
  "#<^v^^>#",
  "######.#"
};
const size_t numlinescomplexex =
  sizeof (complexexample) / sizeof (complexexample[0]);

int main () {
  printf ("--- Small Example ---\n");
  // - Debug:  Draw map from blizzard array
  // DrawMapBlizzards (numblizzards, blizzards, mapdim, entrypt, exitpt);
  // - Debug:  Precalculate maps for time steps
  // const size_t timesteps = 6;
  // unsigned char ***maps = SetUpBlizzardMaps (timesteps, numblizzards, blizzards,
  //   mapdim, entrypt, exitpt);
  // for (size_t i = 0; i < timesteps; i++) {
  //   printf ("Time step %zu\n", i);
  //   DrawMap ((const unsigned char**)maps[i], mapdim);
  // }
  unsigned int needtimesteps = FindAWay (numlinessmallex, smallexample);
  printf ("* Needed time steps to reach the exit: %u *\n", needtimesteps);
  printf ("\n");

  printf ("--- Complex Example ---\n");
  needtimesteps = FindAWay (numlinescomplexex, complexexample);
  printf ("* Needed time steps to reach the exit: %u *\n", needtimesteps);
  printf ("\n");

  printf ("--- Puzzle 1: Time needed to reach the exit ---\n");
  size_t maxlines = 200, numlines = 0;
  char **inputlines = (char**) malloc (maxlines * sizeof (char*));
  ssize_t numchars = readlines ("24-blizzard-basin-input.txt",
    &maxlines, &numlines, &inputlines);
  TrimLineEndings (numlines, inputlines);
  printf ("Read %zu lines, %zd characters\n", numlines, numchars);
  needtimesteps = FindAWay (numlines, (const char**)inputlines);
  printf ("*** Needed time steps to reach the exit: %u ***\n", needtimesteps);
  for (size_t i = 0; i < numlines; i++)  free (inputlines[i]);
  free (inputlines);
  return 0;
}

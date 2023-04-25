// Advent of Code 2022 Day 14 Regolith Reservoir
// https://adventofcode.com/2022/day/14

#include <ctype.h>
#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "freadlns.h"

#ifdef __GNUC__
#define UNUSED __attribute__((__unused__))
#endif

typedef struct Position_struct {
  int x;
  int y;
} Position;


typedef struct Rock_st {
  size_t maxpos;
  size_t numpos;
  Position *pos;
} Rock;


// steps to increase storage of Position arrays
#define REALLOC_POSITIONS_CHUNKSIZE 10


/// \brief Parse a line describing a rock structure
//
void ParseRockStructure (const char *line, Rock *rock) {
  const char *c = line;
  const char *e = line + strlen (line);
  rock->numpos = 0;
  while (c < e) {
    // Expect two integers separated by comma
    int xcoord;
    int ycoord;
    int numchars;
    int numscanned = sscanf (c, " %d , %d%n", &xcoord, &ycoord, &numchars);
    if (numscanned < 2)  return;
    // Store coordinate
    if (rock->numpos >= rock->maxpos) {
      // reallocate space for more positions
      rock->maxpos += REALLOC_POSITIONS_CHUNKSIZE;
      rock->pos = realloc (rock->pos, rock->maxpos * sizeof (Position));
      if (rock->pos == NULL)  return;
    }
    rock->pos[rock->numpos].x = xcoord;
    rock->pos[rock->numpos].y = ycoord;
    (rock->numpos) ++;
    c += numchars;   // advance cursor
    // Look out for spaces and an arrow
    while (isspace (*c))  c++;
    if (*c == '-' && *(c+1) == '>')  c += 2;
  }
}


/// \brief Parse lines describing rock structures
//
void ParseRockStructures (size_t numlines, const char **lines,
    size_t *maxrocks, size_t *numrocks, Rock **rocks) {
  *numrocks = 0;
  for (size_t i = 0; i < numlines; i++) {
    // Rock *rock = (Rock*) malloc (*numrocks * sizeof (Rock));
    Rock r;
    r.maxpos = 10;
    r.numpos = 0;
    // rock->maxpos = 10;
    // rock->pos = (Position*) malloc (rock->maxpos * sizeof (Position*));
    r.pos = (Position*) malloc (r.maxpos * sizeof (Position*));
    // rock->numpos = 0;
    ParseRockStructure (lines[i], &r);
    // reallocate if necessary
    if (*numrocks >= *maxrocks) {
      (*maxrocks) += 20;
      *rocks = realloc (*rocks, *maxrocks * sizeof (Rock));
      if (*rocks == NULL)  return;
    }
    // Store the newly parsed rock
    (*rocks)[*numrocks].numpos = r.numpos;
    (*rocks)[*numrocks].maxpos = r.maxpos;
    (*rocks)[*numrocks].pos = r.pos;
    (*numrocks) ++;
  }
}


/// \brief Print one rock structure
//
void PrintRock (const Rock rock) {
  printf ("Rock: %zd positions: ", rock.numpos);
  for (size_t i = 0; i < rock.numpos; i++)  printf (" (%d, %d)",
    rock.pos[i].x, rock.pos[i].y);
  printf ("\n");
}


/// \brief Print the rock structures
//
void PrintRocks (size_t numrocks, const Rock *rocks) {
  for (size_t i = 0; i < numrocks; i++) {
    printf ("%4zd ", i + 1);
    PrintRock (rocks[i]);
  }
}


/// \brief Create a cave map from the list of rock structures
//
void CreateCaveMap (size_t numrocks, const Rock *rock,
    int *left, int *right, int *top, int *bottom, char ***cave, int sandx, int sandy UNUSED) {
  // Determine dimensions
  *left = INT_MAX;  *right = INT_MIN;  *top = INT_MAX;  *bottom = INT_MIN;
  for (size_t i = 0; i < numrocks; i++) {
    for (size_t j = 0; j < rock[i].numpos; j++) {
      if (*left > rock[i].pos[j].x)  *left = rock[i].pos[j].x;
      if (*right < rock[i].pos[j].x)  *right = rock[i].pos[j].x;
      if (*top > rock[i].pos[j].y)  *top = rock[i].pos[j].y;
      if (*bottom < rock[i].pos[j].y)  *bottom = rock[i].pos[j].y;
    }
  }
  // top needs to include the row where sand enters
  if (*top > 0)  *top = 0;
  // Extend cave to the left and right to be able to fill it up
  // to the sand entry point:  At least the distance from top to bottom
  // is also needed to the left and right, plus two more units
  // for the additional floor structure below
  if (sandx - *left < *bottom - *top + 1)  *left = sandx - (*bottom - *top + 3);
  if (*right - sandx < *bottom - *top + 1)  *right = sandx + (*bottom - *top + 3);
  (*bottom) += 2;
  // Allocate a cave and fill it with air
  *cave = (char**) malloc ((*bottom - *top + 1) * sizeof (char*));
  if (*cave == NULL)  return;
  for (int y = *top; y <= *bottom; y++) {
    (*cave)[y - *top] = (char*) malloc ((*right - *left + 1) * sizeof (char));
    if ((*cave)[y - *top] == NULL)  return;
    for (int x = *left; x <= *right; x++) {
      (*cave)[y - *top][x - *left] = '.';
    }
  }
  // Build rock structures
  for (size_t i = 0; i < numrocks; i++) {
    for (size_t j = 0; j < rock[i].numpos - 1; j++) {
      if (rock[i].pos[j].y == rock[i].pos[j+1].y) {
        // Horizontal edge, left to right or right to left
        int startx, endx;
        if (rock[i].pos[j].x < rock[i].pos[j+1].x) {
          startx = rock[i].pos[j].x;  endx = rock[i].pos[j+1].x;
        } else {
          startx = rock[i].pos[j+1].x;  endx = rock[i].pos[j].x;
        }
        for (int k = startx; k <= endx; k++) {
          (*cave)[rock[i].pos[j].y - *top][k - *left] = '#';
        }
      }
      if (rock[i].pos[j].x == rock[i].pos[j+1].x) {
        // Vertical edge, down or up
        int starty, endy;
        if (rock[i].pos[j].y < rock[i].pos[j+1].y) {
          starty = rock[i].pos[j].y;  endy = rock[i].pos[j+1].y;
        } else {
          starty = rock[i].pos[j+1].y;  endy = rock[i].pos[j].y;
        }
        for (int k = starty; k <= endy; k++) {
          (*cave)[k - *top][rock[i].pos[j].x - *left] = '#';
        }
      }
    }
  }
  // Add the long floor structure below
  for (int x = 0; x < *right - *left + 1; x++) {
    (*cave)[*bottom - *top][x] = '#';
  }
  // Find egdes into nowhere: Test every x from bottom up
  for (int tx = 0; tx <= *right - *left; tx++) {
    for (int ty = *bottom - *top; ty >= 0; ty--) {
      // Replace "air" '.' with "abyss" '~' to mark spots that lead to nowhere
      if ((*cave)[ty][tx] == '.')  (*cave)[ty][tx] = '~';
      else break;
    }
  }
}


/// \brief Visualise the cave
//
void PrintCave (int left, int right, int top, int bottom, const char **cave) {
  for (int y = top; y <= bottom; y++) {
    printf ("%4d|", y);
    for (int x = left; x <= right; x++) {
      printf ("%c", cave[y-top][x-left]);
    }
    printf ("\n");
  }
}


/// \brief Test whether the specified position is still empty
//
bool IsFreePos (int left, int right, int top, int bottom, const char **map,
    int testx, int testy) {
  // A position outside the map counts as "free"
  if (testx < left || testx > right || testy < top || testy > bottom)  return true;
  // Positions inside the grid have to be tested
  if (map[testy - top][testx - left] == '#')  return false;
  if (map[testy - top][testx - left] == 'o')  return false;
  if (map[testy - top][testx - left] == '.')  return true;
  if (map[testy - top][testx - left] == '~')  return true;
  return true;
}


/// \brief Let sand trickle into the cave at the specified position
/// \param left Left edge of cave dimensions
/// \param right Right edge of cave dimensions
/// \param top Upper edge of cave dimensions
/// \param bottom Lower edge of cave dimensions
/// \param cave Map of the cave
/// \param sandx Sand start position, x coordinate
/// \param sandy Sand start position, y coordinate
/// \param numtofill On output: Number of sand units until cave is filled
/// \return Number of sand units (grains of sand) resting in the cave structures above ground
//
unsigned long int PourSand (int left, int right, int top, int bottom, char **cave,
    int sandx, int sandy, unsigned long *numtofill) {
  unsigned long int numsand = 0, numrest = 0;
  // unsigned long int numtimesteps = 0;
  bool trickle = true;
  while (trickle) {
    // Draw "current frame"
    // PrintCave (left, right, top, bottom, (const char**)cave);
    // Put a grain of sand at the start position
    int sx = sandx;  int sy = sandy;
    // Abort if sand is stacked to the top
    if (cave[sandy - top][sandx - left] == 'o') {
      printf ("Stopping because entry at (%d, %d) is blocked by a grain of sand\n", sandx, sandy);
      break;
    }
    // Let it fall down
    // TODO Should be cleaned up and use the function IsFreePos()
    //   to simplify all the stacked tests below
    bool freefall = true;
    while (freefall) {
      // Reached the level two above the floor for the first time,
      // i.e. no longer inside the structures above;
      // save this as the number of resting sand units and visualise the cave
      if (numrest == 0 && sy >= bottom - 2) {
        numrest = numsand;
        PrintCave (left, right, top, bottom, (const char**)cave);
      }
      // Still inside the cave grid and at least one row below?
      if (sy > bottom - 1 || sx < left || sx > right) {
        trickle = false;  break;
      }
      // Only the abyss below?
      if (sy >= top && cave[sy - top][sx - left] == '~') {
        trickle = false;  break;
      }
      // Free position directly below? ...
      if (sy < top - 1 || cave[sy - top + 1][sx - left] == '.') {
        sy++;  /* numtimesteps++; */  continue;
      }
      // ... or below left?
      if (sx > left) {
        if (sy < top - 1 || cave[sy - top + 1][sx - left - 1] == '.') {
          sy++;  sx--;  /* numtimesteps++; */  continue;
        } else if (sy >= top - 1 && cave[sy - top + 1][sx - left - 1] == '~') {
          trickle = false;  break;
        }
      } else {   // Fallen outside to the left
        trickle = false;  break;
      }
      // ... or below right?
      if (sx < right) {
        if (sy < top - 1 || cave[sy - top + 1][sx -left + 1] == '.') {
          sy++;  sx++;  /* numtimesteps++; */  continue;
        } else if (sy >= top - 1 && cave[sy - top + 1][sx - left + 1] == '~') {
          trickle = false;  break;
        }
      } else {   // Fall outside to the right
        trickle = false;  break;
      }
      // printf ("No free position found after %d (%d..%d), %d, row below: \"%.*s\"\n", sx, left, right, sy, right - left + 1, cave[sy - top + 1]);
      // Grain of sand has come to rest
      cave[sy - top][sx - left] = 'o';
      numsand++;   // Count grain but do not increase numtimesteps
      freefall = false;
    }
  }
  *numtofill = numsand;
  return numrest;
}


/// \brief Build a cave from the rock description lines and fill it with sand
/// \param numlines Number of lines to consider for rock descriptions
/// \param lines Lines with rock descriptions
/// \param numtofill On output: Number of sand units until cave is filled
/// \return Number of sand units (grains of sand) resting in the cave at the end
//
unsigned long LetItSand (size_t numlines, const char **lines, unsigned long *numtofill) {
  // Read rock structures
  size_t maxrocks = 100, numrocks = 0;
  Rock *rocks = (Rock*) malloc (maxrocks * sizeof (Rock));
  ParseRockStructures (numlines, lines, &maxrocks, &numrocks, &rocks);
  printf ("%zd rocks (max %zd)\n", numrocks, maxrocks);
  // PrintRocks (numrocks, rocks);
  // Convert rock structures to a cave map
  int cavel, caver, cavet, caveb;
  char **cavemap = NULL;
  CreateCaveMap (numrocks, rocks, &cavel, &caver, &cavet, &caveb, &cavemap, 500, 0);
  printf ("Cave: %d ... %d x %d ... %d\n", cavel, caver, cavet, caveb);
  // PrintCave (cavel, caver, cavet, caveb, (const char**)cavemap);
  // Let the sand trickle down
  // TODO free positions
  unsigned long int numsand = PourSand (cavel, caver, cavet, caveb, cavemap, 500, 0, numtofill);
  PrintCave (cavel, caver, cavet, caveb, (const char**)cavemap);
  // Cleanup and return
  for (size_t i = 0; i < numrocks; i++)  free (rocks[i].pos);
  free (rocks);
  for (int y = 0; y < caveb - cavet + 1; y++)  free (cavemap[y]);
  free (cavemap);
  return numsand;
}


const char *examplelines[] = {
  "498,4 -> 498,6 -> 496,6",
  "503,4 -> 502,4 -> 502,9 -> 494,9"
};


int main () {
  printf ("--- Example ---\n");
  unsigned long int numtillfull;
  unsigned long int numsand = LetItSand (sizeof (examplelines) / sizeof (examplelines[0]), examplelines, &numtillfull);
  printf ("* Number of sand units resting in the cave: %lu *\n", numsand);
  printf ("* Number of sand units in the cave at the end: %lu *\n", numtillfull);
  printf ("\n");

  printf ("--- Puzzle 1: Number of sand units in the structures ---\n");
  size_t maxlines = 200, numlines = 0;
  char **lines = (char**) malloc (maxlines * sizeof (char*));
  readlines ("14-regolith-reservoir-input.txt", &maxlines, &numlines, &lines);
  printf ("%zd lines read\n", numlines);
  numsand = LetItSand (numlines, (const char**)lines, &numtillfull);
  printf ("*** Number of sand units resting in the cave: %lu ***\n", numsand);
  printf ("\n");

  printf ("--- Puzzle 2: Total number of sand units in the cave ---\n");
  printf ("*** Number of sand units in the cave at the end: %lu ***\n", numtillfull);
  for (size_t i = 0; i < numlines; i++)  free (lines[i]);
  free (lines);
}

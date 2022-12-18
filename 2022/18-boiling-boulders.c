/// Advent of Code 2022 Day 18 Boiling Boulders
/// https://adventofcode.com/2022/day/18

#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "freadlns.h"

const char* examplelines[] = {
  "2,2,2", "1,2,2", "3,2,2", "2,1,2",
  "2,3,2", "2,2,1", "2,2,3", "2,2,4",
  "2,2,6", "1,2,5", "3,2,5", "2,1,5",
  "2,3,5"
};


typedef struct Droplet_st {
  int px;   ///< Position, x coordinate
  int py;   ///< Position, y coordinate
  int pz;   ///< Position, z coordinate
  size_t nx;   ///< Number of cubes in x direction
  size_t ny;   ///< Number of cubes in y direction
  size_t nz;   ///< Number of cubes in z direction
  char *voxels;
} Droplet;

char GetVoxel (const Droplet *droplet, int x, int y, int z) {
  if (x < droplet->px || x >= droplet->px + (int)droplet->nx)  return ' ';
  if (y < droplet->py || y >= droplet->py + (int)droplet->ny)  return ' ';
  if (z < droplet->pz || z >= droplet->pz + (int)droplet->nz)  return ' ';
  return droplet->voxels[((z - droplet->pz) * droplet->ny + (y - droplet->py)) *
    droplet->nx + (x - droplet->px)];
}
void SetVoxel (Droplet *droplet, int x, int y, int z, char value) {
  if (x < droplet->px || x >= droplet->px + (int)droplet->nx)  return;
  if (y < droplet->py || y >= droplet->py + (int)droplet->ny)  return;
  if (z < droplet->pz || z >= droplet->pz + (int)droplet->nz)  return;
  droplet->voxels[((z - droplet->pz) * droplet->ny + (y - droplet->py)) *
    droplet->nx + (x - droplet->px)] = value;
}

/// \brief Read a droplet from its voxel locations
/// \return Volume of the droplet
//
unsigned long ParseDroplet (Droplet *droplet,
    const size_t nlines, const char **lines) {
  // Read all individual cubes/voxels first and remember min/max coordinates
  int xmin = INT_MAX, xmax = INT_MIN;
  int ymin = INT_MAX, ymax = INT_MIN;
  int zmin = INT_MAX, zmax = INT_MIN;
  int *coords = (int*) malloc (nlines * 3 * sizeof (int));
  for (size_t i = 0; i < nlines; i++) {
    int numconv = sscanf (lines[i], " %d , %d , %d",
      &(coords[i*3]), &(coords[i*3+1]), &(coords[i*3+2]));
    if (numconv != 3) {
      fprintf (stderr, "Error: Line %zu: Converted %d values instead of 3\n",
        i, numconv);
    }
    if (coords[i*3] < xmin)  xmin = coords[i*3];
    if (coords[i*3] > xmax)  xmax = coords[i*3];
    if (coords[i*3+1] < ymin)  ymin = coords[i*3+1];
    if (coords[i*3+1] > ymax)  ymax = coords[i*3+1];
    if (coords[i*3+2] < zmin)  zmin = coords[i*3+2];
    if (coords[i*3+2] > zmax)  zmax = coords[i*3+2];
  }
  printf ("Droplet dimensions: (%d, %d, %d) ... (%d, %d, %d)\n",
    xmin, ymin, zmin, xmax, ymax, zmax);
  // Now convert to a 3-dimensional array of member voxels
  droplet->px = xmin;  droplet->nx = xmax - xmin + 1;
  droplet->py = ymin;  droplet->ny = ymax - ymin + 1;
  droplet->pz = zmin;  droplet->nz = zmax - zmin + 1;
  droplet->voxels =
    (char*) malloc (droplet->nx * droplet->ny * droplet->nz * sizeof (char));
  memset (droplet->voxels, '.',
    droplet->nx * droplet->ny * droplet->nz * sizeof (char));
  for (size_t i = 0; i < nlines; i++) {
    SetVoxel (droplet, coords[3*i], coords[3*i+1], coords[3*i+2], '*');
    // printf ("Cube %4zu:  (%d, %d, %d)\n",
    //   i, coords[3*i], coords[3*i+1], coords[3*i+2]);
  }
  return droplet->nx * droplet->ny * droplet->nz;
}


/// \brief Calculate the surface area of a droplet
/// \param The droplet to calculate the surface area for
/// \param includeinner  Whether to include inner volume elemnts (true) or only
///   surface accessible from the outside (false)
//
unsigned long DropletSurface (const Droplet *droplet, bool includeinner) {
  long surf = 0;
  for (int z = droplet->pz; z < droplet->pz + (int)droplet->nz; z++)
    for (int y = droplet->py; y < droplet->py + (int)droplet->ny; y++)
      for (int x = droplet->px; x < droplet->px + (int)droplet->nx; x++) {
        if (GetVoxel (droplet, x, y, z) != '*')  continue;   // Not part of droplet
        int n = 0;
        // Check all six neighbours
        if (includeinner) {
          // Accept a neighbour which is not a part of the droplet
          if (GetVoxel (droplet, x-1, y  , z  ) != '*')  n++;
          if (GetVoxel (droplet, x+1, y  , z  ) != '*')  n++;
          if (GetVoxel (droplet, x  , y-1, z  ) != '*')  n++;
          if (GetVoxel (droplet, x  , y+1, z  ) != '*')  n++;
          if (GetVoxel (droplet, x  , y  , z-1) != '*')  n++;
          if (GetVoxel (droplet, x  , y  , z+1) != '*')  n++;
        }
        else {
          // Accept only neighbour which is "outside air"
          if (GetVoxel (droplet, x-1, y  , z  ) == ' ')  n++;
          if (GetVoxel (droplet, x+1, y  , z  ) == ' ')  n++;
          if (GetVoxel (droplet, x  , y-1, z  ) == ' ')  n++;
          if (GetVoxel (droplet, x  , y+1, z  ) == ' ')  n++;
          if (GetVoxel (droplet, x  , y  , z-1) == ' ')  n++;
          if (GetVoxel (droplet, x  , y  , z+1) == ' ')  n++;
        }
        // printf ("Cube (%d, %d, %d) has %d uncovered faces\n", x, y, z, n);
        surf += n;
      }
  return surf;
}


/// \brief Identify accessible and inaccessible air cubes
///
/// "Propagation of accessible volume elements to the inside":
///
/// After parsing, cubes part of the droplet are marked by '*',
/// cubes not part of the droplet ("air") are marked by '.' .
/// Now mark all air cubes accessible from the outside with ' ' (space).
/// Cubes that remain '.' are enclosed air cubes and not accessible.
///
/// \return Number of enclosed inner volume elements
//
unsigned long CloseDroplet (Droplet *droplet) {
  // Surround the droplet with a layer of air and loop over all droplet
  // volume elements to identify elements connected to outside air
  // until no new volume elements could be identified in a loop
  bool foundcube = true;
  size_t runs = 0;   // Count runs necessary
  while (foundcube && runs < droplet->nz * droplet->ny * droplet->nx) {
    foundcube = false;
    for (int z = droplet->pz; z < droplet->pz + (int)droplet->nz; z++) {
      for (int y = droplet->py; y < droplet->py + (int)droplet->ny; y++) {
        for (int x = droplet->px; x < droplet->px + (int)droplet->nx; x++) {
          // Only consider air volume elements
          if (GetVoxel (droplet, x, y, z) != '.')  continue;
          // If this volume element is at the outside of the droplet, it is accessible
          if (z == droplet->pz || z == droplet->pz + (int)droplet->nz - 1 ||
              y == droplet->py || y == droplet->py + (int)droplet->ny - 1 ||
              x == droplet->px || y == droplet->px + (int)droplet->nx - 1) {
            SetVoxel (droplet, x, y, z, ' ');
            foundcube = true;
            continue;
          }
          // If this volume element has an air neighbour, mark it as accessible as well
          if (GetVoxel (droplet, x-1, y  , z  ) == ' ' ||
              GetVoxel (droplet, x+1, y  , z  ) == ' ' ||
              GetVoxel (droplet, x  , y-1, z  ) == ' ' ||
              GetVoxel (droplet, x  , y+1, z  ) == ' ' ||
              GetVoxel (droplet, x  , y  , z-1) == ' ' ||
              GetVoxel (droplet, x  , y  , z+1) == ' ') {
            SetVoxel (droplet, x, y, z, ' ');
            foundcube = true;
            continue;
          }
    } } } // for x, y, z
    runs++;
  } // while foundcube
  printf ("Needed %zu runs to close the droplet\n", runs);
  // Count inner volume elements
  long unsigned inner = 0;
  for (int z = droplet->pz; z < droplet->pz + (int)droplet->nz; z++) {
    for (int y = droplet->py; y < droplet->py + (int)droplet->ny; y++) {
      for (int x = droplet->px; x < droplet->px + (int)droplet->nx; x++) {
        // Only consider air volume elements
        if (GetVoxel (droplet, x, y, z) == '.') {
          // printf ("Inner cube (%d, %d, %d)\n", x, y, z);
          inner++;
        }
  } } }
  return inner;
}

/// \brief Output droplet xy layers for the specified z range
///
/// Note: z values outside the droplet are not an error
///   (but will be output as empty layers)
//
void PrintDropletLayers (const Droplet *droplet, int zmin, int zmax) {
  for (int z = zmin; z <= zmax; z++) {
    printf ("z = %d\n", z);
    for (int y = droplet->py + (int)droplet->ny - 1; y >= droplet->py; y--) {
      printf ("%4d|", y);
      for (int x = droplet->px; x < droplet->px + (int)droplet->nx; x++) {
        printf ("%c", GetVoxel (droplet, x, y, z));
      }
      printf ("|\n");
    }
  }
}


int main () {
  printf ("--- Example ---\n");
  Droplet droplet;
  unsigned long dropvol = ParseDroplet (&droplet,
    sizeof (examplelines) / sizeof (examplelines[0]), examplelines);
  printf ("Read a droplet with volume %lu (%zu x %zu x %zu)\n",
    dropvol, droplet.nx, droplet.ny, droplet.nz);
  unsigned long dropsurface = DropletSurface (&droplet, true);
  printf ("* Surface area: %lu *\n", dropsurface);
  printf ("\n");

  printf ("--- Example without inner volume elements ---\n");
  unsigned long innercubes = CloseDroplet (&droplet);
  PrintDropletLayers (&droplet, droplet.pz - 1, droplet.pz + droplet.nz);
  printf ("Number of inaccessible volume elements: %lu\n", innercubes);
  dropsurface = DropletSurface (&droplet, false);
  printf ("* Surface area: %lu *\n", dropsurface);
  free (droplet.voxels);
  printf ("\n");

  printf ("--- Puzzle 1: Surface area of the droplet ---\n");
  size_t maxlines = 0, numlines;
  char **inputlines = (char**) calloc (maxlines, sizeof (char*));
  ssize_t numchars = readlines ("18-boiling-boulders-input.txt",
    &maxlines, &numlines, &inputlines);
  printf ("Number of lines read: %zu, characters: %zd\n", numlines, numchars);
  dropvol = ParseDroplet (&droplet, numlines, (const char**)inputlines);
  printf ("Read a droplet with volume %lu (%zu x %zu x %zu)\n",
    dropvol, droplet.nx, droplet.ny, droplet.nz);
  dropsurface = DropletSurface (&droplet, true);
  printf ("*** Surface area: %lu ***\n", dropsurface);
  printf ("\n");

  printf ("--- Puzzle 2: Surface area without counting inner volume elements ---\n");
  innercubes = CloseDroplet (&droplet);
  PrintDropletLayers (&droplet, droplet.pz, droplet.pz + droplet.nz - 1);
  printf ("Number of inaccessible volume elements: %lu\n", innercubes);
  dropsurface = DropletSurface (&droplet, false);
  printf ("* Surface area: %lu *\n", dropsurface);
  free (droplet.voxels);
  for (size_t i = 0; i < numlines; i++)   free (inputlines[i]);
  free (inputlines);
}

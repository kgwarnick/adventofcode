// Advent of Code 2022 Day 4: Camp Cleanup
// https://adventofcode.com/2022/day/4

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include "freadlns.h"


/// \brief read section start and end points for two ranges "start1-end1,start2-end2"
/// \return true on success, false on error
//
bool parsesections (const char *line, int *start1, int *end1, int *start2, int *end2) {
  char *next;
  // Read start of first section
  *start1 = (int) strtoul (line, &next, 10);
  // Skip hyphen -
  if (*next == '-') next++; else return false;
  // Read end of first section
  *end1 = (int) strtoul (next, &next, 10);
  // Skip comma ,
  if (*next == ',') next++; else return false;
  // Read start of second section
  *start2 = (int) strtoul (next, &next, 10);
  // Skip hyphen -
  if (*next == '-') next++; else return false;
  // Read end of second section
  *end2 = (int) strtoul (next, &next, 10);
  // Everything read
  return true;
}


/// \brief Check whether one section if fully contained in the other
//
bool IsOneSectionContainedInTheOther (int start1, int end1, int start2, int end2) {
  return ((start1 >= start2 && end1 <= end2) || (start2 >= start1 && end2 <= end1));
}


/// \brief Check whether the two sections overlap
//
bool IsSectionPairWithOverlap (int start1, int end1, int start2, int end2) {
  return ((end1 >= start2 && start1 <= end2) || (end2 >= start1 && start2 <= end1));
}


/// \brief Count in how many range pairs one is fully contained in the other
//
int NumberOfFullyContainedRanges (int numlines, char **lines, int loglevel) {
  int numcontained = 0;
  for (int i = 0; i < numlines; i++) {
    int a1, a2, b1, b2;
    bool parsesuccess = parsesections (lines[i], &a1, &a2, &b1, &b2);
    bool onecontained = IsOneSectionContainedInTheOther (a1, a2, b1, b2);
    if (onecontained) numcontained++;
    if (loglevel >= 1)
      printf ("%3d  %s  ->  %s  %3d ... %3d ,  %3d ... %d  %s\n", i, lines[i],
        parsesuccess ? " PARSEOK  " : "PARSEERROR", a1, a2, b1, b2,
        onecontained ? "[CONT]" : "[----]");
  }
  return numcontained;
}


/// \brief Count in how many range pairs the two ranges overlap
//
int NumberOfOverlappingRanges (int numlines, char **lines, int loglevel) {
  int numoverlap = 0;
  for (int i = 0; i < numlines; i++) {
    int a1, a2, b1, b2;
    bool parsesuccess = parsesections (lines[i], &a1, &a2, &b1, &b2);
    bool overlap = IsSectionPairWithOverlap (a1, a2, b1, b2);
    if (overlap) numoverlap++;
    if (loglevel >= 1)
      printf ("%3d  %s  ->  %s  %3d ... %3d ,  %3d ... %d  %s\n", i, lines[i],
        parsesuccess ? " PARSEOK  " : "PARSEERROR", a1, a2, b1, b2,
        overlap ? "[OVER]" : "[----]");
  }
  return numoverlap;
}


int main () {

  printf ("--- Examples ---\n");
  char *examples[] = {
    "2-4,6-8",  "2-3,4-5",  "5-7,7-9",  "2-8,3-7",  "6-6,4-6",  "2-6,4-8"
  };
  int numcontained = NumberOfFullyContainedRanges (
    sizeof (examples) / sizeof (examples[0]), examples, 1);
  printf ("Number of assignment pairs with "
    "one range fully contained in the other: %d\n", numcontained);
  int numoverlap = NumberOfOverlappingRanges (
    sizeof (examples) / sizeof (examples[0]), examples, 1);
  printf ("Number of assignment pairs with overlap: %d\n", numoverlap);
  printf ("\n");

  printf ("--- Puzzle 1: Fully contained ranges ---\n"); 
  size_t maxlines = 1000;
  size_t numlines = 0;
  char **lines = (char**) calloc (maxlines, sizeof (char*));
  ssize_t numchars = readlines ("04-camp-cleanup-input.txt", &maxlines, &numlines, &lines);
  printf ("Lines read: %zu (array size %zu), characters: %zd\n", numlines, maxlines, numchars);
  numcontained = NumberOfFullyContainedRanges (numlines, lines, 0);
  printf ("Number of assignment pairs with "
    "one range fully contained in the other: %d\n", numcontained);
  printf ("\n");

  printf ("--- Puzzle 2: Overlapping ranges ---\n");
  numoverlap = NumberOfOverlappingRanges (numlines, lines, 0);
  printf ("Number of assignment pairs with overlap: %d\n", numoverlap);
  for (size_t i = 0; i < numlines; i++)  free (lines[i]);
  free (lines);

  return 0;
}

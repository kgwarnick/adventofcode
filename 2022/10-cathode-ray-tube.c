// Advent of Code 2022 Day 10 Cathode-Ray Tube
// https://adventofcode.com/2022/day/10


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "freadlns.h"


const char *shortexample[] = { "noop", "addx 3", "addx -5" };


/// \brief Run the program and return the sum of signal strengths
//
long int RunProgram (size_t numlines, const char **lines, int loglevel) {
  long int cycle = 0;   // cycle counter
  long int regx = 1;    // register x value
  long int sigstrength = 0;   // sum of signal strengths
  for (int i = 0; i < numlines; i++) {
    int waittime = 1;   // how many cycles to wait before setting x
    long int pendingx = regx;   // value for x at the end of the cycle
    // Read instruction
    if (strncmp ("addx ", lines[i], 5) == 0) {
      waittime = 2;
      long incr = strtol (lines[i] + 5, NULL, 10);
      if (loglevel >= 2)
        printf ("%4ld  addx %3ld  %4ld\n", cycle + 1, incr, regx);
      pendingx = regx + strtol (lines[i] + 5, NULL, 10);
    }
    else if (strncmp ("noop", lines[i], 4) == 0) {
      if (loglevel >= 2)
        printf ("%4ld  noop      %4ld\n", cycle + 1, regx);
    }
    // Run instruction
    // - wait necessary cycles
    for (int t = 0; t < waittime; t++) {
      // Take samples during cycles 20, 60, 100, 140, ...
      if ((cycle + 20) % 40 == 39) {
        if (loglevel >= 1)
          printf ("%4ld   -->      %4ld  --> %ld\n", cycle + 1, regx,
            (cycle + 1) * regx);
        sigstrength += (cycle + 1) * regx;
      }
      cycle++;
    }
    // - set new register value
    regx = pendingx;
  }
  if (loglevel >= 2)
    printf ("%4ld  ----      %4ld\n", cycle + 1, regx);
  return sigstrength;
}


int main () {
  printf ("--- Example ---\n");
  size_t maxlines = 10;
  char **inputlines = (char**) calloc (maxlines, sizeof (char*));
  size_t numlines;
  ssize_t numchars = readlines ("10-cathode-ray-tube-example.txt",
    &maxlines, &numlines, &inputlines);
  printf ("Lines read: %zd, characters: %zu\n", numlines, numchars);
  long int sigstrsum = RunProgram (numlines, (const char**)inputlines, 1);
  printf ("Signal strength sum: %ld\n", sigstrsum);
  for (int i = 0; i < numlines; i++)  free (inputlines[i]);
  printf ("\n");

  printf ("--- Puzzle 1: Sum of signal strengths ---\n");
  numchars = readlines ("10-cathode-ray-tube-input.txt",
    &maxlines, &numlines, &inputlines);
  printf ("Lines read: %zd, characters: %zu\n", numlines, numchars);
  sigstrsum = RunProgram (numlines, (const char**)inputlines, 1);
  printf ("Signal strength sum: %ld\n", sigstrsum);
  for (int i = 0; i < numlines; i++)  free (inputlines[i]);
  free (inputlines);
}

// Advent of Code 2022 Day 10 Cathode-Ray Tube
// https://adventofcode.com/2022/day/10


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "freadlns.h"

/// Character to use for a pixel switched on
#define PIXELON_CHAR '#'
/// Character to use for a pixel switched off
#define PIXELOFF_CHAR '.'

const char *shortexample[] = { "noop", "addx 3", "addx -5" };


/// \brief Output ("render") the screen contents in text form
//
void OutputScreen (const char screen[6][40]) {
  for (int l = 0; l < 6; l++)  printf ("%*.*s\n", 40, 40, screen[l]);
}


/// \brief Run the program and return the sum of signal strengths
/// \param numlines  Number of instructions to run
/// \param lines     Instructions to run
/// \param screen    Pixels on a 6 lines x 40 column grid
/// \param loglevel  Verbosity of output
//
long int RunProgram (size_t numlines, const char **lines,
    char screen[][40], int loglevel) {
  long int cycle = 0;   // cycle counter
  long int regx = 1;    // register x value
  long int sigstrength = 0;   // sum of signal strengths
  for (int i = 0; i < numlines; i++) {
    int waittime = 1;   // how many cycles to wait before setting x
    long int pendingx = regx;   // value for x at the end of the cycle
    // Read instruction
    if (strncmp ("addx ", lines[i], 5) == 0) {
      waittime = 2;
      long incrx = strtol (lines[i] + 5, NULL, 10);
      if (loglevel >= 2)
        printf ("%4ld  addx %3ld  %4ld\n", cycle + 1, incrx, regx);
      pendingx = regx + incrx;
    }
    else if (strncmp ("noop", lines[i], 4) == 0) {
      if (loglevel >= 2)
        printf ("%4ld  noop      %4ld\n", cycle + 1, regx);
    }
    // Run instruction
    // - wait necessary cycles
    for (int t = 0; t < waittime; t++) {
      // Take signal strength samples during cycles 20, 60, 100, 140, ...
      if ((cycle + 20) % 40 == 39) {
        if (loglevel >= 1)
          printf ("%4ld   -->      %4ld  --> %ld\n", cycle + 1, regx,
            (cycle + 1) * regx);
        sigstrength += (cycle + 1) * regx;
      }
      // Draw a pixel
      // printf ("Store pixel: %d %d\n", cycle / 40, cycle % 40);
      screen[cycle / 40 % 6][cycle % 40] =
        (regx >= (cycle % 40) - 1 && regx <= (cycle % 40) + 1) ?
          PIXELON_CHAR : PIXELOFF_CHAR;
      if (loglevel >= 2 && cycle % 240 == 239)  OutputScreen (screen);
      // Move on to next cycle
      cycle++;
    }
    // - set new register value
    regx = pendingx;
  }
  if (loglevel >= 2)
    printf ("%4ld  ----      %4ld\n", cycle + 1, regx);
  return sigstrength;
}


/// \brief Run a program from file,
///   return the sum of signal strengths and the screen content
//
long int RunFile (const char *filename, char screen[6][40], int loglevel) {
  size_t maxlines = 10;
  char **inputlines = (char**) calloc (maxlines, sizeof (char*));
  size_t numlines;
  ssize_t numchars = readlines (filename, &maxlines, &numlines, &inputlines);
  if (loglevel >= 1)
    printf ("Lines read: %zd, characters: %zu\n", numlines, numchars);
  long int sigstrsum = RunProgram (numlines, (const char**)inputlines,
    screen, loglevel);
  for (int i = 0; i < numlines; i++)  free (inputlines[i]);
  free (inputlines);
  return sigstrsum;
}


int main () {
  printf ("--- Example ---\n");
  char screen[6][40];
  long int sigstrsum = RunFile ("10-cathode-ray-tube-example.txt",
    screen, 1);
  printf ("Signal strength sum: %ld\n", sigstrsum);
  OutputScreen (screen);
  printf ("\n");

  printf ("--- Puzzle 1: Sum of signal strengths ---\n");
  sigstrsum = RunFile ("10-cathode-ray-tube-input.txt", screen, 1);
  printf ("Signal strength sum: %ld\n", sigstrsum);
  printf ("\n");

  printf ("--- Puzzle 2: Render characters on screen ---\n");
  OutputScreen (screen);
  return 0;
}

// Advent of Code 2022 Day 20: Grove Positioning System
// https://adventofcode.com/2022/day/20

#include <stdio.h>
#include <stdlib.h>

#include "freadlns.h"

/// \brief Element data type in a simple doubly-linked list
//
typedef struct Number_st {
  long int value;
  struct Number_st *prev;
  struct Number_st *next;
} Number;


/// \brief Do one or more mixes of the numbers on the input lines, multiplying each number with the key first
//
long RunMixer (size_t numlines, const char **lines, int nummix, int multkey) {
  Number numbers[numlines];
  for (size_t i = 0; i < numlines; i++) {
    numbers[i].value = strtol (lines[i], NULL, 10) * multkey;
    numbers[i].prev = &(numbers[i > 0 ? i - 1 : numlines - 1]);
    numbers[i].next = &(numbers[i < numlines - 1 ? i + 1 : 0]);
  }
  for (int nmix = 0; nmix < nummix; nmix++) {
    for (size_t i = 0; i < numlines; i++) {
      Number *thisnum = &(numbers[i]);
      Number *newpos = &(numbers[i]);
      // Move forward to new position
      // Note:  One number skips over the other (numlines - 1) numbers
      if (numbers[i].value > 0)
        for (int j = 0; j <= numbers[i].value % (int)(numlines - 1); j++)
          newpos = newpos->next;
      // Move backward
      if (numbers[i].value < 0)
        for (int j = 0; j > numbers[i].value % (int)(numlines - 1); j--)
          newpos = newpos->prev;
      // Adapt neighbour pointers: take out old position, insert new
      if (numbers[i].value % (int)(numlines - 1) != 0) {
        Number *oldprev = thisnum->prev, *oldnext = thisnum->next;
        thisnum->prev->next = oldnext;   // oldprev --++++++++--oldnext
        thisnum->next->prev = oldprev;   //     \---[ thisnum ]---/
        thisnum->prev = newpos->prev;    // newprev <-- [ thisnum ] --> newpos
        thisnum->next = newpos;          //
        newpos->prev->next = thisnum;    // newprev --> [ thisnum ] <-- newpos
        newpos->prev = thisnum;
      }
      // Output new order
      // - Forward (to check next pointers)
      // Number *n = &(numbers[0]);
      // for (size_t j = 0; j <= numlines; j++, n = n->next) {
      //   printf ("%s%d", j > 0 ? ", " : "", n->value);
      // }
      // - Backward (to check prev pointers)
      // n = &(numbers[numlines - 1]);
      // printf ("\n");
      // for (size_t j = 0; j <= numlines; j++, n = n->prev) {
      //   printf ("%s%d", j > 0 ? ", " : "", n->value);
      // }
      // printf ("\n");
    } // for i
  } // for nmix
  // Determine result by finding 1000th, 2000th, 3000th number after value 0
  Number *currnum = NULL;
  for (size_t i = 0; i < numlines; i++)
    if (numbers[i].value == 0) { currnum = &(numbers[i]); break; }
  if (currnum == NULL) {
    fprintf (stderr, "Error: Value 0 not found\n");
    return 0;
  }
  long result = 0;
  // Move forward an find the three numbers
  for (int count = 0; count < 3; count++) {
    for (size_t skip = 0; skip < 1000 % numlines; skip++)
      currnum = currnum->next;
    printf ("Adding number: %ld\n", currnum->value);
    result += currnum->value;
  }
  return result;
}


const char *examplelines[] = { "1", "2", "-3", "3", "-2", "0", "4" };
const long decryptkey = 811589153;

int main () {
  printf ("--- Example ---\n");
  long result = RunMixer (sizeof (examplelines) / sizeof (examplelines[0]),
    examplelines, 1, 1);
  printf ("* Result: %ld *\n", result);
  printf ("\n");
  printf ("--- Example with key ---\n");
  result = RunMixer (sizeof (examplelines) / sizeof (examplelines[0]),
    examplelines, 10, decryptkey);
  printf ("* Result: %ld *\n", result);
  printf ("\n");

  printf ("--- Puzzle 1: Mix once ---\n");
  size_t maxlines = 5120, numlines = 0;
  char **inputlines = (char**) malloc (maxlines * sizeof (char*));
  /* ssize_t numchars = */ readlines ("20-grove-positioning-system-input.txt",
    &maxlines, &numlines, &inputlines);
  // printf ("Read %zu lines, %zd characters\n", numlines, numchars);
  result = RunMixer (numlines, (const char**)inputlines, 1, 1);
  printf ("*** Result: %ld ***\n", result);
  printf ("\n");

  printf ("--- Puzzle 2: Mix ten times and use decryption key ---\n");
  result = RunMixer (numlines, (const char**)inputlines, 10, decryptkey);
  printf ("*** Result: %ld ***\n", result);
  return 0;
}

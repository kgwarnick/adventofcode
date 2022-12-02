// Advent of Code 2022 Day 2: Rock Paper Scissors
// https://adventofcode.com/2022/day/2

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/// \brief Calculate the score for one round
//
unsigned long int roundscore (char chosen, char against) {
  // Calculate score for chosen shape
  unsigned short shapepoints =
    chosen == 'X' ? 1 : chosen == 'Y' ? 2 : chosen == 'Z' ? 3 : 0;
  // Calculate score for round result
  unsigned short roundresult =
    ((chosen == 'X' && against == 'C') || (chosen == 'Y' && against == 'A')
      || (chosen == 'Z' && against == 'B')) ? 6 :
    ((chosen == 'X' && against == 'A') || (chosen == 'Y' && against == 'B')
      || (chosen == 'Z' && against == 'C')) ? 3 :
    0;
  // Score for this round is the sum
  return shapepoints + roundresult;
}


/// \brief Predict the total score from the strategy guide read frmo file
//
unsigned long int predictedscore (const char * filename) {
  FILE *inputfile = fopen (filename, "rt");
  if (inputfile == NULL)  return -1;
  size_t linelen = 64;
  char *s = (char*) malloc (linelen);
  unsigned long int totalscore = 0;
  while (feof (inputfile) == 0) {
    int n = getline (&s, &linelen, inputfile);
    if (n == 0 || feof (inputfile))  break;
    // Parse characters to update score: opponent's choice - space - own choice
    totalscore += roundscore (s[2], s[0]);
  }
  free (s);
  return totalscore;
}


int main () {

  printf ("--- Example ---\n");
  printf ("Strategy tips:  A Y,  B X,  C Z\n");
  printf ("1st round: %c - %c = %lu\n", 'A', 'Y', roundscore ('Y', 'A'));
  printf ("2nd round: %c - %c = %lu\n", 'B', 'X', roundscore ('X', 'B'));
  printf ("3rd round: %c - %c = %lu\n", 'C', 'Z', roundscore ('Z', 'C'));
  printf ("\n");

  printf ("--- Puzzle 1: Predict score from strategy guide ---\n");
  printf ("Total score: %lu\n", predictedscore ("02-rock-paper-scissors-input.txt"));
}

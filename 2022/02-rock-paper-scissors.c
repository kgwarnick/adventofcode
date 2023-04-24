// Advent of Code 2022 Day 2: Rock Paper Scissors
// https://adventofcode.com/2022/day/2

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/// \brief Calculate the score for one round
//
unsigned long int roundscore (char against, char chosen) {
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


/// \brief Predict the total score from the strategy guide read from file
//
unsigned long int predictedscore (const char *filename) {
  FILE *inputfile = fopen (filename, "rt");
  if (inputfile == NULL)  return -1;
  size_t linelen = 64;
  char *s = (char*) malloc (linelen);
  unsigned long int totalscore = 0;
  while (feof (inputfile) == 0) {
    int n = getline (&s, &linelen, inputfile);
    if (n == 0 || feof (inputfile))  break;
    // Parse characters to update score: opponent's choice - space - own choice
    totalscore += roundscore (s[0], s[2]);
  }
  free (s);
  fclose (inputfile);
  return totalscore;
}


/// \brief Choose a shape so that the round ends as desired
/// \param result defines the desired outcome:
///   'X' = the round should be lost,
///   'Y' = the round should end in a draw
///   'Z' = the round should be won,
/// \return the shape to choose in order to achieve the expected result:
///   'X' = Rock, 'Y' = Paper, 'Z' = Scissors
char chooseshape (char against, char result) {
  switch (against) {
  case 'A': // Opponent chooses Rock
    return (result == 'X') ? 'Z' : (result == 'Z') ? 'Y': 'X';
  case 'B': // Opponent chooses Paper
    return (result == 'X') ? 'X' : (result == 'Z') ? 'Z': 'Y';
  case 'C': // Opponent chooses Scissors
    return (result == 'X') ? 'Y' : (result == 'Z') ? 'X': 'Z';
  default:
    return ' ';   // nonsense
  }
}


/// \brief Predict score for specific outcome of each round
//
unsigned long int scorewithforcedmatchoutcome (const char *filename) {
  FILE *inputfile = fopen (filename, "rt");
  if (inputfile == NULL)  return -1;
  size_t linelen = 64;
  char *s = (char*) malloc (linelen);
  unsigned long int totalscore = 0;
  // int roundno = 0;
  while (feof (inputfile) == 0) {
    // roundno++;
    int n = getline (&s, &linelen, inputfile);
    if (n == 0 || feof (inputfile))  break;
    // Choose appropriate shape to end the round as desired
    char choice = chooseshape (s[0], s[2]);
    totalscore += roundscore (s[0], choice);
  }
  free (s);
  fclose (inputfile);
  return totalscore;
}


int main () {

  printf ("--- Example Part 1: Predicted results ---\n");
  printf ("Strategy tips:  A Y,  B X,  C Z\n");
  printf ("1st round: %c - %c = %lu\n", 'A', 'Y', roundscore ('A', 'Y'));
  printf ("2nd round: %c - %c = %lu\n", 'B', 'X', roundscore ('B', 'X'));
  printf ("3rd round: %c - %c = %lu\n", 'C', 'Z', roundscore ('C', 'Z'));
  printf ("\n");

  printf ("--- Puzzle 1: Predict score from strategy guide ---\n");
  printf ("Total score: %lu\n", predictedscore ("02-rock-paper-scissors-input.txt"));
  printf ("\n");

  printf ("--- Example Part 2: Produce a certain result in each round ---\n");
  char choose = chooseshape ('A', 'Y');
  printf ("1st round: %c - %c = %lu\n", 'A', choose, roundscore ('A', choose));
  choose = chooseshape ('B', 'X');
  printf ("2nd round: %c - %c = %lu\n", 'B', choose, roundscore ('B', choose));
  choose = chooseshape ('C', 'Z');
  printf ("3rd round: %c - %c = %lu\n", 'C', choose, roundscore ('C', choose));
  printf ("\n");

  printf ("--- Puzzle 2: Predict score for specific round results ---\n");
  printf ("Total score: %lu\n",
    scorewithforcedmatchoutcome ("02-rock-paper-scissors-input.txt"));
}

// Advent of Code 2022 Day 6: Tuning Trouble
// https://adventofcode.com/2022/day/6


#include <stdio.h>
#include <stdlib.h>
// #include <stdbool.h>
#include <string.h>


size_t ReadCharacters (int n, char *buf, const char *filename) {
  FILE *f = fopen (filename, "rt");
  if (f == NULL) return 0;
  size_t nread = fread (buf, sizeof (char), n, f);
  fclose (f);
  return nread;
}




size_t DetectStartOfPacket (const char *datastream) {
  for (int i = 3; i < strlen (datastream); i++) {
    if (datastream[i-3] != datastream[i-2] &&
        datastream[i-3] != datastream[i-1] &&
        datastream[i-3] != datastream[i]   &&
        datastream[i-2] != datastream[i-1] &&
        datastream[i-2] != datastream[i] &&
        datastream[i-1] != datastream[i] )
      return i + 1;   // return a 1-based index
  }
  return 0;   // not found
}


char* examples[] = {
  "mjqjpqmgbljsphdztnvjfqwrcgsmlb",
  "bvwbjplbgvbhsrlpgdmjqwftvncz",
  "nppdvjthqldpwncqszvftbrmjlhg",
  "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg",
  "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"
};


int main () {

  printf ("--- Examples ---\n");
  for (int i = 0; i < sizeof (examples) / sizeof (examples[0]); i++) {
    size_t sop = DetectStartOfPacket (examples[i]);
    printf ("* %s -> Start-of-packet at position: %zu\n", examples[i], sop);
  }
  printf ("\n");

  printf ("--- Puzzle 1: Find start of packet ---\n");
  char *characters = (char*) malloc (sizeof (char) * 65536);
  size_t numchars = ReadCharacters (
    65536, characters, "06-tuning-trouble-input.txt");
  printf ("Characters read: %zu\n", numchars);
  size_t sop = DetectStartOfPacket (characters);
  printf ("*** Start-of-packet at position: %zu ***\n", sop);
  free (characters);
  return 0;
}

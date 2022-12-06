// Advent of Code 2022 Day 6: Tuning Trouble
// https://adventofcode.com/2022/day/6


#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>


size_t ReadCharacters (int n, char *buf, const char *filename) {
  FILE *f = fopen (filename, "rt");
  if (f == NULL) return 0;
  size_t nread = fread (buf, sizeof (char), n, f);
  fclose (f);
  return nread;
}


/// \brief Detect a sequence of all-different characters
/// \param seqlen  required number of characters being different
/// \return   The 1-based index of the last character in the sequence if found,
///           0 if no such sequence was found
//
size_t DetectSequenceOfUniqueCharacters (size_t seqlen, const char *datastream) {
  // Test all candidate sequences, first possible index is the length of the sequence
  for (size_t i = seqlen - 1; i < strlen (datastream); i++) {
    // Compare all characters of the candidate sequence ending here at index i to their preceding character
    // - First character of the sequence is at (i - seqlen + 1)
    // - The first comparison is necessary with the second character at (i - seqlen + 2)
    bool dupfound = false;
    for (size_t j = i - seqlen + 2; j <= i; j++) {
      for (size_t k = i - seqlen + 1; k < j; k++) {
        if (datastream[j] == datastream[k])  { dupfound = true; break; }
      }
      if (dupfound)  break;
    }
    // If no duplicate was found then this is the required sequence
    if (!dupfound)  return i + 1;   // return the 1-based index
  }
  // No sequence of enough different characters was found
  return 0;
}


/// \brief Detect the first start-of-packet marker, consisting of a sequence of four different characters
/// \return The index of the last character of the four-byte start-of-packet sequence (1-based), 0 if not found
//
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


/// \brief Detect the first start-of-message marker, consisting of a sequence of fourteen different characters
/// \return The index of the last character of the fourteen-byte start-of-packet sequence (1-based), 0 if not found
//
size_t DetectStartOfMessage (const char *datastream) {
  return DetectSequenceOfUniqueCharacters (14, datastream);
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
    size_t som = DetectStartOfMessage (examples[i]);
    printf ("* %s -> Start-of-packet at position: %zu\n", examples[i], sop);
    printf ("* %s -> Start-of-message at position: %zu\n", examples[i], som);
  }
  printf ("\n");

  printf ("--- Puzzle 1: Find start of packet ---\n");
  char *characters = (char*) malloc (sizeof (char) * 65536);
  size_t numchars = ReadCharacters (
    65536, characters, "06-tuning-trouble-input.txt");
  printf ("Characters read: %zu\n", numchars);
  size_t sop = DetectStartOfPacket (characters);
  char sopcontent[5];  sopcontent[4] = '\0';
  strncpy (sopcontent, characters + sop, 4);
  printf ("*** Start-of-packet at position: %zu, content: %4s ***\n", sop, sopcontent);
  printf ("*** Start-of-4-byte-sequence at position: %zu ***\n", DetectSequenceOfUniqueCharacters (4, characters));
  printf ("\n");

  printf ("--- Puzzle 2: Find start of message ---\n");
  size_t som = DetectStartOfMessage (characters);
  char somcontent[15];  somcontent[14] = '\0';
  strncpy (somcontent, characters + som, 14);
  printf ("*** Start-of-message = Start-of-14-byte-sequence at position: %zu, content: %14s ***\n", som, somcontent);
  free (characters);
  return 0;
}

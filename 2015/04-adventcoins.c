// Advent of Code 2015, Day 4, The Ideal Stocking Stuffer (Advent Coin Mining)
// https://adventofcode.com/2015/day/4


#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <sys/types.h> 
#include <md5.h>

char examplekey1[] = "abcdef";
char examplekey2[] = "pqrstuv";
int numreqzeroes = 5;


char *testhash (char *buffer, const char *key, const char *number) {
  buffer[0] = 0;
  MD5_CTX *md5ctx = (MD5_CTX*) malloc (sizeof (MD5_CTX));
  // printf ("- md5ctx = %p\n", md5ctx);
  if (md5ctx == NULL)  return NULL;
  char s[64];
  strncpy (s, key, 64);
  snprintf (s + strlen (s), 63 - strlen (s), "%s", number);
  // printf ("test data: %s\n", s);
  MD5Init (md5ctx);
  MD5Update (md5ctx, s, strlen (s));
  // MD5Final (buffer, md5ctx);
  MD5End (md5ctx, buffer);
  free (md5ctx);
  return buffer;
}

int findnumber (char *buffer, const char *key, int numrequiredzeroes) {
  for (unsigned int i = 0; i < UINT_MAX; i++) {
    char t[128];
    char s[32];  s[31] = '\0';
    char *res;
    snprintf (s, 32, "%u", i);
    if ((res = testhash (t, key, s)) == NULL) {
      fprintf (stderr, "Internal error: Failed to allocate memory for hash calculation\n");
      return -2;
    }
    bool fulfilled = true;
    for (int j = 0; j < numrequiredzeroes; j++) {
      if (t[j] != '0')  { fulfilled = false; break; }
    }
    if (fulfilled) {
      strncpy (buffer, t, 64);
      return i;
    }
  }
  return -1;   // kein passender Hash gefunden
}


int main (int argc, char *argv[]) {

  printf ("--- Beispiele ---\n");
  unsigned char buf[128];
  printf ("key = %-10s -> find number and hash: %8d  %s\n",
    examplekey1, findnumber (buf, examplekey1, 5), buf);
  printf ("key = %-10s -> find number and hash: %8d  %s\n",
    examplekey2, findnumber (buf, examplekey2, 5), buf);
  printf ("\n");

  printf ("--- Aufgabe 1: Finde Hash mit mindestens 5 Nullen ---\n");
  FILE *inputfile = fopen  ("04-adventcoins-input.txt", "rt");
  if (inputfile == NULL) {
    fprintf (stderr, "Fehler beim Lesen der Eingabedatei\n");
    return 1;
  }
  char keybuf[256];
  while (feof (inputfile) == 0) {
    char *c = fgets (keybuf, 256, inputfile);
    if (c == NULL) break;   // Nichts mehr gelesen, Dateiende?
    // Zeilenende-Zeichen entfernen
    for (char *ende = c + strlen (c) - 1;
         (ende > c) && (*ende == '\n' || *ende == '\r');
	  ende--) {
      *ende = '\0';
    }
    printf ("key = %s -> find number and hash: %d  %s\n",
      c, findnumber (buf, c, 5), buf);
  }
  return 0;
}

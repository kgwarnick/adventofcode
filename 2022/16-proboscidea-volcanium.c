// Advent of Code 2022 Day 16: Proboscidea Volcanium
// https://adventofcode.com/2022/day/16

#include <ctype.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "freadlns.h"

// Helper functions
int min (int a, int b) { return a < b ? a : b; }
int max (int a, int b) { return a > b ? a : b; }

const char *Example[] = {
  "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB",
  "Valve BB has flow rate=13; tunnels lead to valves CC, AA",
  "Valve CC has flow rate=2; tunnels lead to valves DD, BB",
  "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE",
  "Valve EE has flow rate=3; tunnels lead to valves FF, DD",
  "Valve FF has flow rate=0; tunnels lead to valves EE, GG",
  "Valve GG has flow rate=0; tunnels lead to valves FF, HH",
  "Valve HH has flow rate=22; tunnel leads to valve GG",
  "Valve II has flow rate=0; tunnels lead to valves AA, JJ",
  "Valve JJ has flow rate=21; tunnel leads to valve II "
};


/// \brief Data type to store valve information
//
typedef struct Valve_st {
  int index;
  char name[4];
  int rate;
  int numtunnels;
  char **tunnels;
  struct Valve_st **neighbours;
} Valve;

/// \brief Output valve information
//
void PrintValve (const Valve *v) {
  printf ("Valve %d %s (flow rate %d) -> ", v->index, v->name, v->rate);
  for (int i = 0; i < v->numtunnels; i++)
    printf ("%s%d %2s", (i > 0 ? ", ": ""),
      (v->neighbours != NULL && v->neighbours[i] != NULL ? v->neighbours[i]->index : -1),
      v->tunnels[i]);
  printf ("\n");
}

/// \brief Parse the tunnel part of a line
//
int ParseTunnelDescription (const char *s, char ***vlist) {
  const char *tunlist = NULL;
  // Where does the valve list actually start
  if (strncmp (s, "tunnel leads to valve ", 22) == 0)
    tunlist = s + 22;
  else if (strncmp (s, "tunnels lead to valves ", 23) == 0)
    tunlist = s + 23;
  if (tunlist <= s)  return 0;
  // How many tunnels? -- Count the commas and add one
  int ntun = 1;
  const char *c = tunlist - 1;
  while ((c = strchr (c + 1, ',')) != NULL)  ntun++;
  // Read the tunnels
  *vlist = (char**) malloc (ntun * sizeof (char*));
  c = tunlist;
  for (int i = 0; i < ntun; i++) {
    while (isspace (*c))  c++;
    (*vlist)[i] = strndup (c, 2);
    c = strchr (c, ',') + 1;
  }
  return ntun;
}

/// \brief Find a valve index by name
//
const Valve *FindValve (int numvalves, const Valve *v, const char *valvename) {
  for (int i = 0; i < numvalves; i++)
    if (strncmp (v[i].name, valvename, strlen (valvename)) == 0)  return &(v[i]);
  return NULL;   // not found
}


/// \brief Set up the neighbour pointers
//
void ResolveValves (int numvalves, Valve *valves) {
  for (int i = 0; i < numvalves; i++) {
    valves[i].neighbours =
      (Valve**) malloc (valves[i].numtunnels * sizeof (struct Valve_st*));
    for (int j = 0; j < valves[i].numtunnels; j++) {
      valves[i].neighbours[j] = (Valve*) FindValve (numvalves, valves, valves[i].tunnels[j]);
    }
  }
}

/// \brief Parse a single valve description
//
Valve *ParseValve (const char *line, Valve *v) {
  char vname[4];
  int r;
  int numchars;
  // Read name and flow rate
  int n = sscanf (line, "Valve %2s has flow rate=%d; %n", vname, &r, &numchars);
  if (n != 2)  return NULL;
  if (v == NULL)  v = (Valve*) malloc (sizeof (Valve));
  if (v == NULL)  return NULL;
  strncpy (v->name, vname, 4);
  v->rate = r;
  // Read connected tunnels
  v->numtunnels = ParseTunnelDescription (line + numchars, &(v->tunnels));
  // Return the parsed valve
  return v;
}

/// \brief Parse all valve descriptions
//
Valve *ParseValves (int numlines, const char **lines) {
  Valve *valves = (Valve*) malloc (numlines * sizeof (Valve));
  if (valves == NULL)  return NULL;
  for (int i = 0; i < numlines; i++) {
    valves[i].index = i;
    ParseValve (lines[i], valves + i);
  }
  ResolveValves (numlines, valves);
  return valves;
}

void FreeValves (int numvalves, Valve *valves) {
  for (int i = 0; i < numvalves; i++) {
    for (int j = 0; j < valves[i].numtunnels; j++) {
      free (valves[i].tunnels[j]);
    }
    free (valves[i].tunnels);
    free (valves[i].neighbours);
  }
  free (valves);
}


/// \brief Maximize pressure release based on distance table
//
// One step is: moving to a valve with non-zero flow rate and opening it.
//   This takes as many seconds as the shortest distance between current and
//   new valve, plus one second to open the valve.
//   Then the current pressure release has increased and the next step starts
//   by a recursive call
//
//   Note:  Reporting of best path not working
//
int MaxRelease (int numvalves, Valve *valves, const int **dist,
    int timeleft, bool *opened, int rate, int amount, int currvalve,
    int numsteps, char *currpath, char *bestpath) {
  int bestamount = amount;
  currpath[numsteps * 2] = '-';  currpath[numsteps*2+1] = '-';  currpath[numsteps*2+2] = '\0';
  // Try each valve
  for (int i = 0; i < numvalves; i++) {
    // Ignore valves without flow or which are already open
    if (valves[i].rate == 0 || opened[i])  continue;
    if (i == currvalve)  continue;
    // Open this valve and get the best result from following that path
    int needtime = dist[currvalve][i] + 1;
    if (needtime >= timeleft)  continue;   // Cannot reach that valve in time
    opened[i] = true;   // Open the valve for recursive call
    currpath[numsteps*2] = valves[i].name[0];  currpath[numsteps*2+1] = valves[i].name[1];
    int temp = MaxRelease (numvalves, valves, dist, timeleft - needtime,
      opened, rate + valves[i].rate, amount + needtime * rate, i,
      numsteps + 1, currpath, bestpath);
    opened[i] = false;   // Close the valve again before trying the next
    if (temp > bestamount) {
      bestamount = temp;
      bestpath[numsteps*2] = valves[i].name[0];  bestpath[numsteps*2+1] = valves[i].name[1];
    }
  }
  // Just run down the remaining seconds
  if (amount + timeleft * rate > bestamount) {
    bestamount = amount + timeleft * rate;
    bestpath[numsteps*2] = '-';  bestpath[numsteps*2+1] = '-';  bestpath[numsteps*2+2] = '\0';
  }
  return bestamount;
}


/// \brief Build table of shortest distances between every pair of valves
///   via Floyd-Warshall algorithm
//
int **CreateValveDistances (int numvalves, Valve *valves) {
  int **dist = (int**) malloc (numvalves * sizeof (int*));
  // Initialisation
  for (int i = 0; i < numvalves; i++) {
    dist[i] = (int*) malloc (numvalves * sizeof (int));
    memset (dist[i], 0x0F, numvalves * sizeof (int));   // Set to large number
    dist[i][i] = 0;   // Set diagonals (="distance to self") to 0
    for (int t = 0; t < valves[i].numtunnels; t++) {
      int j = valves[i].neighbours[t]->index;
      dist[i][j] = 1;   // Fill direct connections with 1
    }
  }
  for (int k = 0; k < numvalves; k++)
    for (int i = 0; i < numvalves; i++)
      for (int j = 0; j < numvalves; j++)
        dist[i][j] = min (dist[i][j], dist[i][k] + dist[k][j]);
  return dist;
}


/// \brief Determine the best path to open the valves for the setup given
//
int FindMaximumPressureRelease (int numlines, const char **lines, int timesteps) {
  Valve *v = ParseValves (numlines, lines);
  int **dist = CreateValveDistances (numlines, v);
  bool *openvalves = (bool*) malloc (numlines * sizeof (bool));
  memset (openvalves, 0x00, numlines * sizeof (bool));
  char testpath[256], bestpath[256];
  testpath[0] = '\0';  bestpath[0] = '\0';
  // Find starting position
  int startvalve = FindValve (numlines, v, "AA") ->index;
  printf ("Start at valve: %d\n", startvalve);
  // Calculate maximum pressure release
  int released = MaxRelease (numlines, v, (const int**)dist, timesteps,
    openvalves, 0, 0, startvalve, 0, testpath, bestpath);
  // printf ("Pressure released: %d with path \"%s\"\n", released, bestpath);
  printf ("Pressure released: %d\n", released);
  free (openvalves);
  for (int i = 0; i < numlines; i++)  free (dist[i]);
  free (dist);
  FreeValves (numlines, v);
  return released;
}


int main () {
  printf ("--- Example ---\n");
  FindMaximumPressureRelease (sizeof (Example) / sizeof (Example[0]), Example, 30);
  printf ("\n");

  printf ("--- Puzzle 1: Best pressure release ---\n");
  size_t maxlines = 100, numlines = 0;
  char **lines = (char**) malloc (maxlines * sizeof (char*));
  readlines ("16-proboscidea-volcanium-input.txt",
    &maxlines, &numlines, &lines);
  FindMaximumPressureRelease (numlines, (const char**)lines, 30);
}

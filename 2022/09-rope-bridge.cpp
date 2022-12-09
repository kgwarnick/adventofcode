// Advent of Code 2022 Day 9: Rope Bridge
// https://adventofcode.com/2022/day/9

#include <iostream>
#include <iomanip>
#include <list>
#include <vector>
#include <map>
#include <string>
#include <sstream>
#include <numeric>

using namespace std;

#include "fileread.hpp"

const list<string> examplelines = {
  "R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"
};
const list<string> largerexamplelines = {
  "R 5", "U 8", "L 8", "D 3", "R 17", "D 10", "L 25", "U 20"
};


struct CoordPair {
  int x;
  int y;
 public:
};

bool operator< (const CoordPair& lhs, const CoordPair& rhs) {
  return (lhs.x < rhs.x) || (lhs.x == rhs.x && lhs.y < rhs.y);
}


/// \brief A rope with a specified number of nodes
//
class Rope {
 public:
  vector<CoordPair> nodes;
  int numnodes () const { return nodes.size(); }
  Rope (int numnodes = 2) { nodes.resize (numnodes); }
  void Print (int left, int right, int bottom, int top) const;
};

/// \brief Output a rope drawing with the specified boundaries
//
void Rope::Print (int left, int right, int bottom, int top) const {
  for (int y = top; y >= bottom; y--) {
    for (int x = left; x <= right; x++) {
      char c = '.';
      for (int n = nodes.size() - 1; n >= 0; n--) {
        if (nodes[n].x == x && nodes[n].y == y)  c = '0' + n;
      }
      cout << c;
    }
    cout << endl;
  }
}


/// \brief Increment a map entry or create the entry if it does not exist yet
/// \typename K  Type of the map key
/// \typename V  Type of the map value
/// \param mapping  The map of K -> V pairs
/// \param key      The key to search for
/// \param defaultval  Value to set if the key is not yet contained in the map
/// \param increment   Increment to apply on the entry if the key already exists
//
template<typename K, typename V>
void IncrementOrCreateEntry (map<K,V>& mapping, K key,
    V defaultval = 1, V increment = 1) {
  typename map<K,V>::iterator iter = mapping.find (key);
  if (iter != mapping.end()) iter->second += increment;
  else mapping[key] = defaultval;
}


/// \brief Perform a single step of the coordinate pair in the specified direction
//
CoordPair MakeStep (const CoordPair& coords, char direction) {
  CoordPair cp = coords;
  if (direction == 'L')  cp.x--;
  else if (direction == 'R')  cp.x++;
  else if (direction == 'D')  cp.y--;
  else if (direction == 'U')  cp.y++;
  else cerr << "Error: invalid direction '" << direction << "'" << endl;
  return cp;
}


/// \brief Follow the preceding node
//
CoordPair FollowStep (const CoordPair& head, const CoordPair& tail) {
  CoordPair cp = tail;
  // Coordinates need to be updated if one dimension is off by more than 1.
  // If the other coordinate is different, too, a diagonal step is done
  if (tail.y < head.y - 1) {   // at least two behind in y direction
    cp.y += 1;
    if (tail.x - head.x < 0)  cp.x += 1;
    else if (tail.x - head.x > 0)  cp.x -= 1;
  }
  else if (tail.y > head.y + 1) {   // at least two ahead in y direction
    cp.y -= 1;
    if (tail.x - head.x < 0)  cp.x += 1;
    else if (tail.x - head.x > 0)  cp.x -= 1;
  }
  else if (tail.x < head.x - 1) {   // at least two behind in x direction
    cp.x += 1;
    if (tail.y - head.y < 0)  cp.y += 1;
    else if (tail.y - head.y > 0)  cp.y -= 1;
  }
  else if (tail.x > head.x + 1) {   // at least two ahead in x direction
    cp.x -= 1;
    if (tail.y - head.y < 0)  cp.y += 1;
    else if (tail.y - head.y > 0)  cp.y -= 1;
  }
  return cp;
}


/// \brief Follow the movements from the input list and trace head and tail positions
/// \param lines  Input lines describing the rope head's movements
/// \param HeadVisited  All the positions the rope head visited with the count how often
/// \param HeadVisited  All the positions the rope tail visited with the count how often
/// \param numnodes     Number of nodes to simulate
/// \param startpoint   Starting position
//
void FollowMovements (const list<string>& lines,
    map<CoordPair,unsigned int>& HeadVisited,
    map<CoordPair,unsigned int>& TailVisited,
    int numnodes = 2,
    CoordPair startpoint = { 0, 0 }) {
  list<CoordPair> headpositions, tailpositions;
  Rope rope (numnodes);
  // All nodes visit the starting position first
  headpositions.push_back (startpoint);
  tailpositions.push_back (startpoint);
  IncrementOrCreateEntry (HeadVisited, startpoint);
  IncrementOrCreateEntry (TailVisited, startpoint);
  // Apply movements and record visited locations
  for (string line : lines) {
    stringstream ss (line);
    char direction;
    int steps;
    ss >> direction >> steps;
    // cout << "(" << direction << ", " << steps << ")" << endl;
    for (int i = 0; i < steps; i++) {
      // Let the had node make another step
      rope.nodes[0] = MakeStep (rope.nodes[0], direction);
      // Every other node follows its predecessor
      for (int n = 1; n < rope.numnodes(); n++) {
        rope.nodes[n] = FollowStep (rope.nodes[n-1], rope.nodes[n]);
        // cout << "  - head: (" << rope.nodes[n-1].x << ", " << rope.nodes[n-1].y << "), tail: ("
        //   << rope.nodes[n].x << ", " << rope.nodes[n].y << ")" << endl;
      }
      // Record head and tail positions
      headpositions.push_back (rope.nodes.front());
      tailpositions.push_back (rope.nodes.back());
      IncrementOrCreateEntry (HeadVisited, rope.nodes.front());
      IncrementOrCreateEntry (TailVisited, rope.nodes.back());
    }
    // rope.Print (-30, 30, -10, 15);  cout << endl;
  }
}


/// \brief Visualise the nodes in the map
//
void PrintVisitedNodes (map<CoordPair,unsigned int> coordmap) {
  // Determine minimum and maximum coordinates
  int xmin = accumulate (coordmap.cbegin(), coordmap.cend(), (unsigned int)0,
    [] (int m, const pair<CoordPair,unsigned int>& cp) { return min (m, cp.first.x); });
  int xmax = accumulate (coordmap.cbegin(), coordmap.cend(), (unsigned int)0,
    [] (int m, const pair<CoordPair,unsigned int>& cp) { return max (m, cp.first.x); });
  int ymin = accumulate (coordmap.cbegin(), coordmap.cend(), (unsigned int)0,
    [] (int m, const pair<CoordPair,unsigned int>& cp) { return min (m, cp.first.y); });
  int ymax = accumulate (coordmap.cbegin(), coordmap.cend(), (unsigned int)0,
    [] (int m, const pair<CoordPair,unsigned int>& cp) { return max (m, cp.first.y); });
  // Print a header
  cout << "      ";
  for (int x = xmin; x <= xmax; x++)
    cout << (x % 10 != 0 ? ' ' : (x == 0 ? '0' : ':'));
  cout << endl;
  // Print the grid of positions
  CoordPair cp;
  for (int y = ymax; y >= ymin; y--) {
    cp.y = y;
    cout << setw(4) << y << "  ";
    for (int x = xmin; x <= xmax; x++) {
      cp.x = x;
      char c = '.';
      map<CoordPair,unsigned int>::iterator miter = coordmap.find (cp);
      if (miter != coordmap.end()) {
        // output a digit that represents how often this node was visited,
        // a star * stands for ten or more times
        if (miter->second < 10)  c = ('0' + miter->second);  else c = '*';
      }
      cout << c;
    }
    cout << "  " << setw (4) << y << endl;
  }
}


/// \brief Run a sequence of movements and return the number of nodes visited by the tail
///        at least once
//
int RunMovementList (const list<string>& movements, int numnodes = 2, int loglevel = 1) {
  cout << "Running " << movements.size() << " movements" << endl;
  map<CoordPair,unsigned int> heads, tails;
  FollowMovements (movements, heads, tails, numnodes);
  cout << "Head visited positions: " << heads.size() << endl;
  // for (pair<CoordPair,int> cp : heads)
  //   cout << "  - (" << cp.first.x << ", " << cp.first.y << ") -> " << cp.second << endl;
  cout << "Tail visited positions: " << tails.size() << endl;
  // for (pair<CoordPair,unsigned int> cp : tails)
  //   cout << "  - (" << cp.first.x << ", " << cp.first.y << ") -> " << cp.second << endl;
  if (loglevel >= 2)  PrintVisitedNodes (tails);
  return tails.size();
}


int main () {
  cout << "--- Examples ---" << endl;
  int numvisits = RunMovementList (examplelines, 2, 2);
  cout << endl;
  numvisits = RunMovementList (examplelines, 10, 2);
  cout << endl;
  numvisits = RunMovementList (largerexamplelines, 10, 2);
  cout << endl;

  cout << "--- Puzzle 1: Positions visited by the rope tail for two nodes ---" << endl;
  list<string> inputlines = ReadLines ("09-rope-bridge-input.txt");
  cout << inputlines.size () << " lines read" << endl;
  numvisits = RunMovementList (inputlines);
  cout << "*** Number of nodes visited by the tail at least once: "
    << numvisits << " ***" << endl;
  cout << endl;

  cout << "--- Puzzle 2: Positions visited by the rope tail for 10 nodes ---" << endl;
  numvisits = RunMovementList (inputlines, 10);
  cout << "*** Number of nodes visited by the tail at least once: "
    << numvisits << " ***" << endl;
}

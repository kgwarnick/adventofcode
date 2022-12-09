// Advent of Code 2022 Day 9: Rope Bridge
// https://adventofcode.com/2022/day/9

#include <iostream>
#include <list>
#include <map>
#include <string>
#include <sstream>

using namespace std;

#include "fileread.hpp"

const list<string> examplelines = {
  "R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"
};

struct CoordPair {
  int x;
  int y;
 public:
 /*
  bool operator< (const CoordPair& other) const {
    return (this->x < other.x) || (this->x == other.x && this->y < other.y);
  }
  bool operator> (const CoordPair other) const {
    return (this->x > other.x) || (this->x == other.x && this->y > other.y);
  }
  */
};


bool operator< (const CoordPair& lhs, const CoordPair& rhs) {
  return (lhs.x < rhs.x) || (lhs.x == rhs.x && lhs.y < rhs.y);
}


/// \brief Perform a single step of the head in the specified direction and update the tail
//
void MakeStep (CoordPair& head, CoordPair& tail, char direction) {
  // Move head
  if (direction == 'L')  head.x--;
  else if (direction == 'R')  head.x++;
  else if (direction == 'D')  head.y--;
  else if (direction == 'U')  head.y++;
  else cerr << "Error: invalid direction '" << direction << "'" << endl;
  // Update tail
  // Note: This does not handle case properly
  //   where the tail is off by more than 1 in both x and y directions,
  //   so print a warning if this is the case
  if ((head.x - tail.x < -1 || head.x - tail.x > 1) &&
      (head.y - tail.y < -1 || head.y - tail.y > 1))
    cerr << "Warning: Tail (" << tail.x << ", " << tail.y <<
      ") too far behind head (" << head.x << ", " << head.y << ")" << endl;
  // Move the tail nearer the head again
  if (tail.y < head.y - 1) { tail.y = head.y - 1;  tail.x = head.x; }
  else if (tail.y > head.y + 1) { tail.y = head.y + 1;  tail.x = head.x; }
  else if (tail.x < head.x - 1) { tail.x = head.x - 1;  tail.y = head.y; }
  else if (tail.x > head.x + 1) { tail.x = head.x + 1;  tail.y = head.y; }
}


/// \brief Follow the movements from the input list and trace head and tail positions
/// \param lines  Input lines describing the rope head's movements
/// \param HeadVisited  All the positions the rope head visited with the count how often
/// \param HeadVisited  All the positions the rope tail visited with the count how often
/// \param startpoint  Starting position
//
void FollowMovements (const list<string>& lines,
    map<CoordPair,unsigned int>& HeadVisited,
    map<CoordPair,unsigned int>& TailVisited,
    CoordPair startpoint = { 0, 0 }) {
  list<CoordPair> headpositions, tailpositions;
  map<CoordPair,unsigned int>::iterator headiter, tailiter;
  // Head and tail start at the same location
  CoordPair headpos = startpoint;
  CoordPair tailpos = startpoint;
  headpositions.push_back (startpoint);
  tailpositions.push_back (startpoint);
  headiter = HeadVisited.find (startpoint);
  if (headiter != HeadVisited.end()) headiter->second++;
  else HeadVisited[startpoint] = 1;
  tailiter = TailVisited.find (startpoint);
  if (tailiter != TailVisited.end()) tailiter->second++;
  else TailVisited[startpoint] = 1;
  // Apply movements and record visited locations
  for (string line : lines) {
    stringstream ss (line);
    char direction;
    int steps;
    ss >> direction >> steps;
    // cout << "(" << direction << ", " << steps << ")" << endl;
    for (int i = 0; i < steps; i++) {
      MakeStep (headpos, tailpos, direction);
      // cout << "  - head: (" << headpos.x << ", " << headpos.y << "), tail: ("
      //   << tailpos.x << ", " << tailpos.y << ")" << endl;
      headpositions.push_back (headpos);
      tailpositions.push_back (tailpos);
      headiter = HeadVisited.find (headpos);
      if (headiter != HeadVisited.end()) headiter->second++;
      else HeadVisited[headpos] = 1;
      tailiter = TailVisited.find (tailpos);
      if (tailiter != TailVisited.end()) tailiter->second++;
      else TailVisited[tailpos] = 1;
    }
  }
}


void RunMovementList (const list<string>& movements) {
  cout << "Running " << movements.size() << " movements" << endl;
  map<CoordPair,unsigned int> heads, tails;
  FollowMovements (movements, heads, tails);
  cout << "Head visited positions: " << heads.size() << endl;
  // for (pair<CoordPair,int> cp : heads)
  //   cout << "  - (" << cp.first.x << ", " << cp.first.y << ") -> " << cp.second << endl;
  cout << "Tail visited positions: " << tails.size() << endl;
  // for (pair<CoordPair,unsigned int> cp : tails)
  //   cout << "  - (" << cp.first.x << ", " << cp.first.y << ") -> " << cp.second << endl;
}


int main () {
  cout << "--- Example ---" << endl;
  RunMovementList (examplelines);
  cout << endl;

  cout << "--- Puzzle 1: Positions visited by the rope tail ---" << endl;
  list<string> inputlines = ReadLines ("09-rope-bridge-input.txt");
  cout << inputlines.size () << " lines read" << endl;
  RunMovementList (inputlines);
}

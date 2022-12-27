// Advent of Code 2022 Day 23: Unstable Diffusion
// https://adventofcode.com/2022/day/23

#include <climits>
#include <list>
#include <map>
#include <iostream>
#include <iomanip>
#include <string>
#include <numeric>

using namespace std;

#include "fileread.hpp"


/// \brief 2D coordinates
//
struct Coord2D {
  int x;
  int y;
  bool operator < (const struct Coord2D& other) const {
    return (this->x < other.x) || (this->x == other.x && this->y < other.y);
  }
};


/// \brief Ground represented by '.' for empty ground or '#' for elf
//
struct Ground {
  int startx;
  int starty;
  vector<vector<char>> gr;
  char GetTile (int x, int y) const;
};

/// \brief Get tile at specified position;
///   positions outside the used range are returned as empty ' '
char Ground::GetTile (int x, int y) const {
  if (y < starty || y >= starty + (int) gr.size())  return ' ';
  if (x < startx || x >= startx + (int) gr[y-starty].size())  return ' ';
  return gr[y-starty][x-startx];
}


/// \brief An elf described by their position and also the proposal for the next movement
//
struct Elf {
  int x;
  int y;
  bool hasproposedstep;
  int proposedx;
  int proposedy;
  void PlanNoStep () { hasproposedstep = false; proposedx = 0; proposedy = 0; }
  void PlanStep (int newx, int newy) {
    hasproposedstep = true; proposedx = newx; proposedy = newy; }
  /// \brief Is there a neighbour in the three adjacent tiles to the north
  bool HasNorthNeighbours (const Ground& groundmap) const;
  /// \brief Is there a neighbour in the three adjacent tiles to the south
  bool HasSouthNeighbours (const Ground& groundmap) const;
  /// \brief Is there a neighbour in the three adjacent tiles to the west
  bool HasWestNeighbours (const Ground& groundmap) const;
  /// \brief Is there a neighbour in the three adjacent tiles to the east
  bool HasEastNeighbours (const Ground& groundmap) const;
  /// \brief Is there a neighbour in the specified direction
  bool HasNeighboursIn (const Ground& groundmap, int direction) const;
  /// \brief Is there a neighbour in any of the eight surrounding tiles
  bool HasNeighbours (const Ground& groundmap) const;
  /// \brief Suggest x and y positions for a move in the specified direction
  void SuggestStepIn (int& px, int& py, int phase) const;
  /// \brief Try to find a direction in which a mocce is possible,
  ///   starting with the direction given by `phase`
  bool TryProposeStep (const Ground& groundmap, int phase, int& px, int& py) const;
};

bool Elf::HasNorthNeighbours (const Ground& groundmap) const {
  return (groundmap.GetTile (this->x - 1, this->y - 1) == '#' ||
          groundmap.GetTile (this->x    , this->y - 1) == '#' ||
          groundmap.GetTile (this->x + 1, this->y - 1) == '#');
}

bool Elf::HasSouthNeighbours (const Ground& groundmap) const {
  return (groundmap.GetTile (this->x - 1, this->y + 1) == '#' ||
          groundmap.GetTile (this->x    , this->y + 1) == '#' ||
          groundmap.GetTile (this->x + 1, this->y + 1) == '#');
}

bool Elf::HasWestNeighbours (const Ground& groundmap) const {
  return (groundmap.GetTile (this->x - 1, this->y - 1) == '#' ||
          groundmap.GetTile (this->x - 1, this->y    ) == '#' ||
          groundmap.GetTile (this->x - 1, this->y + 1) == '#');
}

bool Elf::HasEastNeighbours (const Ground& groundmap) const {
  return (groundmap.GetTile (this->x + 1, this->y - 1) == '#' ||
          groundmap.GetTile (this->x + 1, this->y    ) == '#' ||
          groundmap.GetTile (this->x + 1, this->y + 1) == '#');
}

bool Elf::HasNeighboursIn (const Ground& groundmap, int direction) const {
  if (direction % 4 == 0)  return HasNorthNeighbours (groundmap);
  if (direction % 4 == 1)  return HasSouthNeighbours (groundmap);
  if (direction % 4 == 2)  return HasWestNeighbours (groundmap);
  if (direction % 4 == 3)  return HasEastNeighbours (groundmap);
  return HasNeighbours (groundmap);
}

bool Elf::HasNeighbours (const Ground& groundmap) const {
  if (groundmap.GetTile (this->x - 1, this->y - 1) == '#' ||
      groundmap.GetTile (this->x    , this->y - 1) == '#' ||
      groundmap.GetTile (this->x + 1, this->y - 1) == '#')  return true;
  if (groundmap.GetTile (this->x - 1, this->y) == '#' ||
      groundmap.GetTile (this->x + 1, this->y) == '#')  return true;
  if (groundmap.GetTile (this->x - 1, this->y + 1) == '#' ||
      groundmap.GetTile (this->x    , this->y + 1) == '#' ||
      groundmap.GetTile (this->x + 1, this->y + 1) == '#')  return true;
  return false;
}

void Elf::SuggestStepIn (int& px, int& py, int phase) const {
  if (phase % 4 == 0)  { px = x; py = y - 1; }   // to north
  else if (phase % 4 == 1)  { px = x; py = y + 1; }   // to south
  else if (phase % 4 == 2)  { px = x - 1; py = y; }   // to west
  else if (phase % 4 == 3)  { px = x + 1; py = y; }   // to east
  else { px = x; py = y; }   // no move
}

bool Elf::TryProposeStep (const Ground& groundmap, int phase, int& px, int& py) const {
  if (!HasNeighboursIn (groundmap, phase)) {
    SuggestStepIn (px, py, phase);  return true; }
  else if (!HasNeighboursIn (groundmap, phase + 1)) {
    SuggestStepIn (px, py, phase + 1);  return true; }
  else if (!HasNeighboursIn (groundmap, phase + 2)) {
    SuggestStepIn (px, py, phase + 2);  return true; }
  else if (!HasNeighboursIn (groundmap, phase + 3)) {
    SuggestStepIn (px, py, phase + 3);  return true; }
  else return false;
}


/// \brief Parse input lines to determine the elves' positions
//
list<Elf> ReadElfPositions (const list<string>& lines) {
  list<Elf> elves;
  int y = 0;
  for (string line : lines) {
    string::size_type x = -1;
    while ((x = line.find ("#", x + 1)) != string::npos) {
      elves.push_back (Elf { (int)x, y, false, 0, 0 });
    }
    y++;
  }
  return elves;
}


/// \brief Generate a map of the ground covered by the elves in the list
//
Ground CreateGroundMap (const list<Elf>& elves) {
  Ground ground;
  int minx = accumulate (elves.cbegin(), elves.cend(), INT_MAX,
    [] (int n, const Elf& e) { return min (n, e.x); });
  int maxx = accumulate (elves.cbegin(), elves.cend(), INT_MIN,
    [] (int n, const Elf& e) { return max (n, e.x); });
  int miny = accumulate (elves.cbegin(), elves.cend(), INT_MAX,
    [] (int n, const Elf& e) { return min (n, e.y); });
  int maxy = accumulate (elves.cbegin(), elves.cend(), INT_MIN,
    [] (int n, const Elf& e) { return max (n, e.y); });
  ground.startx = minx;  ground.starty = miny;
  ground.gr = vector<vector<char>> (maxy - miny + 1);
  // Fill with empty ground '.'
  for (vector<char>& l : ground.gr) {
    l.insert (l.begin(), maxx - minx + 1, '.');
  }
  // Place elves '#'
  for (const Elf& e : elves) {
    ground.gr[e.y - miny][e.x - minx] = '#';
  }
  // Return map
  return ground;
}

/// \brief Output a map in text form of the ground covered
//
void OutputMap (const Ground& ground) {
  for (int y = 0; y < (int)ground.gr.size(); y++) {
    cout << setw(4) << y + ground.starty << "|";
    for (int x = 0; x < (int)ground.gr[y].size(); x++)  cout << ground.gr[y][x];
    cout << "|" << endl;
  }
}


/// \brief Find possible movements for every elf
//
map<Coord2D,unsigned short> ProposePositions (list<Elf>& elves,
    const Ground& ground, unsigned int round) {
  map<Coord2D,unsigned short> prop;
  for (Elf& e : elves) {
    // Check whether there are neighbours at all
    if (!e.HasNeighbours (ground)) {
      // cout << "[" << round << "]  Elf at ("
      //   << e.x << ", " << e.y << " has no neigbours" << endl;
      e.PlanNoStep ();
      continue;
    }
    // Propose a direction
    int propx, propy;
    if (e.TryProposeStep (ground, round % 4, propx, propy)) {
#ifdef DEBUG
      cout << "[" << setw(4) << round << "]  Elf at ("
        << e.x << ", " << e.y << ") proposes move to (" << propx << ", "
        << propy << ")" << endl;
#endif
      e.PlanStep (propx, propy);
      map<Coord2D,unsigned short>::iterator mit =
        prop.find (Coord2D { propx, propy });
      if (mit != prop.end()) {
        mit->second++;
        // cout << "- Already proposed, count: " << mit->second << endl;
      } else {
        prop [Coord2D { propx, propy }] = 1;
        // cout << "- Not yet proposed, count: 1" << endl;
      }
    }
  }
  return prop;
}


/// \brief Carry out all movements without conflicts (where only one elf target a specific tile)
//
list<Elf> PerformMoves (const list<Elf>& elves,
    const map<Coord2D,unsigned short> proposals) {
  list<Elf> movedelves;
  for (const Elf& elf : elves) {
    if (elf.hasproposedstep) {
      map<Coord2D,unsigned short>::const_iterator prop =
        proposals.find (Coord2D { elf.proposedx, elf.proposedy });
      if (prop->second <= 1) {
        movedelves.push_back (Elf { elf.proposedx, elf.proposedy, false, 0, 0 });
#ifdef DEBUG
        cout << "- Elf at (" << elf.x << ", " << elf.y << ") moves to ("
          << elf.proposedx << ", " << elf.proposedy << ")" << endl;
#endif
      }
      else {
        movedelves.push_back (Elf { elf.x, elf.y, false, 0, 0 });
#ifdef DEBUG
        cout << "- Elf at (" << elf.x << ", " << elf.y << ") cannot move to ("
          << elf.proposedx << ", " << elf.proposedy << ")" << endl;
#endif
      }
    }
    else {
      movedelves.push_back (Elf { elf.x, elf.y, false, 0, 0 });
#ifdef DEBUG
      cout << "- Elf at (" << elf.x << ", " << elf.y << ") does not move" << endl;
#endif
    }
  }
  return movedelves;
}


/// \brief Run a number of rounds until the limit is reached or no moves
///   are necessary any more
//
list<Elf> RunRounds (const list<Elf>& elves, int maxrounds, int& runrounds) {
  Ground grmap = CreateGroundMap (elves);
  // OutputMap (grmap);
  list<Elf> currelves = elves;
  for (runrounds = 0; runrounds < maxrounds; runrounds++) {
    map<Coord2D,unsigned short> proposals =
      ProposePositions (currelves, grmap, runrounds);
    if (proposals.empty()) {
      cout << "[" << setw(4) << runrounds << "]  No more moves" << endl;
      break;
    }
    // for (pair<Coord2D,unsigned short> proposal : proposals) {
    //   cout << "Proposal:  (" << proposal.first.x << ", "
    //     << proposal.first.y << ")  " << proposal.second << "x" << endl;
    // }
    list<Elf> newelves = PerformMoves (currelves, proposals);
    currelves = newelves;
    grmap = CreateGroundMap (currelves);
    // OutputMap (grmap);
  }
  return currelves;
}


unsigned long GetEmptyGround (const list<Elf>& elves) {
  // Empty ground is total ground minus number of elves
  int minx = accumulate (elves.cbegin(), elves.cend(), INT_MAX,
    [] (int n, const Elf& e) { return min (n, e.x); });
  int maxx = accumulate (elves.cbegin(), elves.cend(), INT_MIN,
    [] (int n, const Elf& e) { return max (n, e.x); });
  int miny = accumulate (elves.cbegin(), elves.cend(), INT_MAX,
    [] (int n, const Elf& e) { return min (n, e.y); });
  int maxy = accumulate (elves.cbegin(), elves.cend(), INT_MIN,
    [] (int n, const Elf& e) { return max (n, e.y); });
  return (maxy - miny + 1) * (maxx - minx + 1) - elves.size();
}


const list<string> smallexamplelines = {
  ".....",
  "..##.",
  "..#..",
  ".....",
  "..##.",
  "....."
};

const list<string> largeexamplelines = {
  "....#..",
  "..###.#",
  "#...#.#",
  ".#...##",
  "#.###..",
  "##.#.##",
  ".#..#.."
};

int main () {
  cout << "--- Small Example ---" << endl;
  list<Elf> elves = ReadElfPositions (smallexamplelines);
  cout << "Found " << elves.size() << " elves on "
    << smallexamplelines.size() << " lines" << endl;
  int needrounds = 0;
  list<Elf> diffused = RunRounds (elves, 10, needrounds);
  Ground grmap = CreateGroundMap (diffused);
  cout << "After " << needrounds << " rounds" << endl;
  OutputMap (grmap);
  cout << "* Empty ground tiles: " << GetEmptyGround (diffused) << " *" << endl;
  cout << endl;

  cout << "--- Larger Example ---" << endl;
  elves = ReadElfPositions (largeexamplelines);
  cout << "Found " << elves.size() << " elves on "
    << largeexamplelines.size() << " lines" << endl;
  diffused = RunRounds (elves, 10, needrounds);
  grmap = CreateGroundMap (diffused);
  cout << "After " << needrounds << " rounds" << endl;
  OutputMap (grmap);
  cout << "* Empty ground tiles: " << GetEmptyGround (diffused) << " *" << endl;
  cout << endl;

  cout << "--- Puzzle 1: Empty ground after 10 rounds ---" << endl;
  list<string> inputlines = ReadLines ("23-unstable-diffusion-input.txt");
  elves = ReadElfPositions (inputlines);
  cout << "Found " << elves.size() << " elves on "
    << inputlines.size() << " lines" << endl;
  diffused = RunRounds (elves, 10, needrounds);
  grmap = CreateGroundMap (diffused);
  cout << "After " << needrounds << " rounds" << endl;
  OutputMap (grmap);
  cout << "*** Empty ground tiles: " << GetEmptyGround (diffused) << " ***" << endl;

  return 0;
}

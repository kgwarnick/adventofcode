// Advent of Code 2022 Day 17: Pyroclastic Flow
// https://adventofcode.com/2022/day/17

#include <iostream>
#include <iomanip>
#include <map>
#include <vector>
#include <cstring>

using namespace std;

#include "fileread.hpp"


const long ChamberHeight = 32000;
const string ExampleJet = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";

int min4 (int a, int b, int c, int d) {
  return a < b && a < c && a < d ? a : b < c && b < d ? b : c < d ? c : d;
}
int max4 (int a, int b, int c, int d) {
  return a > b && a > c && a > d ? a : b > c && b > d ? b : c > d ? c : d;
}


struct Coord2D {
  int x;
  int y;
};

bool operator< (const Coord2D& a, const Coord2D& b) {
  return a.y < b.y ? true : a.y > b.y ? false : a.x < b.x;
}


class Rock {
 public:
  int width;
  int height;
  int x;
  int y;
  Rock () : width(0), height(0), x(0), y(0) { }
  virtual int GetLeftEdge () const { return x; }
  virtual int GetRightEdge () const { return x + width - 1; }
  virtual int GetLowerEdge () const { return y; }
  virtual int GetUpperEdge () const { return y + height - 1; }
  void SetShape (int index);
  void SetPos (int px, int py) {
    Move (px - x, py - y);   // Move to new position
  }
  vector<Coord2D> points;
  // Move the shape by moving all points
  void Move (int dx, int dy) {
    for (size_t i = 0; i < points.size(); i++) {
      points[i].x += dx;  points[i].y += dy;
    }
    x += dx;  y += dy;
  }
  // Rock covers this position?
  bool AtPos (int x, int y) const {
    for (size_t i = 0; i < points.size(); i++)
      if (points[i].x == x && points[i].y == y)  return true;
    return false;
  }
};

void Rock::SetShape (int index) {
  int oldx = x;  int oldy = y;
  x = 0;  y = 0;   // Set up the shape with (0, 0) reference
  switch (index % 5) {
    case 0:   // Horizontal bar 4x1
      points = vector<Coord2D> { {0,0}, {1,0}, {2,0}, {3,0} };
      width = 4;  height = 1;
      break;
    case 1:   // Cross shape
      points = vector<Coord2D> { {0,1}, {1,0}, {1,1}, {1,2}, {2,1} };
      width = 3;  height = 3;
      break;
    case 2:   // Lower-right angle
      points = { {0,0}, {1,0}, {2,0}, {2,1}, {2,2} };
      width = 3;  height = 3;
      break;
    case 3:   // Vertical bar 1x4
      points = vector<Coord2D> { {0,0}, {0,1}, {0,2}, {0,3} };
      width = 1;  height = 4;
      break;
    case 4:   // Block 2x2
      points = { {0,0}, {1,0}, {0,1}, {1,1} };
      width = 2;  height = 2;
      break;
  }
  Move (oldx, oldy);   // And move it to its location afterwards
}


/// \brief Properties of a rock tower
//
struct RockTower {
  long numrocks;
  long height;
};

/// \brief Floor heights described as individual height of every floor element
//
struct FloorHeights {
  short f[7];
  string str () const {
    string s = to_string (f[0]);
    for (int i = 1; i < 7; i++)  s += ", " + to_string(f[i]);
    return s;
  }
};

/// \brief Chamber state, consisting of shape, jet offset and floor height
//
struct ChamberState {
  char shape;   ///< Shape index 0...4
  unsigned short jetoffset;   ///< current position in the jet data
  FloorHeights flheight;   ///< Floor offsets, relative to the floor height
};

/// \brief Compare two chamber states and find out if the first is "less than" the second
//
bool operator< (const ChamberState& a, const ChamberState& b) {
  if (a.jetoffset < b.jetoffset)  return true;
  if (a.jetoffset > b.jetoffset)  return false;
  if (a.shape < b.shape)  return true;
  if (a.shape > b.shape)  return false;
  for (int i = 0; i < 7; i++) {
    if (a.flheight.f[i] < b.flheight.f[i]) return true;
    if (a.flheight.f[i] > b.flheight.f[i]) return false;
  }
  return false;   // not less than, but equal
}


/// \brief Data type to keep track of the chamber state
//
class Chamber {
 public:
  bool floor[ChamberHeight][7];
  int floorheight;
  int numrocks;
  bool fallingrock;
  string gasjet;
  size_t jetunits;
  Rock rock;   ///< Currently falling rock
  /// Dictionary of previously seen states, collected just before a new rock s selected
  map<ChamberState,RockTower> seenstates;
  /// Create a chamber with the specified gas stream
  Chamber (const string& gasstream) :
      floorheight(0), numrocks(0), fallingrock(false),
      gasjet(gasstream), jetunits(0) {
    memset (&floor, 0, sizeof (floor));
    for (int i = 0; i < 7; i++)  floor[0][i] = true;
  }
  void Draw (int down = 10) const;
  bool SpawnRock ();
  bool PushRock ();
  bool FallRock ();
  bool DoStep ();   ///< Perform the next push + fall step on the rock, create new rock if necessary
  int DoSteps (int numsteps);
  int FloorHeightAfterManyRocks (int numrestingrocks);
  long FloorHeightAfterVeryManyRocksShortcut (int numsim, long numrestingrocks);
  bool RockCrashed (const Rock& rock) const;   // Collision test for rock
  FloorHeights GetFloorLevels() const;
};


/// \brief Visualise the upper part of the chamber around the current floor level
//
void Chamber::Draw (int down) const {
  for (int y = fallingrock ? rock.GetUpperEdge() : floorheight + 5;
       y >= floorheight - down && y >= 0; y--) {
    cout << setw(5) << y << "|";
    for (int x = 0; x <= 6; x++) {
      // Draw rock or ground
      if (rock.AtPos (x, y))  cout << "@";
      else cout << (floor[y][x] ? '#' : '.');
    }
    cout << "|" << endl;
  }
}


/// \brief Start falling of the next rock
/// \return  true if a repeating pattern of (shape, jet, floor) was detected
//
bool Chamber::SpawnRock () {
  // Add current state to dictionary of all previously seen states
  ChamberState chst;   // { (char)(numrocks % 5), (unsigned short)(jetunits % gasjet.size()) };
  chst.shape = numrocks % 5;
  chst.jetoffset = jetunits % gasjet.size();
  chst.flheight = GetFloorLevels ();
  map<ChamberState,RockTower>::iterator oldstate = seenstates.find (chst);
  bool repeated = false;
  if (oldstate != seenstates.end()) {
    repeated = true;
    // cout << "Chamber state (numrocks: " << numrocks << ", jet: "
    //   << jetunits << "/" << jetunits % gasjet.size() << ") -> "
    //   << "(rocks: " << numrocks << ", height: " << floorheight
    //   << "): Already seen in (rocks: " << oldstate->second.numrocks
    //   << ", height: " << oldstate->second.height << ")" << endl;
    // if (oldstate->second.height == 62) Draw();
    // cout << "Diff: (rocks: " << numrocks - oldstate->second.numrocks
    //   << ", jet: " << jetunits - oldstate->first.jetoffset << ", floor: "
    //   << floorheight - oldstate->second.height << ")" << endl;
  } else {
    RockTower roctow { numrocks, floorheight };
    // cout << "Storing chamber state: (rocks: " << roctow.numrocks
    //   << ", height: " << roctow.height << ")" << endl;
    seenstates[chst] = roctow;
  }
  // Select next rock shape
  fallingrock = true;
  rock.SetShape (numrocks);
  rock.SetPos (2, floorheight + 4);
  return repeated;
}


/// \brief Push rock to left or right according to gasjet
//
bool Chamber::PushRock () {
  if (!fallingrock) { cerr << "Internal error: No rock falling" << endl; return false; }
  int dx = gasjet[jetunits++ % gasjet.size()] == '<' ? -1 : 1;
  // cout << "[" << jetunits << "] Push rock at " << rock.x << ", " << rock.y
  //   << " by dx = " << dx << endl;
  // Try movement, if it leads to a collision don't actually use it
  Rock pushed (rock);
  pushed.Move (dx, 0);
  bool movepossible = !RockCrashed (pushed);
  if (movepossible)  rock = pushed;   // Use new position
  return movepossible;
}


/// \brief Let the rock fall down one unit
/// \return true if the rock has come to a rest
//
bool Chamber::FallRock () {
  if (!fallingrock) { cerr << "Internal error: No rock falling" << endl; return false; }
  Rock fallen = rock;
  fallen.Move (0, -1);
  bool collision = RockCrashed (fallen);
  if (!collision)  rock = fallen;   // Use new position
  return collision;
}


/// \brief Do one step: A new rock enters the chamber, or the current one is moved
/// \return  true if the chamber's state is a repetition of an earlier state
//
bool Chamber::DoStep () {
  if (!fallingrock) {
    // We count spawning a rock as one step and return immediately from here
    bool repeated = SpawnRock ();
    // Draw ();
    return repeated;
  }
  PushRock ();
  if (FallRock ()) {
    numrocks++;
    // cout << "Rock " << numrocks << " has landed" << endl;
    // Make the rock part of the ground
    for (Coord2D p: rock.points) {
      if (p.x < 0 || p.x > 6 || p.y < 0 || p.y > ChamberHeight)  continue;   // outside
      floor[p.y][p.x] = true;
    }
    floorheight = max (floorheight, rock.GetUpperEdge());
    // cout << "New highest floor elevation: " << floorheight << endl;
    // No falling rock any more at the moment
    fallingrock = false;
  }
  // No repeated pattern could be detected because checks are only done
  // when a new rock enters the chamber
  return false;
}


/// \brief Number of rocks resting after doing a number of steps
//
int Chamber::DoSteps (int numsteps) {
  for (int n = 0; n < numsteps; n++) {
    DoStep ();
    // Draw ();
  }
  return numrocks;
}

/// \brief Do steps until a certain number of rocks are resting and get
///   the resulting height
//
int Chamber::FloorHeightAfterManyRocks (int numrestingrocks) {
  while (numrocks < numrestingrocks)  DoStep ();
  return floorheight;
}


/// \brief Do steps until a repetition of the state of the chamber is detected, then calculate the final height
/// \param numsim  Number of steps to simulate at most
/// \param numrestingrocks  Number of rocks to calculate the height for
//
long Chamber::FloorHeightAfterVeryManyRocksShortcut (int numsim, long numrestingrocks) {
  bool repetitionfound = false;
  long nsim;
  for (nsim = 0; nsim < numsim && !repetitionfound; nsim++) {
    repetitionfound = DoStep ();
  }
  if (!repetitionfound) {
    cerr << "Error: No repetition was detected for " << numsim << endl;
    return 0;
  }
  // Get the oldstate
  ChamberState currstate;
  currstate.shape = numrocks % 5;
  currstate.jetoffset = jetunits % gasjet.size();
  currstate.flheight = GetFloorLevels ();
  map<ChamberState,RockTower>::iterator oldstate = seenstates.find (currstate);
  if (oldstate == seenstates.end()) {
    cerr << "Internal error: Repeated state was detected and lost again?!" << endl;
    return 0;
  }
  cout << "Repetition found in simulation step " << nsim << endl;
  cout << "- Chamber state (rocks/shape: " << numrocks << "/" << numrocks % 5
    << ", jet/offset: " << jetunits << "/" << jetunits % gasjet.size() << ", floor: "
    << currstate.flheight.str() << ") -> "
    << "(rocks: " << numrocks << ", height: " << floorheight << ")" << endl;
  cout << "- Already seen in (shape: " << (int)oldstate->first.shape
    << ", jetoffset: " << oldstate->first.jetoffset
    <<" ) -> (rocks: " << oldstate->second.numrocks
    << ", height: " << oldstate->second.height << ")" << endl;
  long reprocks = numrocks - oldstate->second.numrocks;
  long repjet = jetunits - oldstate->first.jetoffset;
  long repheight = floorheight - oldstate->second.height;
  cout << "- Differences: (rocks: " << reprocks << ", jet: " << repjet
    << ", height: " << repheight << ")" << endl;
  // Calculate final tower height:
  //   height until repeated unit starts + n * repeated unit + rest to top
  // TODO
  // - How many repetitions do we need
  long needreps = (numrestingrocks - numrocks) / reprocks;
  long towerheight = floorheight + needreps * repheight;
  long remainingrocks = numrestingrocks - numrocks - needreps * reprocks;
  cout << "- Need repetitions: " << needreps << endl;
  cout << "- Need " << remainingrocks << " more rocks" << endl;
  // Get the remaining height by finding a suitable rock to end the tower,
  // it should already have been simulated
  ChamberState endchstate;
  RockTower enddata { 0, 0 };
  for (pair<ChamberState,RockTower> rt : seenstates) {
    // Search for a data with the right number of rocks
    // after the beginning of the repetition unit
    if (rt.second.numrocks == oldstate->second.numrocks + remainingrocks) {
      endchstate = rt.first;
      enddata = rt.second;
    }
  }
  cout << "- End rock state, should be rock " << oldstate->second.numrocks + remainingrocks
    << ": (shape: " << (int)endchstate.shape
    << ", jetoffset: " << endchstate.jetoffset
    <<" ) -> (rocks: " << enddata.numrocks
    << ", height: " << enddata.height << ")" << endl;
  long remheight = enddata.height - oldstate->second.height;
  cout << "- Additonal height: " << remheight << endl;
  towerheight += remheight;
  return towerheight;
}


bool Chamber::RockCrashed (const Rock& r) const {
  for (Coord2D p: r.points) {
    if (p.x < 0 || r.GetRightEdge() > 6)  return true;   // Crashed into left or right wall
    if (p.y < 0 || p.y > ChamberHeight)  continue;   // Cannot test outside the chamber
    if (floor[p.y][p.x])  return true;   // Rock and ground in same location
  }
  return false;   // No rock point collided with the floor
}


/// \brief Calculate height of floor tiles relative to the topmost tile = floorheight
//
FloorHeights Chamber::GetFloorLevels () const {
  FloorHeights flh;
  for (int x = 0; x < 7; x++) {
    flh.f[x] = floorheight;   // Start with endless void down to bottom
    for (int y = 0; y < 1000 && y <= floorheight; y++) {
      if (floor[floorheight-y][x]) { flh.f[x] = y; break; }
    }
  }
  return flh;
}


int main () {
  cout << "--- Example ---" << endl;
  Chamber chamber (ExampleJet);
  cout << "Start chamber:" << endl;
  chamber.Draw ();
  // - Do a certain number of simulation steps
  // int numrest = chamber.DoSteps (65);
  // cout << "Resting rocks: " << numrest << endl;
  int floorheight = chamber.FloorHeightAfterManyRocks (2022);
  cout << "Top layers of the end chamber:" << endl;
  chamber.Draw (32);
  cout << "* Floor height: " << floorheight << " *" << endl;
  cout << endl;

  cout << "Calculate height after 10^12 rocks" << endl;
  Chamber ExampleChamber (ExampleJet);
  // - To verify with the result of the first method:
  // long ohoh = ExampleChamber.FloorHeightAfterVeryManyRocksShortcut (2022, 2022);
  long babeltower = ExampleChamber.FloorHeightAfterVeryManyRocksShortcut (12000, 1000000L * 1000000L);
  cout << "* Floor height: " << babeltower << " *" << endl;

  cout << "--- Puzzle 1: Tower height after 2022 rocks ---" << endl;
  vector<string> InputLines = ReadLinesVector ("17-pyroclastic-flow-input.txt");
  Chamber InputChamber (InputLines[0]);
  floorheight = InputChamber.FloorHeightAfterManyRocks (2022);
  InputChamber.Draw ();
  cout << "*** Floor height: " << floorheight << " ***" << endl;
  cout << endl;

  cout << "--- Puzzle 2: Really big tower of 10^12 rocks ---" << endl;
  Chamber TestChamber (InputLines[0]);
  babeltower = TestChamber.FloorHeightAfterVeryManyRocksShortcut (24000, 1000000L * 1000000L);
  cout << "Simulated chamber:  Height: " << TestChamber.floorheight << ", number of gas jet blows: "
    << TestChamber.jetunits << endl;
  TestChamber.Draw ();
  cout << "*** Total floor height: " << babeltower << " ***" << endl;
}

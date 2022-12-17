// Advent of Code 2022 Day 17: Pyroclastic Flow
// https://adventofcode.com/2022/day/17

#include <iostream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <numeric>
#include <cstring>

using namespace std;

#include "fileread.hpp"


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


/// \brief Data type to keep track of the chamber state
//
class Chamber {
 public:
  bool floor[5000][7];
  int floorheight;
  int numrocks;
  bool fallingrock;
  string gasjet;
  size_t jetunits;
  Rock rock;   ///< Currently falling rock
  Chamber (const string& gasstream) :
      floorheight(0), numrocks(0), fallingrock(false),
      gasjet(gasstream), jetunits(0) {
    memset (&floor, 0, sizeof (floor));
    for (int i = 0; i < 7; i++)  floor[0][i] = true;
  }
  void Draw (int down = 10) const;
  void SpawnRock ();
  bool PushRock ();
  bool FallRock ();
  void DoStep ();   // Perform the next push + fall step on the rock
  int DoSteps (int numsteps);
  int FloorHeightAfterManyRocks (int numrestingrocks);
  bool RockCrashed (const Rock& rock) const;   // Collision test for rock
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
//
void Chamber::SpawnRock () {
  // Select next rock shape
  fallingrock = true;
  rock.SetShape (numrocks);
  rock.SetPos (2, floorheight + 4);
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
  if (movepossible) {
    rock = pushed;
    // cout << "[" << jetunits << "] Pushed rock to " << rock.x << ", " << rock.y << endl;
  }
  // else {
  //   cout << "[" << jetunits << "] Could not move rock " << rock.x << ", " << rock.y << endl;
  // }
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
  if (!collision) {
    rock = fallen;
    // cout << "Rock fell down one unit to " << rock.x << ", " << rock.y << endl;
  } // else {
  //   cout << "Rock has reached the ground at " << rock.x << ", " << rock.y << endl;
  //}
  return collision;
}


/// \brief Do one step: A new rock enters the chamber, or the current one is moved
//
void Chamber::DoStep () {
  if (!fallingrock) {
    SpawnRock ();
    // Draw ();
    return;   // Spawning a rock counts as one step?
  }
  PushRock ();
  if (FallRock ()) {
    numrocks++;
    // cout << "Rock " << numrocks << " has landed" << endl;
    // Make the rock part of the ground
    for (Coord2D p: rock.points) {
      if (p.x < 0 || p.x > 6 || p.y < 0 || p.y > 5000)  continue;   // outside
      floor[p.y][p.x] = true;
    }
    floorheight = max (floorheight, rock.GetUpperEdge());
    // cout << "New highest floor elevation: " << floorheight << endl;
    // No falling rock any more at the moment
    fallingrock = false;
  }
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


bool Chamber::RockCrashed (const Rock& r) const {
  for (Coord2D p: r.points) {
    if (p.x < 0 || r.GetRightEdge() > 6)  return true;   // Crashed into left or right wall
    if (p.y < 0 || p.y > 5000)  continue;   // Cannot test outside the chamber
    if (floor[p.y][p.x])  return true;   // Rock and ground in same location
  }
  return false;   // No rock point collided with the floor
}


int main () {
  cout << "--- Example ---" << endl;
  Chamber chamber (">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>");
  cout << "Start chamber:" << endl;
  chamber.Draw ();
  // int numrest = chamber.DoSteps (65);
  // cout << "Resting rocks: " << numrest << endl;
  int floorheight = chamber.FloorHeightAfterManyRocks (2022);
  cout << "Top layers of the end chamber:" << endl;
  chamber.Draw (32);
  cout << "* Floor height: " << floorheight << " *" << endl;
  cout << endl;

  cout << "--- Puzzle 1: Tower height after 2022 rocks ---" << endl;
  vector<string> InputLines = ReadLinesVector ("17-pyroclastic-flow-input.txt");
  Chamber InputChamber (InputLines[0]);
  floorheight = InputChamber.FloorHeightAfterManyRocks (2022);
  InputChamber.Draw ();
  cout << "*** Floor height: " << floorheight << " ***" << endl;
}

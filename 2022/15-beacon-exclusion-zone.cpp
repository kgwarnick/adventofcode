// Advent of Code 2022 Day 15 BEacon Exclusion Zone
// https://adventofcode.com/2022/day/15

#include <iostream>
#include <string>
#include <vector>
#include <numeric>

using namespace std;

#include "fileread.hpp"
#include "strsplit.hpp"

const vector<string> example = {
  "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
  "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
  "Sensor at x=13, y=2: closest beacon is at x=15, y=3",
  "Sensor at x=12, y=14: closest beacon is at x=10, y=16",
  "Sensor at x=10, y=20: closest beacon is at x=10, y=16",
  "Sensor at x=14, y=17: closest beacon is at x=10, y=16",
  "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
  "Sensor at x=2, y=0: closest beacon is at x=2, y=10",
  "Sensor at x=0, y=11: closest beacon is at x=2, y=10",
  "Sensor at x=20, y=14: closest beacon is at x=25, y=17",
  "Sensor at x=17, y=20: closest beacon is at x=21, y=22",
  "Sensor at x=16, y=7: closest beacon is at x=15, y=3",
  "Sensor at x=14, y=3: closest beacon is at x=15, y=3",
  "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
};


/// \brief Helper function to calculate the Manhattan distance of two integers
//
int manhdist (int x1, int y1, int x2, int y2) {
  return (x1 < x2 ? x2 - x1 : x1 - x2) + (y1 < y2 ? y2 - y1 : y1 - y2);
}


/// \brief Data type storing coordinates of a sensor and its closest beacon
//
struct Sensor {
  int sx;   ///< Sensor x position
  int sy;   ///< Sensor y position
  int bx;   ///< x Position of nearest beacon
  int by;   ///< y Position of nearest beacon
  int GetCurrRange() const { return manhdist (sx, sy, bx, by); }
  bool IsInRange (int x, int y) const {
    return manhdist (x, y, sx, sy) <= GetCurrRange(); }
  void Print () const {
    cout << "Sensor at (" << sx << ", " << sy
      << "), closest beacon at (" << bx << ", " << by
      << "), current sensor range " << GetCurrRange()
      << ", covering " << sx - GetCurrRange() << "..."
      << sx + GetCurrRange() << endl;
  }
};


/// \brief Read a sensor location and the location of the beacon it is locked on
//
vector<Sensor> ParseSensors (const vector<string>& lines) {
  vector<Sensor> sensors;
  for (string l : lines) {
    Sensor testsensor;
    int numscanned = sscanf (l.c_str(),
      "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d",
      &testsensor.sx, &testsensor.sy, &testsensor.bx, &testsensor.by);
    if (numscanned == 4)  sensors.push_back (testsensor);
    else {
      cerr << "Failed to read line (" << numscanned << " coordinates parsed)"
        << ":  " << l << endl;
    }
  }
  return sensors;
}


/// \brief How many points on a particular line are covered by the sensors
//
int GetSensorCoverageOnLine (const vector<Sensor>& sensors, int y) {
  if (sensors.empty())   return 0;
  // Left-most or right-most location possibly covered by any sensor is no
  // farther than every sensor's range to the left or right
  // (simplified by not considering if the y coordinate is in range at all)
  int leftmost = accumulate (sensors.cbegin(), sensors.cend(), sensors[0].sx,
    [] (int i, const Sensor& s) { return min (i, s.sx - s.GetCurrRange()); });
  int rightmost = accumulate (sensors.cbegin(), sensors.cend(), sensors[0].sx,
    [] (int i, const Sensor& s) { return max (i, s.sx + s.GetCurrRange()); });
  cout << "Have to test x = " << leftmost << " ... " << rightmost <<  endl;
  // Test all the possibly covered points of the line for sensor coverage
  int coverage = 0;
  for (int x = leftmost; x <= rightmost; x++) {
    bool inrange = false, isbeacon = false;
    for (const Sensor& s : sensors) {
      if (y == s.by && x == s.bx) {
        // This is a beacon location
        isbeacon = true;
        break;   // Everything worth knowing is known for this point, move on
      }
      if (s.IsInRange (x, y)) {
        inrange = true;
      }
    }
    // Count the location as covered if it is in range of any sensor and
    // not already occupied by a beacon
    // FIXME What if a location is occupied by sensor?! -- Not the case in puzzle input
    if (inrange && !isbeacon)  coverage++;
  }
  return coverage;
}


int main () {
  cout << "--- Example ---" << endl;
  vector<Sensor> sensors = ParseSensors (example);
  cout << "Read " << sensors.size() << " sensors" << endl;
  for (size_t i = 0; i < sensors.size (); i++)  sensors[i].Print();
  int cov = GetSensorCoverageOnLine (sensors, 10);
  cout << "* Sensor coverage: " << cov << " *" << endl;
  cout << endl;

  cout << "--- Puzzle 1: Where can't the signal originate ---" << endl;
  vector<string> lines = ReadLinesVector ("15-beacon-exclusion-zone-input.txt");
  sensors = ParseSensors (lines);
  cout << "Read " << sensors.size() << " sensors" << endl;
  for (size_t i = 0; i < sensors.size (); i++)  sensors[i].Print();
  cov = GetSensorCoverageOnLine (sensors, 2000000);
  cout << "*** Sensor coverage: " << cov << " ***" << endl;
}

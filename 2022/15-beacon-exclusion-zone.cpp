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
  int GetRangeLeftEdge (int y) const {
    if (y < sy - GetCurrRange() || y > sy + GetCurrRange()) return 0;
    return sx - GetCurrRange() + (y < sy ? sy - y : y - sy);
  }
  int GetRangeRightEdge (int y) const {
    if (y < sy - GetCurrRange() || y > sy + GetCurrRange()) return 0;
    return sx + GetCurrRange() - (y < sy ? sy - y : y - sy);
  }
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


/// \brief Data type to map an x coordinate to a transition from "covered" to "uncovered" state or "covered" to "uncovered"
//
struct RangeTransition {
  int x;
  int n;
};


/// \brief How many locations on a particular line and in the specified x range
///   are not covered by any sensor
//
list<int> FindUncoveredPointsOnLine (const vector<Sensor>& sensors, int y,
    int xmin, int xmax) {
  // Work through a line and observe changes into or out of a sensor range
  // Transitions are collected as pairs of x coordinate and number of ranges
  // at that location
  list<RangeTransition> transitions;
  // Collect all enter and exit points as change +1/-1
  for (const Sensor& s : sensors) {
    transitions.push_back (RangeTransition { s.GetRangeLeftEdge (y), +1 });
    transitions.push_back (RangeTransition { s.GetRangeRightEdge (y) + 1, -1 });
  }
  // Combine single changes to a list of number of ranges, in ascending order;
  // each entry describes the number of covering sensors at this point and
  // right of it, up to the next entry
  transitions.sort ([] (const RangeTransition& a, const RangeTransition& b) {
    return a.x < b.x; });
  // Note: Saving numranges is not really necessary
  vector<RangeTransition> numranges;
  // Collect non-covered locations while combining the list
  list<int> unoccupied;
  // Remember the last non-covered x coordinate, initialise to "invalid",
  // i.e. not currently collecting unoccupied points
  int lastx = xmin - 1;
  int ncovered = 0;
  for (RangeTransition rt : transitions) {
    // cout << rt.x << " -> " << rt.n << endl;
    // Add to last value if the x coordinate is the same
    if (numranges.size() > 0 && numranges.back().x == rt.x) {
      ncovered += rt.n;
      numranges.back().n += rt.n;
    }
    // Otherwise add a new entry
    else {
      ncovered += rt.n;
      numranges.push_back (RangeTransition { rt.x, ncovered });
    }
    // Remember points which are covered by no sensor
    if (ncovered == 0 && numranges.back().x >= xmin && numranges.back().x <= xmax) {
      // cout << "Found non-covered location(s) starting at "
      //   << numranges.back().x << ", " << y << endl;
      lastx = numranges.back().x;
    }
    // Stop a range of uncovered points, record all of them
    if (ncovered > 0 && lastx >= xmin) {
      // cout << "Range of non-covered location(s) stopped at "
      //   << numranges.back().x << ", " << y << endl;
      for (int i = lastx; i < numranges.back().x && i <= xmax; i++)
        unoccupied.push_back (i);
      lastx = xmin - 1;   // set to "invalid" again
    }
  }
  if (ncovered > 0)
    cerr << "Internal Error: Inconsistency: Unexpected signals at infinity: "
      << ncovered << endl;
  // Output sensor transitions found
  // for (RangeTransition rt : numranges) {
  //   cout << rt.x << " -> " << rt.n << endl;
  // }
  return unoccupied;
}


/// \brief Data type for a coordinate pair
//
struct Coord2D {
  int x;
  int y;
};


/// \brief Find all points covered by no sensor in the specified x and y range
//
list<Coord2D> FindUncoveredPoints (const vector<Sensor>& sensors,
    int xmin, int xmax, int ymin, int ymax) {
  list<Coord2D> uncov;
  for (int y = ymin; y <= ymax; y++) {
    list<int> l = FindUncoveredPointsOnLine (sensors, y, xmin, xmax);
    // if (l.size() > 0)  cout << "Number of unoccupied points on y = "
    //     << y << ": " << l.size() << endl;
    for (int x : l)  uncov.push_back (Coord2D { x, y });
  }
  return uncov;
}


int main () {
  cout << "--- Example ---" << endl;
  vector<Sensor> sensors = ParseSensors (example);
  cout << "Read " << sensors.size() << " sensors" << endl;
  for (size_t i = 0; i < sensors.size (); i++)  sensors[i].Print();
  int cov = GetSensorCoverageOnLine (sensors, 10);
  cout << "* Sensor coverage in y = 10: " << cov << " *" << endl;
  // list<int> beaconlocs = FindUncoveredPointsOnLine (sensors, 11, 0, 20);
  // for (int i : beaconlocs)  cout << "- Location: x = " << i << endl;
  cout << endl;

  list<Coord2D> beaconpositions = FindUncoveredPoints (sensors, 0, 20, 0, 20);
  for (Coord2D bpos : beaconpositions)
    cout << "* Beacon Position: x = " << bpos.x << ", y = " << bpos.y
      << " -> Tuning frequency = " << bpos.x * 4000000 + bpos.y << " *" << endl;
  cout << endl;

  cout << "--- Puzzle 1: Where can't the signal originate ---" << endl;
  vector<string> lines = ReadLinesVector ("15-beacon-exclusion-zone-input.txt");
  sensors = ParseSensors (lines);
  cout << "Read " << sensors.size() << " sensors" << endl;
  for (size_t i = 0; i < sensors.size (); i++)  sensors[i].Print();
  cov = GetSensorCoverageOnLine (sensors, 2000000);
  cout << "*** Sensor coverage: " << cov << " ***" << endl;
  // beaconlocs = FindUncoveredPointsOnLine (sensors, 2000000, 0, 4000000);
  // for (int i : beaconlocs)  cout << "- Location: x = " << i << endl;
  cout << endl;

  cout << "--- Puzzle 2: Where is the beacon ---" << endl;
  beaconpositions = FindUncoveredPoints (sensors, 0, 4000000, 0, 4000000);
  if (beaconpositions.size() != 1) {
    cerr << "Error: Expected one beacon position, but found "
      << beaconpositions.size() << endl;
  }
  for (Coord2D bpos : beaconpositions)
    cout << "*** Beacon Position: x = " << bpos.x << ", y = " << bpos.y
      << " -> Tuning frequency = " << bpos.x * 4000000L + bpos.y << " ***" << endl;
  return 0;
}

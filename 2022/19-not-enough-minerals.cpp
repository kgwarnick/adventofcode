// Advent of Code 2022 Day 19: Not Enough Minerals
// https://adventofocode.com/2022/day/19

#include <iostream>
#include <vector>
#include <string>
#include <iostream>
#include <chrono>

using namespace std;

#include "fileread.hpp"

int min (int a, int b) { return a < b ? a : b; }
int max (int a, int b) { return a > b ? a : b; }

/// \brief Integer division with rounding up to next higher integer
long int ceildiv (long int n, long int d) { return (n + d - 1) / d; }

/// \brief Mineral reservoir
//
struct Minerals {
  long ore;
  long clay;
  long obsidian;
  long geodes;
  void Print () const {
    cout << "Minerals (ore: " << ore << ", clay: " << clay
      << ", obsidian: " << obsidian << ", geode: " << geodes << ")";
  }
};

/// \brief Mineral requirements for different bot types
//
struct Blueprint {
  int id;
  int orecollneedore;
  int claycollneedore;
  int obsidiancollneedore;
  int obsidiancollneedclay;
  int geodecrackneedore;
  int geodecrackneedobsidian;
  void Print() const {
    cout << "Blueprint " << id << ":  ore robot needs " << orecollneedore << " ore,"
      << "  clay robot needs " << claycollneedore << " ore,"
      << "  obsidian robot needs " << obsidiancollneedore << " ore and "
        << obsidiancollneedclay << " clay,"
      << "  geode robot needs " << geodecrackneedore << " ore and "
        << geodecrackneedobsidian << " obsidian." << endl;
  }
};

/// \brief Available set of robots
struct RobotFleet {
  int orecoll;
  int claycoll;
  int obsidiancoll;
  int geodecrack;
  void Print() const {
    cout << "Robots (ore: " << orecoll << ", clay: " << claycoll
      << ", obsidian: " << obsidiancoll << ", geode: " << geodecrack << ")";
  }
};


/// \brief Simulate one time step and determine the best result
///
/// Recursion in units of "one time step",
/// takes much too long for actual inputs with more than about 20 iterations
//
unsigned long TryTimeStep (const Blueprint& blueprint, int step, int maxsteps, const RobotFleet& robots, const Minerals& minerals) {
  // Debug info
  //cout << "[" << step << "]  ";  robots.Print ();  cout << ",  ";  minerals.Print ();  cout << endl;
  // Prepare update mineral reservoir effective at the end of this step
  // (tests for enough minerals have to be done on the original numbers)
  Minerals collected = minerals;
  collected.ore += robots.orecoll;
  collected.clay += robots.claycoll;
  collected.geodes += robots.geodecrack;
  collected.obsidian += robots.obsidiancoll;
  // Time limit reached, return number of cracked geodes
  if (step >= maxsteps)  return minerals.geodes;
  // Choices what to build next
  unsigned long bestresult = minerals.geodes, testresult;
  RobotFleet testbots = robots;
  // - Produce ore-collecting robot
  if (minerals.ore >= blueprint.orecollneedore) {
    testbots.orecoll++;
    collected.ore -= blueprint.orecollneedore;
    testresult = TryTimeStep (blueprint, step + 1, maxsteps, testbots, collected);
    if (testresult > bestresult) { bestresult = testresult; }
    testbots.orecoll--;
    collected.ore += blueprint.orecollneedore;
  }
  // - Produce clay-collecting robot
  if (minerals.ore >= blueprint.claycollneedore) {
    testbots.claycoll++;
    collected.ore -= blueprint.claycollneedore;
    testresult = TryTimeStep (blueprint, step + 1, maxsteps, testbots, collected);
    if (testresult > bestresult) { bestresult = testresult; }
    testbots.claycoll--;
    collected.ore += blueprint.claycollneedore;
  }
  // - Produce obsidian-collecting robot
  if (minerals.ore >= blueprint.obsidiancollneedore && minerals.clay >= blueprint.obsidiancollneedclay) {
    testbots.obsidiancoll++;
    collected.ore -= blueprint.obsidiancollneedore;
    collected.clay -= blueprint.obsidiancollneedclay;
    testresult = TryTimeStep (blueprint, step + 1, maxsteps, testbots, collected);
    if (testresult > bestresult) { bestresult = testresult; }
    testbots.obsidiancoll--;
    collected.ore += blueprint.obsidiancollneedore;
    collected.clay += blueprint.obsidiancollneedclay;
  }
  // - Produce geode-cracking robot
  if (minerals.ore >= blueprint.geodecrackneedore && minerals.obsidian >= blueprint.geodecrackneedobsidian) {
    testbots.geodecrack++;
    collected.ore -= blueprint.geodecrackneedore;
    collected.obsidian -= blueprint.geodecrackneedobsidian;
    testresult = TryTimeStep (blueprint, step + 1, maxsteps, testbots, collected);
    if (testresult > bestresult) { bestresult = testresult; }
    testbots.geodecrack--;
    collected.ore += blueprint.geodecrackneedore;
    collected.obsidian += blueprint.geodecrackneedobsidian;
  }
  // Collect but do not build anything
  // (because not enough minerals or saving for a more expensive bot)
  testresult = TryTimeStep (blueprint, step + 1, maxsteps, testbots, collected);
  if (testresult > bestresult) { bestresult = testresult; }
  // Return best result from production choices tried above
  return bestresult;
}


/// \brief Build next bot and determine best result
///
/// Recursion in units of "one robot built",
/// including waiting until enough resources are available and skipping build
/// choices that would not improve the result any more
///
/// Build a certain robot if either enough resources are already available
/// or the collection rate is high enough to build one later.
/// (But only if there is at least one cycle left to use the bot afterwards.)
/// During the wait time and in the final buildstep minerals are collected
/// by the available robots.
//
unsigned long TryBuildStep (const Blueprint& blueprint, int step, int maxsteps, const RobotFleet& robots, const Minerals& minerals) {
  // Debug info
  // cout << "[" << step << "]  ";  robots.Print ();  cout << ",  ";  minerals.Print ();  cout << endl;
  // Time limit reached, return number of cracked geodes
  if (step >= maxsteps) {
    // cout << "[" << step << "] exit with geodes: " << minerals.geodes << endl;
    return minerals.geodes;
  }
  unsigned long bestresult = minerals.geodes, testresult;
  RobotFleet testbots = robots;
  Minerals collected;
  // Note:  All robots other than a geode cracker do not increase the result immediately,
  //   so producing them is only worth the ressources
  //   if there is time to build a geode cracking robot later,
  // How many robots of a kind are enough?
  //   There is no point building more robots of a kind that the demand
  //   of that ressource per cycle (example: if no robot costs more than 3 ore,
  //   3 ore robots will restock the reservoir in every step)
  // - Ore is needed by every type of robot, so calculate the maximum ore;
  // - The other resources are given by the matching blueprint entry
  // - You can never have enough geode crackers
  int enoughore = max (max (blueprint.orecollneedore, blueprint.claycollneedore),
                       max (blueprint.obsidiancollneedore, blueprint.geodecrackneedore));
  // Choices: What robot to build next?
  // - Produce ore-collecting robot
  if (robots.orecoll < enoughore &&
      minerals.ore + robots.orecoll * (maxsteps - step - 2) >= blueprint.orecollneedore) {
    int waittime = max (0, ceildiv ((blueprint.orecollneedore - minerals.ore), robots.orecoll));
    // cout << "[" << step << "] (ore) Need to wait " << waittime << endl;
    testbots.orecoll++;
    collected.ore = minerals.ore + robots.orecoll * (waittime + 1) - blueprint.orecollneedore;
    collected.clay = minerals.clay + robots.claycoll * (waittime + 1);
    collected.obsidian = minerals.obsidian + robots.obsidiancoll * (waittime + 1);
    collected.geodes = minerals.geodes + robots.geodecrack * (waittime + 1);
    testresult = TryBuildStep (blueprint, step + waittime + 1, maxsteps, testbots, collected);
    if (testresult > bestresult) { bestresult = testresult; }
    testbots.orecoll--;
  }
  // - Produce clay-collecting robot
  //   Producing a clay-collecting robot only pays off if
  //   (1) it can collect some clay,
  //   (2) an obsidian-collecting robot is built with that clay,
  //   (3) that collects some obsidian and
  //   (4) another geode-cracking robot is built
  //   (5) and can crack some geodes.
  //   So require additional 5 cycles
  if (robots.claycoll < blueprint.obsidiancollneedclay &&
      minerals.ore + robots.orecoll * (maxsteps - step - 6) >= blueprint.claycollneedore) {
    int waittime = max (0, ceildiv ((blueprint.claycollneedore - minerals.ore), robots.orecoll));
    // cout << "[" << step << "] (clay) Need to wait " << waittime << endl;
    testbots.claycoll++;
    collected.ore = minerals.ore + robots.orecoll * (waittime + 1) - blueprint.claycollneedore;
    collected.clay = minerals.clay + robots.claycoll * (waittime + 1);
    collected.obsidian = minerals.obsidian + robots.obsidiancoll * (waittime + 1);
    collected.geodes = minerals.geodes + robots.geodecrack * (waittime + 1);
    testresult = TryBuildStep (blueprint, step + waittime + 1, maxsteps, testbots, collected);
    if (testresult > bestresult) { bestresult = testresult; }
    testbots.claycoll--;
  }
  // - Produce obsidian-collecting robot
  //   only pays off if
  //   (1) at least one obsidian unit is collected
  //   (2) to build a geode-cracking robot
  //   (3) that cracks at least one geode
  //   Require 3 more cycles
  if (robots.obsidiancoll < blueprint.geodecrackneedobsidian &&
      minerals.ore + robots.orecoll * (maxsteps - step - 4) >= blueprint.obsidiancollneedore &&
      minerals.clay + robots.claycoll * (maxsteps - step - 4) >= blueprint.obsidiancollneedclay) {
    int waittime = max (0, ceildiv ((blueprint.obsidiancollneedore - minerals.ore), robots.orecoll));
    waittime = max (waittime, ceildiv ((blueprint.obsidiancollneedclay - minerals.clay), robots.claycoll));
    // cout << "[" << step << "] (obsidian) Need to wait " << waittime << endl;
    testbots.obsidiancoll++;
    collected.ore = minerals.ore + robots.orecoll * (waittime + 1) - blueprint.obsidiancollneedore;
    collected.clay = minerals.clay + robots.claycoll * (waittime + 1) - blueprint.obsidiancollneedclay;
    collected.obsidian = minerals.obsidian + robots.obsidiancoll * (waittime + 1);
    collected.geodes = minerals.geodes + robots.geodecrack * (waittime + 1);
    testresult = TryBuildStep (blueprint, step + waittime + 1, maxsteps, testbots, collected);
    if (testresult > bestresult) { bestresult = testresult; }
    testbots.obsidiancoll--;
  }
  // - Produce geode-cracking robot
  if (minerals.ore + robots.orecoll * (maxsteps - step - 2) >= blueprint.geodecrackneedore &&
      minerals.obsidian + robots.obsidiancoll * (maxsteps - step - 2) >= blueprint.geodecrackneedobsidian) {
    int waittime = max (0, ceildiv ((blueprint.geodecrackneedore - minerals.ore), robots.orecoll));
    waittime = max (waittime, ceildiv ((blueprint.geodecrackneedobsidian - minerals.obsidian), robots.obsidiancoll));
    // cout << "[" << step << "] (geode) Need to wait " << waittime << endl;
    testbots.geodecrack++;
    collected.ore = minerals.ore + robots.orecoll * (waittime + 1) - blueprint.geodecrackneedore;
    collected.clay = minerals.clay + robots.claycoll * (waittime + 1);
    collected.obsidian = minerals.obsidian + robots.obsidiancoll * (waittime + 1) - blueprint.geodecrackneedobsidian;
    collected.geodes = minerals.geodes + robots.geodecrack * (waittime + 1);
    testresult = TryBuildStep (blueprint, step + waittime + 1, maxsteps, testbots, collected);
    if (testresult > bestresult) { bestresult = testresult; }
    testbots.geodecrack--;
  }
  // Build nothing, just collect more material and crack geodes
  collected.ore = minerals.ore + robots.orecoll * (maxsteps - step);
  collected.clay = minerals.clay + robots.claycoll * (maxsteps - step);
  collected.obsidian = minerals.obsidian + robots.obsidiancoll * (maxsteps - step);
  collected.geodes = minerals.geodes + robots.geodecrack * (maxsteps - step);
  if ((unsigned long)collected.geodes > bestresult) { bestresult = collected.geodes; }
  // Best option
  return bestresult;
}


const vector<string> ExampleLines = {
  "Blueprint 1:"
  "  Each ore robot costs 4 ore."
  "  Each clay robot costs 2 ore."
  "  Each obsidian robot costs 3 ore and 14 clay."
  "  Each geode robot costs 2 ore and 7 obsidian.",
  "Blueprint 2:"
  "  Each ore robot costs 2 ore."
  "  Each clay robot costs 3 ore."
  "  Each obsidian robot costs 3 ore and 8 clay."
  "  Each geode robot costs 3 ore and 12 obsidian."
};


/// \brief Read robot blueprints from lines
//
vector<Blueprint> ParseBlueprints (const vector<string>& lines) {
  vector<Blueprint> blueprints (lines.size());
  for (size_t i = 0; i < lines.size(); i++) {
    Blueprint bp;
    int numscanned = sscanf (lines[i].c_str(),
      "Blueprint %d : Each ore robot costs %d ore. Each clay robot costs %d ore."
      " Each obsidian robot costs %d ore and %d clay."
      " Each geode robot costs %d ore and %d obsidian.",
      &bp.id, &bp.orecollneedore, &bp.claycollneedore,
      &bp.obsidiancollneedore, &bp.obsidiancollneedclay,
      &bp.geodecrackneedore, &bp.geodecrackneedobsidian
    );
    if (numscanned != 7) {
      cerr << "Error: Blueprint " << i + 1
        << "Failed to parsed 7 elements from line, found " << numscanned << endl;
    }
    blueprints[i] = bp;
  }
  return blueprints;
}


/// \brief Determine quality levels of all blueprints and return the total
//
unsigned long TestBlueprints (const vector<string>& bluelines, int maxsteps,
    RobotFleet startrobots, Minerals startressources) {
  vector<Blueprint> blueprints = ParseBlueprints (bluelines);
  cout << "Read " << blueprints.size() << " blueprints" << endl;
  unsigned long totalquality = 0;
  for (Blueprint bluepr : blueprints) {
    bluepr.Print ();
    chrono::time_point<chrono::steady_clock> starttp = chrono::steady_clock::now();
    // unsigned long cracknum = TryTimeStep (bluepr, 0, maxsteps, startrobots, startressources);
    unsigned long cracknum = TryBuildStep (bluepr, 0, maxsteps, startrobots, startressources);
    chrono::time_point<chrono::steady_clock> endtp = chrono::steady_clock::now();
    cout << "- Number of open geodes: " << cracknum
      << ", quality level = " << cracknum * bluepr.id
      << "  (Time taken to calculate: "
      << chrono::duration_cast<chrono::microseconds> (endtp - starttp) .count()
      << " microseconds)" << endl;
    totalquality += cracknum * bluepr.id;
  }
  return totalquality;
}


/// \brief Run blueprints and multiply the results
//
unsigned long RunBlueprints (const vector<string>& bluelines, int maxsteps,
    RobotFleet startrobots, Minerals startressources) {
  vector<Blueprint> blueprints = ParseBlueprints (bluelines);
  cout << "Read " << blueprints.size() << " blueprints" << endl;
  unsigned long geodeproduct = 1;
  for (Blueprint bluepr : blueprints) {
    bluepr.Print ();
    chrono::time_point<chrono::steady_clock> starttp =
      chrono::steady_clock::now();
    // unsigned long cracknum = TryTimeStep (bluepr, 0, maxsteps, startrobots, startressources);
    unsigned long cracknum = TryBuildStep (bluepr, 0, maxsteps, startrobots, startressources);
    chrono::time_point<chrono::steady_clock> endtp =
      chrono::steady_clock::now();
    cout << "- Number of open geodes: " << cracknum
      << "  (Time taken to calculate: "
      << chrono::duration_cast<chrono::microseconds> (endtp - starttp) .count()
      << " microseconds)" << endl;
    geodeproduct *= cracknum;
  }
  return geodeproduct;
}


int main () {
  cout << "--- Example ---" << endl;
  unsigned long sumquality = TestBlueprints (ExampleLines, 24,
    RobotFleet { 1, 0, 0, 0 }, Minerals { 0, 0, 0, 0 });
  cout << "* Sum of quality levels: " << sumquality << " *" << endl;
  cout << endl;
  vector<string> selectedprints = { ExampleLines[0], ExampleLines[1] };
  unsigned long product = RunBlueprints (selectedprints, 32,
    RobotFleet { 1, 0, 0, 0 }, Minerals { 0, 0, 0, 0 });
  cout << "* Product of two blueprints: " << product << " *" << endl;
  cout << endl;

  cout << "--- Puzzle 1: Find best blueprint ---" << endl;
  vector<string> inputlines = ReadLinesVector ("19-not-enough-minerals-input.txt");
  sumquality = TestBlueprints (inputlines, 24,
    RobotFleet { 1, 0, 0, 0 }, Minerals { 0, 0, 0, 0 });
  cout << "*** Sum of quality levels: " << sumquality << " ***" << endl;
  cout << endl;

  cout << "--- Puzzle 2: Run three blueprints and multiply the results ---" << endl;
  selectedprints = { inputlines[0], inputlines[1], inputlines[2] };
  product = RunBlueprints (selectedprints, 32,
    RobotFleet { 1, 0, 0, 0 }, Minerals { 0, 0, 0, 0 });
  cout << "*** Product of first three blueprints: " << product << " ***" << endl;
}

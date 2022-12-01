// Advent of Code 2022 Day 1: Calorie Counting
// https://adventofcode.com/2022/day/1

#include "strsplit.hpp"
#include "fileread.hpp"

#include <list>
#include <map>
#include <string>
#include <iostream>

using namespace std;


/// \brief Build groups of successive lines, empty lines separate groups
//
list<list<string> > LineGroups (list<string> lines) {
  list<list<string> > lls;
  list<string> ls;
  for (string s : lines) {
    // if the line is empty, a group ends here ...
    if (s.size() == 0) {
      // but only if it contains at least one line
      if (ls.size() > 0)  lls.push_back (ls);
      ls.clear ();   // Prepare for the next group by emptying the list
      continue;
    }
    ls.push_back (s);   // Append this non-empty line to the current group
  }
  // Append the last group if it is not empty
  if (ls.size() > 0)  lls.push_back (ls);
  // Return the list of groups
  return lls;
}


/// \brief Find line groups with the greatest sum
//
map <int, unsigned long int> GetTopThreeCalorieCarriers (list<string> lines) {
  list<list<string> > groups = LineGroups (lines);
  map <int, unsigned long int> topcarriers;
  topcarriers[1] = 0;  topcarriers[1] = 0;  topcarriers[2] = 0;
  for (list<string> gr : groups) {
    unsigned long int calories = 0;
    for (string s : gr)  calories += atol (s.c_str());
    if (calories > topcarriers[1]) {
      topcarriers[3] = topcarriers[2];
      topcarriers[2] = topcarriers[1];
      topcarriers[1] = calories;
    } else if (calories > topcarriers[2]) {
      topcarriers[3] = topcarriers[2];
      topcarriers[2] = calories;
    } else if (calories > topcarriers[3]) {
      topcarriers[3] = calories;
    }
  }
  return topcarriers;
}


int main () {
  cout << "--- Example ---" << endl;
  string exampleinput = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n"
    "7000\n8000\n9000\n\n10000\n";
  list<string> examplelines = SplitString (exampleinput, "\n");
  cout << "Largest sum of calories in one group: "
       << GetTopThreeCalorieCarriers (examplelines) [1] << ", "
       << GetTopThreeCalorieCarriers (examplelines) [2] << ", "
       << GetTopThreeCalorieCarriers (examplelines) [3] << endl << endl;

  cout << "--- Puzzle 1/2: Three largest calorie carriers ---" << endl;
  examplelines = ReadLines ("01-calorie-counting-input.txt");
  map <int, unsigned long int> carriers =
    GetTopThreeCalorieCarriers (examplelines);
  cout << "*** Largest sum of calories in one group: "
       << carriers[1] << " + " << carriers[2] << " + " << carriers[3] << " = "
       << carriers[1] + carriers[2] + carriers[3] << " ***" << endl << endl;
  return 0;
}

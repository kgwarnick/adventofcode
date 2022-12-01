// Advent of Code 2022 Day 1: Calorie Counting
// https://adventofcode.com/2022/day/1

#include "strsplit.hpp"
#include "fileread.hpp"

#include <list>
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


/// \brief Find line group with the greatest sum
//
unsigned long int GetLargestCalorieCarrier (list<string> lines) {
  list<list<string> > groups = LineGroups (lines);
  unsigned long int maxcalories = 0;
  for (list<string> gr : groups) {
    unsigned long int calories = 0;
    for (string s : gr)  calories += atol (s.c_str());
    if (calories > maxcalories)  maxcalories = calories;
  }
  return maxcalories;
}


int main () {
  cout << "--- Example ---" << endl;
  string exampleinput = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n"
    "7000\n8000\n9000\n\n10000\n";
  list<string> examplelines = SplitString (exampleinput, "\n");
  cout << "Largest sum of calories in one group: "
       << GetLargestCalorieCarrier (examplelines) << endl << endl;

  cout << "--- Puzzle 1: Largest calorie carrier ---" << endl;
  examplelines = ReadLines ("01-calorie-counting-input.txt");
  cout << "*** Largest sum of calories in one group: "
       << GetLargestCalorieCarrier (examplelines) << " ***" << endl << endl;
  return 0;
}

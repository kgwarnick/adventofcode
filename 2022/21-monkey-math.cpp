// Advent of Code 2022 Day 21: Monkey Math
// https://adventofocode.com/2022/day/21

#include <iostream>
#include <vector>
#include <map>
#include <string>
#include <iostream>
#include <sstream>

using namespace std;

#include "fileread.hpp"


/// \brief Class to describes a monkey's action
//
class MathMonkey {
 public:
  string name;
  bool isoperation;
  long int n;   ///< the number to provide as result
  char operation;
  string ainput;   ///< first monkey to provide input
  string binput;   ///< second monkey to provid input
  bool isresultavail;   ///< Result already stored?
  MathMonkey () : isoperation(false), n(0),
    operation(' '), ainput(""), binput("") { }
  /// \brief Read monkey's job from the input line
  bool Learn (const string& instructions);
  /// \brief Get a monkey description
  string str () const {
    return name + ": " + (isresultavail ? "*" : "-") + "  " +
      (isoperation ? ainput + " " + operation + " " + binput +
                       (isresultavail ? " = " + n : "") :
                     to_string (n));
  }
};


bool MathMonkey::Learn (const string& instructions) {
  stringstream ss (instructions);
  ss >> name;   if (name[name.size() - 1] == ':')  name.erase (name.size() - 1);
  string s1;
  ss >> s1;
  if (isdigit (s1[0])) {   // a simple number as result
    n = stoi (s1);  isoperation = false;  isresultavail = true;
  }
  else {   // an operation involving two other monkeys
    ainput = s1;
    ss >> operation >> binput;
    isoperation = true;  isresultavail = false;
  }
  return true;   // successfully read
}


/// \brief Determine the result of the monkey calculation
//
long int DoMonkeyCalculation (const vector<string>& lines) {
  // Parse all monkeys from input lines
  cout << "- Number of monkeys: " << lines.size() << endl;
  size_t resultsavail = 0;   // Count available results
  map<string,MathMonkey> monkeys;
  for (size_t i = 0; i < lines.size(); i++) {
    MathMonkey mm;
    if (mm.Learn (lines[i])) {
      monkeys[mm.name] = mm;
      if (mm.isresultavail)  resultsavail++;
      // cout << mm.str() << endl;
    }
    else
      cerr << "Failed to read monkey instruction on line " << i + 1 << endl;
  }
  // Try to resolve all results
  cout << "- Available results after reading monkey instructions: "
    << resultsavail << endl;
  size_t numpasses = 0;  bool resolvedsome = true;
  while (resolvedsome && resultsavail < lines.size() && numpasses < lines.size()) {
    resolvedsome = false;
    for (pair<string,MathMonkey> mm : monkeys) {
      if (!mm.second.isresultavail && mm.second.isoperation) {
        MathMonkey a = monkeys[mm.second.ainput];
        MathMonkey b = monkeys[mm.second.binput];
        if (a.isresultavail && b.isresultavail) {
          switch (mm.second.operation) {
            case '+':  mm.second.n = a.n + b.n;  break;
            case '-':  mm.second.n = a.n - b.n;  break;
            case '*':  mm.second.n = a.n * b.n;  break;
            case '/':  mm.second.n = a.n / b.n;
              if (((double)a.n / (double)b.n) - (a.n / b.n) > 0.000001) {
                cerr << "Warning: non-integer division for monkey "
                  << mm.second.name << ": " << a.n << " / " << b.n << " = "
                  << ((double)a.n / (double)b.n) << endl;
              }
              break;
            default:  cerr << "Error: Unknown operation for monkey " << mm.second.name << endl;
          }
          mm.second.isresultavail = true;
          monkeys[mm.first] = mm.second;   // Write back to monkey map
          resultsavail++;
          resolvedsome = true;
        }
      }
    }
    // cout << "- - Available results after " << numpasses << " resolver passes: "
    //   << resultsavail << endl;
    numpasses++;
  }
  cout << "- Available results after " << numpasses << " resolver passes: "
    << resultsavail << endl;
  // cout << "- Result of \"root\": " << monkeys["root"].n << endl;
  return monkeys["root"].n;
}


const vector<string> examplelines = {
  "root: pppw + sjmn",
  "dbpl: 5          ",
  "cczh: sllz + lgvd",
  "zczc: 2          ",
  "ptdq: humn - dvpt",
  "dvpt: 3          ",
  "lfqf: 4          ",
  "humn: 5          ",
  "ljgn: 2          ",
  "sjmn: drzm * dbpl",
  "sllz: 4          ",
  "pppw: cczh / lfqf",
  "lgvd: ljgn * ptdq",
  "drzm: hmdt - zczc",
  "hmdt: 32"
};

int main () {
  cout << "--- Example ---" << endl;
  long int result = DoMonkeyCalculation (examplelines);
  cout << "* Result of \"root\": " << result << " *" << endl;
  cout << endl;

  cout << "--- Puzzle 1: Predict root result ---" << endl;
  vector<string> inputlines = ReadLinesVector ("21-monkey-math-input.txt");
  result = DoMonkeyCalculation (inputlines);
  cout << "*** Result of \"root\": " << result << " ***" << endl;
}

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
                       (isresultavail ? string (" = ") + to_string (n) : "") :
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


/// \brief Build monkey map/dictionary from input lines
//
map<string,MathMonkey> BuildMonkeyDict (const vector<string>& lines) {
  map<string,MathMonkey> monkeymap;
  for (size_t i = 0; i < lines.size(); i++) {
    MathMonkey mm;
    if (mm.Learn (lines[i]))  monkeymap[mm.name] = mm;
    else cerr << "Failed to read monkey instruction on line " << i + 1 << endl;
  }
  return monkeymap;
}


/// \brief Determine the result of the monkey calculation
//
long int DoMonkeyCalculation (const vector<string>& lines) {
  // Parse all monkeys from input lines
  map<string,MathMonkey> monkeys = BuildMonkeyDict (lines);
  cout << "- Number of monkeys: " << monkeys.size() << endl;
  // Count already available results
  size_t resultsavail = 0;
  for (pair<string,MathMonkey> mp : monkeys)
    if (mp.second.isresultavail)  resultsavail++;
  cout << "- Available results after reading monkey instructions: "
  // Try to resolve all remaining results
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


/// \brief Find the appropriate operand value so that the expected result is achieved
/// \param resultmonkey  Monkey whose calculation to analyse
/// \param varmonkey  Monkey whose value can be varied to change the result
/// \param expected  Result needed for the calculation
/// \param depth  Recursion depth, for logging purposes
//
long int FindRequiredOperand (map<string,MathMonkey>& monkeymap,
    const string& resultmonkey, const string& varmonkey, long int expected,
    unsigned int depth = 0) {
  // Base case / Exit test:
  //   if this is the special monkey to vary, the target value was found
  if (resultmonkey == varmonkey) {
    cout << "Info: Found result " << expected << " for \"" << varmonkey
      << "\" at recursion depth " << depth << endl;
    return expected;
  }
  // Otherwise recurse and find the operands to produce this value
  const MathMonkey& resm = monkeymap[resultmonkey];
  const MathMonkey& am = monkeymap[monkeymap[resultmonkey].ainput];
  const MathMonkey& bm = monkeymap[monkeymap[resultmonkey].binput];
  cout << "Info: Depth " << depth << ", need to find result " << expected
    << " for operation " << resm.operation << " of monkey " << resultmonkey << endl;
  // cout << "Left operand of \"" << resultmonkey << "\" is \"" << monkeymap[resultmonkey].ainput << "\" "
  //   << (am.isresultavail ? "(available)" : "(not available)") << ":  " << am.n << endl;
  // cout << "Right operand of \"" << resultmonkey << "\" is \"" << monkeymap[resultmonkey].binput << "\" "
  //   << (bm.isresultavail ? "(available)" : "(not available)") << ":  " << bm.n << endl;
  if (am.isresultavail && bm.isresultavail) {
    cerr << "Internal Error:  Both operands are available for " << resm.name << endl;
    return 0;
  }
  if (!am.isresultavail && !bm.isresultavail) {
    cerr << "Error:  Both operands are unavailable for " << resm.name
      << ", unsupported setup, cannot resolve" << endl;
    return 0;
  }
  long int targetval = 0;
  if (am.isresultavail && !bm.isresultavail) {
    // left operand is available, find value for right one
    switch (resm.operation) {
      case '=':  targetval = am.n;  break;   // Solve   a = x
      case '+':  targetval = expected - am.n;  break;   // Solve   expected = a + x
      case '-':  targetval = am.n - expected;  break;   // Solve   expected = a - x
      case '*':  targetval = expected / am.n;  break;   // Solve   expected = a * x
      case '/':  targetval = am.n / expected;  break;   // Solve   expected = a / x
      default:
        cerr << "Error: Unknown operation for monkey " << resm.name
          << ": " << resm.operation << endl;
    }
    return FindRequiredOperand (monkeymap, bm.name, varmonkey, targetval, depth + 1);
  }
  if (!am.isresultavail && bm.isresultavail) {
    // right operand is available, find value for left one
    switch (resm.operation) {
      case '=':  targetval = bm.n;  break;   // Solve   x = b
      case '+':  targetval = expected - bm.n;  break;   // Solve  expected = x + b
      case '-':  targetval = bm.n + expected;  break;   // Solve  expected = x - b
      case '*':  targetval = expected / bm.n;  break;   // Solve  expected = x * b
      case '/':  targetval = bm.n * expected;  break;   // Solve  expected = x / b
      default:
        cerr << "Error: Unknown operation for monkey " << resm.name
          << ": " << resm.operation << endl;
    }
    return FindRequiredOperand (monkeymap, am.name, varmonkey, targetval, depth + 1);
  }
  // Should never get here
  cerr << "Internal Error: Should never get here for " << resm.name << ": "
    << "a.isresultavail = " << am.isresultavail
    << ", b.isresultavail = " << bm.isresultavail << endl;
  return 0;
}


/// \brief Find the appropriate number for a certain monkey so the "root"
///   monkey gets two identical operands
//
long int FindMonkeySolution (const vector<string>& lines,
    const string& varmonkey = "humn") {
  // Parse all monkeys from input lines
  map<string,MathMonkey> monkeys = BuildMonkeyDict (lines);
  cout << "- Number of monkeys: " << monkeys.size() << endl;
  // Change special variable monkey to "result not available"
  monkeys[varmonkey].isresultavail = false;
  // Change root monkey to equality test
  monkeys["root"].operation = '=';
  // Count already available results
  size_t resultsavail = 0;
  for (pair<string,MathMonkey> mp : monkeys)
    if (mp.second.isresultavail)  resultsavail++;
  cout << "- Available results after reading monkey instructions: "
  // Try to resolve all remaining results
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
  // Find results recursively so that root's equality test passes
  return FindRequiredOperand (monkeys, "root", varmonkey, /* true = */ 1, 0);
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

  cout << "Pass equality test of root" << endl;
  FindMonkeySolution (examplelines);
  cout << "* Number for \"humn\": " << result << " *" << endl;
  cout << endl;

  cout << "--- Puzzle 1: Predict root result ---" << endl;
  vector<string> inputlines = ReadLinesVector ("21-monkey-math-input.txt");
  result = DoMonkeyCalculation (inputlines);
  cout << "*** Result of \"root\": " << result << " ***" << endl;
  cout << endl;

  cout << "--- Puzzle 2: Number to pass root equality test ---" << endl;
  result = FindMonkeySolution (inputlines);
  cout << "*** Number for \"humn\": " << result << " ***" << endl;
  return 0;
}

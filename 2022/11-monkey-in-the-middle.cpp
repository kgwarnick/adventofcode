// Advent of Code 2022 Day 11 Monkey in the Middle
// https://adventofcode.com/2022/day/11

#include <list>
#include <vector>
#include <string>
#include <sstream>
#include <iostream>
#include <algorithm>
#include <numeric>

#include "strsplit.hpp"
#include "fileread.hpp"

using namespace std;

/// \brief Item class with a worry level field
//
class Item {
 public:
  unsigned long int WorryLevel;
  Item (unsigned long int worrylevel = 999) : WorryLevel (worrylevel) { }
  string str();
};

string Item::str() {
  stringstream ss;
  ss << WorryLevel;
  return ss.str();
}


/// \brief An operation that is applied to a worry level
//
enum MonkeyOperator {
  Noop = 0, Plus, Mult, Square
};

string MonkeyOperatorName (MonkeyOperator op) {
  if (op == Noop)  return "Noop";
  else if (op == Plus)  return "+";
  else if (op == Mult)  return "*";
  else if (op == Square)  return "Squared";
  else return "Unknown";
}

unsigned long int ApplyOperation (unsigned long int oldlevel, MonkeyOperator oper, int operand) {
  if (oper == Noop)  return oldlevel;
  else if (oper == Plus)  return oldlevel + operand;
  else if (oper == Mult)  return oldlevel * operand;
  else if (oper == Square)  return oldlevel * oldlevel;
  cerr << "Unknown operator: " << oper << endl;
  return oldlevel;   // return unmodified value as fallback
}


/// \brief A test performed on an item's worry level
//
enum MonkeyTest {
  AlwaysFalse = 0, AlwaysTrue = 1, Divisible
};

string MonkeyTestName (MonkeyTest op) {
  if (op == AlwaysTrue)  return "True";
  else if (op == Divisible)  return "DivisibleBy?";
  else return "Unknown";
}

bool ApplyTest (unsigned long int worrylevel, MonkeyTest testoper, int operand) {
  if (testoper == AlwaysTrue)  return true;
  else if (testoper == AlwaysFalse)  return false;
  else if (testoper == Divisible)  return worrylevel % operand == 0;
  cerr << "Unknown test operator: " << testoper << endl;
  return false;   // default to false
}


/// \brief Monkey class with a list of items in possession, operator and test what to do next
//
class Monkey {
 public:
  list<Item> Items;
  MonkeyOperator  Operation;   ///< Operation to do when starting to inspect an item
  int             Operand;   ///< Additional operand for the operation
  MonkeyTest      TestOperator;   ///< Test to decide to whom to throw an item
  int             TestOperand;   ///< Additional operand for the test
  int   DestTrue;   ///< Throw to this monkey if the test is true
  int   DestFalse;   ///< Throw to this monkey if the test is false
  int   InspectionCount;   ///< How many items did the monkey inspect and throw

  /// \brief Store worry levels modulo this number, to keep the numbers low.
  ///
  /// This should be set to the least common multiple (or simply the product)
  /// of the divisors the "divisible by" test rules of all monkeys.
  /// It will only produce corect results if ThrowNextItem() or ThrowItems()
  /// are called with `divideworries=false`, however.
  /// Otherwise the integer division in every turn (division by 3 followed
  /// by rounding off to the nearest integer) can change the factorisation
  /// of the original number so that divisibility is not preserved.
  unsigned long int WorryModulo;

  size_t Parse (size_t currline, const vector<string> lines);
  Monkey () : Operation(Noop), Operand(0),
    TestOperator(AlwaysTrue), TestOperand(0),
    DestTrue(-1), DestFalse(-1), InspectionCount(0), WorryModulo(1)  { }
  void ParseItems (const string& s);
  void ParseOperation (const string& s);
  void ParseTest (const string& s);
  void Print ();
  size_t GetItemCount () const { return Items.size(); }
  bool ThrowNextItem (vector<Monkey>& monkeys, bool divideworries, int loglevel = 0);
  int ThrowItems (vector<Monkey>& monkeys, bool divideworries = true, int loglevel = 0);
};


size_t Monkey::Parse (size_t currline, const vector<string> lines) {
  stringstream ss (lines[currline]);
  string word1, word2, word3;
  // Read items
  if (lines[currline].substr (0, 18) != "  Starting items: ") {
    cerr << "Error: Did not find item list in line " << currline << endl;
    return currline + 1;
  }
  ParseItems (lines[currline++].substr (18));
  // Read Operation
  if (lines[currline].substr (0, 23) != "  Operation: new = old ") {
    cerr << "Error: Did not find operation in line " << currline << endl;
    return currline + 1;
  }
  ParseOperation (lines[currline++].substr (23));
  // Read Test
  if (lines[currline].substr (0, 8) != "  Test: ") {
    cerr << "Error: Did not find test in line " << currline << endl;
    return currline + 1;
  }
  ParseTest (lines[currline++].substr (8));
  // Parse throw destination for test is true
  if (lines[currline].substr (0, 29) != "    If true: throw to monkey ") {
    cerr << "Error: Did not find destination in line " << currline << endl;
    return currline + 1;
  }
  DestTrue = stoi (lines[currline++].substr (29));
  // Parse throw destination for test is false
  if (lines[currline].substr (0, 30) != "    If false: throw to monkey ") {
    cerr << "Error: Did not find destination in line " << currline << endl;
    return currline + 1;
  }
  DestFalse = stoi (lines[currline++].substr (30));
  // Return the next line to parse
  return currline;
}

void Monkey::ParseItems (const string& s) {
  stringstream ss (s);
  while (ss.good()) {
    int number;
    ss >> number;
    Items.push_back (Item (number));
    if (!ss.good()) break;
    string comma;
    ss >> comma;
    if (comma != ",")
      cerr << "Warning:  A comma was not a comma: " << comma << endl;
  }
}

void Monkey::ParseOperation (const string& s) {
  stringstream ss (s);
  string op, x;
  ss >> op >> x;
  if (op == "*" && x == "old")  Operation = Square;
  else if (op == "*") {
    Operation = Mult;
    Operand = stoi (x);
  }
  else if (op == "+") {
    Operation = Plus;
    Operand = stoi (x);
  }
  else {
    cerr << "Error: unknown operation: " << s << endl;
  }
}

void Monkey::ParseTest (const string& s) {
  stringstream ss (s);
  string op, by;
  int number;
  ss >> op >> by >> number;
  if (op == "divisible" && by == "by") {
    TestOperator = Divisible;
    TestOperand = number;
  }
  else {
    cerr << "Error: unknown test operation: " << s << endl;
  }
}

void Monkey::Print () {
  cout << "Items { ";
  for (Item item : Items)  cout << item.str() << " ";
  cout << "}, " << MonkeyOperatorName (Operation) << " " << Operand << ", ";
  cout << MonkeyTestName (TestOperator) << " " << TestOperand << " -> ";
  cout << DestTrue << " / " << DestFalse;
  cout << endl;
}

/// \brief Inspect and throw the next items
/// \param monkeys        Other monkeys who can receive the items thrown
/// \param divideworries  Divide worry levels by 3 after the item's inspection
/// \return  The next item was successfully  thrown
//
bool Monkey::ThrowNextItem (vector<Monkey>& monkeys, bool divideworries, int loglevel) {
  if (Items.size() < 1)  return false;   // no more items
  // Get item
  Item item = Items.front ();
  // Inspect the item:  Apply start operation
  unsigned long int w = ApplyOperation (item.WorryLevel, Operation, Operand);
  // Finished inspection:  Divide worry level
  if (divideworries)  w /= 3;
  // To keep the worry levels small:
  if (WorryModulo > 1) {
    // All tests are of the form "divisible by m".
    // If a number x is divisible by n if and only if its modulo is also
    // divisible by n.
    // If n is the product (or better: the least common multiple) of all
    // individual "divisible by n" tests, then noone's tests will be disturbed.
    // Beware: This will only work WITHOUT the division by 3 above,
    //   so setting WorryModulo to something other than 1 is only sensible
    //   with divideworries = false
    if (TestOperator == Divisible) {
      w %= WorryModulo;
    }
    else {
      cerr << "Internal error: Not a \"divisible by\" test, "
        << "expect incorrect results because of number overflows" << endl;
    }
  }
  // Decide to which monkey to throw this item
  int destmonkey = ApplyTest (w, TestOperator, TestOperand) ? DestTrue : DestFalse;
  if (destmonkey < 0 || destmonkey >= (int)monkeys.size()) {
    cerr << "Error: Cannot throw to unknown monkey " << destmonkey << endl;
    return false;
  }
  // Modify item worry level and throw it
  if (loglevel >= 3)  cout << "- Throw " << item.WorryLevel << " as " << w
    << " to " << destmonkey << endl;
  item.WorryLevel = w;
  monkeys[destmonkey].Items.push_back (item);
  Items.pop_front ();
  InspectionCount++;
  return true;
}

/// \brief Throw all items in possession
/// \param monkeys        Other monkeys who can receive the items thrown
/// \param divideworries  Divide worry levels by 3 after the item's inspection
/// \return  Number of thrown items
//
int Monkey::ThrowItems (vector<Monkey>& monkeys, bool divideworries, int loglevel) {
  int n = 0;
  while (!this->Items.empty()) {
    if (ThrowNextItem (monkeys, divideworries, loglevel))  n++;
  }
  return n;
}


/// \brief Comparison function to find which monkey is more active
///   (has thrown more items/done more item inspections)
//
bool MonkeyMoreActive (const Monkey& a, const Monkey& b) {
  return a.InspectionCount > b.InspectionCount;
}


const vector<string> ExampleMonkeys = {
  "Monkey 0:",
  "  Starting items: 79, 98",
  "  Operation: new = old * 19",
  "  Test: divisible by 23",
  "    If true: throw to monkey 2",
  "    If false: throw to monkey 3",
  "",
  "Monkey 1:",
  "  Starting items: 54, 65, 75, 74",
  "  Operation: new = old + 6",
  "  Test: divisible by 19",
  "    If true: throw to monkey 2",
  "    If false: throw to monkey 0",
  "",
  "Monkey 2:",
  "  Starting items: 79, 60, 97",
  "  Operation: new = old * old",
  "  Test: divisible by 13",
  "    If true: throw to monkey 1",
  "    If false: throw to monkey 3",
  "",
  "Monkey 3:",
  "  Starting items: 74",
  "  Operation: new = old + 3",
  "  Test: divisible by 17",
  "    If true: throw to monkey 0",
  "    If false: throw to monkey 1"
};


/// \brief Read monkey descriptions from input lines
//
vector<Monkey> ParseMonkeys (size_t& currline, const vector<string>& lines) {
  list<Monkey> ms;
  size_t n = currline;
  while (n < lines.size()) {
    // Read a header line
    if (lines[n].substr (0, 7) != "Monkey ") {
      cerr << "Warning: Not a monkey on line " << n << ": "
        << lines[n] << endl;
      n++;
      continue;
    }
    // Read the monkey characterstics, starting on the next line
    n++;
    Monkey m;
    n = m.Parse (n, lines);
    ms.push_back (m);
    // Expect one empty line but accept any number
    while (n < lines.size() && lines[n].empty())  n++;
  }
  // Convert to a vector and return
  vector<Monkey> mv (ms.size());
  transform (ms.cbegin(), ms.cend(), mv.begin(),
    [] (const Monkey& m) { return m; });
  currline = n;
  return mv;
}

/// \brief Output list of items each monkey holds
//
void PrintMonkeyItems (vector<Monkey> monkeys) {
  for (size_t i = 0; i < monkeys.size(); i++) {
    cout << "Monkey " << i << ": ";
    for (Item item : monkeys[i].Items) {
      cout << item.str() << " ";
    }
    cout << "  (Inspections: " << monkeys[i].InspectionCount << ")" << endl;
  }
}

/// \brief Play one round: every monkey throws all their items
//
void PlayOneRound (vector<Monkey>& monkeys, bool divideworries, int loglevel) {
  for (size_t i = 0; i < monkeys.size(); i++) {
    if (loglevel >= 3)  cout << "Monkey " << i << "'s turn" << endl;
    monkeys[i].ThrowItems (monkeys, divideworries, loglevel);
  }
}


/// \brief Play a number of rounds
//
void PlayManyRounds (vector<Monkey>& monkeys, int rounds = 1,
    bool divideworries = true, int loglevel = 1) {
  for (int r = 1; r <= rounds; r++) {
    PlayOneRound (monkeys, divideworries, loglevel);
    if (loglevel >= 2) {
      cout << "After round " << r << ":" << endl;
      PrintMonkeyItems (monkeys);
    }
  }
}

/// \brief Calculate level of monkey business from two most active ones
//
unsigned long int GetMonkeyBusinessLevel (const vector<Monkey> monkeys) {
  list<Monkey> ml (monkeys.size());
  transform (monkeys.cbegin(), monkeys.cend(), ml.begin(),
    [] (const Monkey& m) { return m; });
  ml.sort (MonkeyMoreActive);
  list<Monkey>::const_iterator it = ml.cbegin();
  int inspec1 = (it++)->InspectionCount;
  int inspec2 = it->InspectionCount;
  return (unsigned long int)inspec1 * (unsigned long int)inspec2;
}

/// \brief Play according to monkey descriptions from input lines
/// \return The "monkey business level"
//
unsigned long int PlayInput (const vector<string> lines, int rounds,
    bool divideworries, bool storemodulo, int loglevel = 1) {
  size_t nextline = 0;
  vector<Monkey> monkeys = ParseMonkeys (nextline, lines);
  cout << "Used lines: " << nextline << ", monkeys found: "
    << monkeys.size() << endl;
  // Output monkeys at the beginning
  if (loglevel >= 1) {
    for (size_t i = 0; i < monkeys.size(); i++) {
      cout << "Monkey " << i << ": ";
      monkeys[i].Print ();
    }
  }
  // Store reduced worry levels by dividing them in every round?
  // Multiply all the divisors from the "divisible by" tests
  // (least common multiple would be better but in this puzzle there
  // are no common factors anyway)
  if (storemodulo) {
    // Calculate product of all divisors
    int prod = accumulate (monkeys.cbegin(), monkeys.cend(), 1,
      [] (int p, const Monkey& m) { return p * m.TestOperand; });
    if (loglevel >= 1)  cout << "Worry level modulo: " << prod << endl;
    // And set every monkey to use it
    for (size_t i = 0; i < monkeys.size(); i++) {
      monkeys[i].WorryModulo = prod;
    }
  }
  // Play rounds
  PlayManyRounds (monkeys, rounds, divideworries, loglevel);
  if (loglevel >= 1) {
    cout << "Item possession after " << rounds << " rounds:" << endl;
    PrintMonkeyItems (monkeys);
  }
  return GetMonkeyBusinessLevel (monkeys);
}


int main () {
  cout << "--- Example: 20 rounds with dividing worry levels ---" << endl;
  unsigned long int mbl = PlayInput (ExampleMonkeys, 20, true, false, 1);
  cout << "* Monkey business level: " << mbl << " *" << endl;
  cout << endl;

  cout << "--- Example: 10000 rounds without dividing worry levels ---" << endl;
  mbl = PlayInput (ExampleMonkeys, 10000, false, true, 1);
  cout << "* Monkey business level: " << mbl << " *" << endl;
  cout << endl;

  cout << "--- Puzzle 1: Monkey business level after 20 rounds, divided worries ---" <<endl;
  vector<string> InputMonkeys =
    ReadLinesVector ("11-monkey-in-the-middle-input.txt");
  mbl = PlayInput (InputMonkeys, 20, true, false, 1);
  cout << "*** Monkey business level: " << mbl << " ***" << endl;
  cout << endl;

  cout << "--- Puzzle 2: Monkey business level after 10000 rounds, undivided worries ---" <<endl;
  mbl = PlayInput (InputMonkeys, 10000, false, true, 1);
  cout << "*** Monkey business level: " << mbl << " ***" << endl;
}

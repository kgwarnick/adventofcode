// Advent of Code 2022 Day 5: Supply Stacks
// https://adventofcode.com/2022/day/5

#include <iostream>
#include <list>
#include <string>
#include <sstream>
#include <vector>
#include <algorithm>

#include "fileread.hpp"

using namespace std;


/// \brief Instruction describing how many crates to move from which stack to which other stack
//
struct MoveInstruction {
  int count;
  int from;
  int to;
};


/// \brief Parse stack arrangement from lines
// list front = stack bottom
//
vector<list<char>> parsestacks (int& currline, const vector<string>& lines) {
  int numstacks = 9;
  vector<list<char>> stacks (numstacks);
  // Read lines containing crates
  while (currline < lines.size() && lines[currline].find ('[') != string::npos) {
    // cout << "Vector size: " << stacks.size() << endl;
    const string& s = lines[currline];
    size_t p = 0;
    if ((s.size() + 3) / 4 > numstacks) {
      cout << "- Need resize from " << numstacks
           << " to " << ((s.size() + 3) / 4) << endl;
      numstacks = (s.size() + 3) / 4;
      stacks.resize (numstacks);
    }
    for (int p = 0; p < s.length(); p += 4) {
      if (p / 4 >= numstacks) cout << "  - need resize" << endl;
      if (s[p] == '[' && s[p+2] == ']') {
        // Put the crate at the bottom of stack n = p / 4
        stacks[p / 4].push_front (s[p+1]);
        // cout << "  - container at pos " << p << " / stack " << p/4
        //   << " with content " << s[p+2] << endl;
      }
      else {
        // cout << "  - Skipping stack " << p/4 << endl;
      }
    }
    currline++;
  }
  // Read the stack number line
  // FIXME Check that the line describes the number of stacks actually found
  cout << "Stack numbers: " << lines[currline] << endl;
  currline++;
  // Return the vector of stacks
  return stacks;
}


/// \brief Output stacks, one stack per line
//
void PrintStacks (vector<list<char> > stacks) {
  for (int n = 0; n < stacks.size(); n++) {
    cout << " " << (n + 1) << "  ";
    for (char c : stacks[n])  cout << " [" << c << "]";
    cout << endl;
  }
}


/// \brief Parse instructions from lines
//
list<MoveInstruction> parseinstructions (int& currline, const vector<string>& lines) {
  list<MoveInstruction> instrlist;
  while (currline < lines.size() && lines[currline].substr (0, 5) == "move ") {
    stringstream ss (lines[currline]);
    MoveInstruction mi;
    string move, from, to;
    ss >> move >> mi.count >> from >> mi.from >> to >> mi.to;
    // cout << "  - Reassembled instruction: " << move << ":" << mi.count
    //   << ":" << from << ":" << mi.from << ":" << to << ":" << mi.to << endl;
    instrlist.push_back (mi);
    currline++;
  }
  return instrlist;
}


/// \brief Carry out a move instruction on the stack vector
/// \return true on success, false on failure (such as a failed move because of an empty stack)
///
/// Beware: Stack numbers are 1-based, vector indices are 0-based!
bool CarryOutMove (const MoveInstruction& moveinstr, vector<list<char>>& stacks) {
  // FIXME Could also check for invalid stack numbers <= 0
  // Check for invalid "from" stack
  if (moveinstr.from > stacks.size()) {
    cerr << "Error: Source stack out of range: "
      << moveinstr.from << " > " << stacks.size() << endl;
    return false;
  }
  // Check for invalid "to" stack
  if (moveinstr.to > stacks.size()) {
    cerr << "Error: Destination stack out of range: "
      << moveinstr.to << " > " << stacks.size() << endl;
    return false;
  }
  // Check for enough "from" crates
  if (moveinstr.count > stacks[moveinstr.from-1].size()) {
    cerr << "Error: Not enough crates remaining in stack " << moveinstr.from
      << ", need " << moveinstr.count << ", have "
      << stacks[moveinstr.from-1].size() << endl;
    return false;
  }
  // Relocate crates one after the other
  for (int i = 0; i < moveinstr.count; i++) {
    char c = stacks[moveinstr.from-1].back();
    stacks[moveinstr.to-1].push_back (c);
    stacks[moveinstr.from-1].pop_back ();
  }
  // Success
  return true;
}


/// \brief Run all move instructions on the stack vector
/// \return Number of instructions successfully carried out
//
int CarryOutMoves (const list<MoveInstruction>& instrlist,
    vector<list<char>>& stacks, int loglevel = 0) {
  int numsuccess = 0;
  for (MoveInstruction m : instrlist) {
    bool success = CarryOutMove (m, stacks);
    if (loglevel >= 2)
      cout << (success ? "[ OK ]" : "[FAIL]") << "  Carry out instruction:  "
        << "move " << m.count << " from " << m.from << " to " << m.to << endl;
    if (success) numsuccess++;
  }
  return numsuccess;
}


/// \brief Find the top-most crate in each stack
/// \return A string of the top-most crates, empty stacks are represented by a space character
//
string GetTopMostCrates (vector<list<char>> stacks) {
  string s;
  for (list<char> stack : stacks)  s += stack.empty() ? ' ' : stack.back ();
  return s;
}


int main () {
  vector<list<char>> examplestacks = {
    { 'C', 'N', 'D' }, { 'M', 'C' }, { 'P' }
  };
  vector<string> examplelines = {
    "    [D]    ",
    "[N] [C]    ",
    "[Z] [M] [P]",
    " 1   2   3 ",
    "",
    "move 1 from 2 to 1",
    "move 3 from 1 to 3",
    "move 2 from 2 to 1",
    "move 1 from 1 to 2"
  };
  int l = 0;
  vector<list<char>> exampleparsed = parsestacks (l, examplelines);
  // Verify an empty line between stacks and instructions
  if (examplelines[l].empty())  l++;
  else  cout << "Warning: Line " << l + 1 << " not empty, trying to parse it" << endl;
  // Parse instructions
  list<MoveInstruction> instructions = parseinstructions (l, examplelines);
  cout << "Found " << instructions.size() << " move instructions" << endl;
  cout << "Total lines parsed: " << l << endl;
  PrintStacks (exampleparsed);
  int numrun = CarryOutMoves (instructions, exampleparsed, 2);
  cout << "Instructions run successfully: " << numrun << endl;
  PrintStacks (exampleparsed);
  cout << "* Top-most crates:  " << GetTopMostCrates (exampleparsed) << " *" << endl;
  cout << endl;

  cout << "--- Puzzle 1: Topmost crates ---" << endl;
  list<string> inputlines = ReadLines ("05-supply-stacks-input.txt");
  vector<string> inputlinevec (inputlines.size());
  transform (inputlines.cbegin(), inputlines.cend(), inputlinevec.begin(), [] (const string& s) { return s; });
  cout << "Read " << inputlines.size() << " lines, vector size: " << inputlinevec.size() << endl;
  l = 0;
  vector<list<char>> inputstacks = parsestacks (l, inputlinevec);
  // Verify an empty line between stacks and instructions
  if (inputlinevec[l].empty())  l++;
  else  cout << "Warning: Line " << l + 1 << " not empty, trying to parse it" << endl;
  // Parse instructions
  instructions = parseinstructions (l, inputlinevec);
  cout << "Found " << instructions.size() << " move instructions" << endl;
  cout << "Total lines parsed: " << l << endl;
  PrintStacks (inputstacks);
  numrun = CarryOutMoves (instructions, inputstacks);
  cout << "Instructions run successfully: " << numrun << endl;
  PrintStacks (inputstacks);
  cout << "*** Top-most crates:  " << GetTopMostCrates (inputstacks)
       << " ***" << endl;
  cout << endl;

  return 0;
}

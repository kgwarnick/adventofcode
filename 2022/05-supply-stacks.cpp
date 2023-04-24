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
vector<list<char>> parsestacks (size_t& currline, const vector<string>& lines) {
  size_t numstacks = 9;
  vector<list<char>> stacks (numstacks);
  // Read lines containing crates
  while (currline < lines.size() && lines[currline].find ('[') != string::npos) {
    // cout << "Vector size: " << stacks.size() << endl;
    const string& s = lines[currline];
    if ((s.size() + 3) / 4 > numstacks) {
      cout << "- Need resize from " << numstacks
           << " to " << ((s.size() + 3) / 4) << endl;
      numstacks = (s.size() + 3) / 4;
      stacks.resize (numstacks);
    }
    for (size_t p = 0; p < s.length(); p += 4) {
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
  for (size_t n = 0; n < stacks.size(); n++) {
    cout << " " << (n + 1) << "  ";
    for (char c : stacks[n])  cout << " [" << c << "]";
    cout << endl;
  }
}


/// \brief Parse instructions from lines
//
list<MoveInstruction> parseinstructions (size_t& currline, const vector<string>& lines) {
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
/// \param moveinstr  The move instruction to run
/// \param stacks     The vector of stacks to operate on
/// \param allinone   Move multiple crates as on group or one after the other
/// \return true on success, false on failure (such as a failed move because of an empty stack)
///
/// Beware: Stack numbers are 1-based, vector indices are 0-based!
bool CarryOutMove (const MoveInstruction& moveinstr,
    vector<list<char>>& stacks, bool allinone = false) {
  // FIXME Could also check for invalid stack numbers <= 0
  // Check for invalid "from" stack
  if (moveinstr.from > (int)stacks.size()) {
    cerr << "Error: Source stack out of range: "
      << moveinstr.from << " > " << stacks.size() << endl;
    return false;
  }
  // Check for invalid "to" stack
  if (moveinstr.to > (int)stacks.size()) {
    cerr << "Error: Destination stack out of range: "
      << moveinstr.to << " > " << stacks.size() << endl;
    return false;
  }
  // Check for enough "from" crates
  if (moveinstr.count > (int)stacks[moveinstr.from-1].size()) {
    cerr << "Error: Not enough crates remaining in stack " << moveinstr.from
      << ", need " << moveinstr.count << ", have "
      << stacks[moveinstr.from-1].size() << endl;
    return false;
  }
  if (allinone) {
    // Move crates to a temporary stack and then to the destination
    // to reverse the sequence twice
    list<char> temp;
    for (int i = 0; i < moveinstr.count; i++) {
      temp.push_back (stacks[moveinstr.from-1].back());
      stacks[moveinstr.from-1].pop_back ();
    }
    for (int i = 0; i < moveinstr.count; i++) {
      stacks[moveinstr.to-1].push_back (temp.back());
      temp.pop_back ();
    }
  }
  // Relocate crates one after the other
  else for (int i = 0; i < moveinstr.count; i++) {
    char c = stacks[moveinstr.from-1].back();
    stacks[moveinstr.to-1].push_back (c);
    stacks[moveinstr.from-1].pop_back ();
  }
  // Success
  return true;
}


/// \brief Run all move instructions on the stack vector
/// \param instrlist  List of instructions to run
/// \param stacks     Crate stacks to operate on
/// \param allinone   Move multiple crates together or a multiple moves
/// \param loglevel   How much output to produce
/// \return Number of instructions successfully carried out
//
int CarryOutMoves (const list<MoveInstruction>& instrlist,
    vector<list<char>>& stacks, bool allinone, int loglevel = 0) {
  int numsuccess = 0;
  for (MoveInstruction m : instrlist) {
    bool success = CarryOutMove (m, stacks, allinone);
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


/// \brief Parse stack arrangement and move instructions from the input lines
/// \param lines         (Input) Lines to parse
/// \param stacks        (Output) Stacks parsed
/// \param instructions  (Output) Instructions parsed
/// \return The number of lines parsed
//
int ParseStacksAndInstructions (const vector<string> lines,
    vector<list<char> >& stacks, list<MoveInstruction>& instructions) {
  size_t l = 0;   // line counter
  // Parse starting stack arrangement
  stacks = parsestacks (l, lines);
  // Verify there is an empty line between stacks and instructions
  if (lines[l].empty())  l++;
  else  cout << "Warning: Line " << l + 1 << " not empty, trying to parse it" << endl;
  // Parse instructions
  instructions = parseinstructions (l, lines);
  // Total number of lines
  return l;
}


/// \brief Run all the instructions on the stack and return the top-most crates
//
string RunInstructions (list<MoveInstruction> instructions,
    vector<list<char>> stacks, bool allinone, int loglevel = 1) {
  if (loglevel >= 1)  PrintStacks (stacks);
  int numrun = CarryOutMoves (instructions, stacks, allinone, loglevel);
  if (loglevel >= 1)  cout << "Instructions run successfully: " << numrun << endl;
  if (loglevel >= 1)  PrintStacks (stacks);
  return GetTopMostCrates (stacks);
}


int main () {
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
  vector<list<char>> examplestacks;
  list<MoveInstruction> exampleinstructions;
  int linesparsed = ParseStacksAndInstructions (
    examplelines, examplestacks, exampleinstructions);
  cout << "Total lines parsed: " << linesparsed << ", found "
    << examplestacks.size() << " crate stacks and "
    << exampleinstructions.size() << " move instructions" << endl;

  string topmost = RunInstructions (exampleinstructions, examplestacks, false, 2);
  cout << "* Top-most crates:  " << topmost << " *" << endl;
  cout << endl;

  cout << "Move multiple crates in one move" << endl;
  topmost = RunInstructions (exampleinstructions, examplestacks, true, 2);
  cout << "* Top-most crates:  " << topmost << " *" << endl;
  cout << endl;

  cout << "--- Puzzle 1: Topmost crates, multiple crates are moved "
    << "one after the other ---" << endl;
  // Read input lines and convert the list to a vector
  list<string> inputlines = ReadLines ("05-supply-stacks-input.txt");
  vector<string> inputlinevec (inputlines.size());
  transform (inputlines.cbegin(), inputlines.cend(), inputlinevec.begin(),
    [] (const string& s) { return s; });
  cout << "Read " << inputlines.size() << " lines, vector size: "
    << inputlinevec.size() << endl;
  vector<list<char>> inputstacks;
  list<MoveInstruction> inputinstructions;
  linesparsed = ParseStacksAndInstructions (inputlinevec, inputstacks, inputinstructions);
  cout << "Total lines parsed: " << linesparsed << ", found "
    << inputstacks.size() << " crate stacks and "
    << inputinstructions.size() << " move instructions" << endl;

  topmost = RunInstructions (inputinstructions, inputstacks, false);
  cout << "*** Top-most crates:  " << topmost << " ***" << endl;
  cout << endl;

  cout << "--- Puzzle 2: Topmost crates when mutliple crates are moved as one block ---" << endl;
  topmost = RunInstructions (inputinstructions, inputstacks, true);
  cout << "*** Top-most crates:  " << topmost << " ***" << endl;

  return 0;
}

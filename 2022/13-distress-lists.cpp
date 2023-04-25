// Advent of Code 2022 Day 13: Distress Signal
// https://adventofocode.com/2022/day/13

#include <list>
#include <iostream>
#include <string>

#include "fileread.hpp"

using namespace std;

/// \brief Data type to store one integer or a list of integers or lists
//
struct IntOrList {
  bool IsList;   ///< Tell whether the integer or list value is valid
  int n;   ///< Integer value, valid only if IsList == false
  list<IntOrList> l;   ///< List value, valid only if IsList == true
  string str() const;
  bool TryParse (const string& s, size_t& cursor);
  IntOrList() : IsList(false), n(0), l() { }
  IntOrList (int i) : IsList(false), n(i), l() { }
  IntOrList (list<IntOrList> iorll) : IsList(true), n(0), l(iorll) { }
};

/// \brief Generate a string representation of the IntOrList
//
string IntOrList::str() const {
  if (!IsList)  return to_string (n);
  // The value is a list, construct its representation
  string temp = "[";
  // TODO comma-separate elements
  for (IntOrList iorl : l) {
    temp += " " + iorl.str();
  }
  temp += " ]";
  return temp;
}

/// \brief Parse an IntOrList type from the string representation
//
bool IntOrList::TryParse (const string& s, size_t& cursor) {
  while (cursor < s.size() && s[cursor] == ' ')  cursor++;   // Skip leading spaces
  if (cursor >= s.size())  return false;   // Unexpected end of string
  if (s[cursor] == '[') {
    IsList = true;
    l.clear();
    cursor++;
    while (cursor < s.size() && s[cursor] == ' ')  cursor++;   // Skip spaces
    // Parse list elements
    while (s[cursor] != ']') {
      IntOrList element;
      if (!element.TryParse (s, cursor))  return false;
      l.push_back (element);
      // Skip spaces and comma
      while (cursor < s.size() && s[cursor] == ' ')  cursor++;
      if (cursor < s.size() && s[cursor] == ',')  cursor++;
      while (cursor < s.size() && s[cursor] == ' ')  cursor++;
    }
    // Move cursor beyond list end
    if (s[cursor] == ']')  cursor++;
  } else {
    IsList = false;
    // Parse as integer
    size_t nextpos;
    n = stoi (s.substr(cursor), &nextpos, 10);
    cursor += nextpos;
    // cout << "Parsed int " << n << ", cursor = " << cursor << endl;
  }
  return true;
}


/// \brief Determine the ordering of a and b
/// \return  -1 if a < b, 0 if a == b, 1 if a > b
//
int IntOrListCompare (const IntOrList& a, const IntOrList& b) {
  // Compare integer to integer
  if (!a.IsList && !b.IsList) {
    // cout << "- Comparing int to int: " << a.n << " <=> " << b.n << endl;
    if (a.n < b.n)  return -1;
    if (a.n > b.n)  return 1;
    return 0;
  }
  // Compare list to list
  if (a.IsList && b.IsList) {
    // cout << "- Comparing list to list: " << a.str() << " <=> " << b.str() << endl;
    // - Compare elements as long as both lists have elements
    list<IntOrList>::const_iterator ait = a.l.cbegin();
    list<IntOrList>::const_iterator bit = b.l.cbegin();
    for (size_t i = 0; i < min (a.l.size(), b.l.size()); i++) {
      int comp = IntOrListCompare (*ait, *bit);
      if (comp < 0) return -1;
      if (comp > 0) return 1;
      ait++;  bit++;
    }
    // - At least one list is out of elements
    if (a.l.size() < b.l.size())  return -1;
    if (a.l.size() > b.l.size())  return 1;
    return 0;
  }
  // a is an integer, wrap it in a list and compare the lists
  if (!a.IsList && b.IsList) {
    list<IntOrList> al;  al.push_back (a);
    IntOrList alist (al);
    // cout << "- Comparing int <=> list: " << alist.str() << " <=> " << b.str() << endl;
    return IntOrListCompare (alist, b.l);
  }
  // b is an integer, wrap it in a list and compare the lists
  if (a.IsList && !b.IsList) {
    list<IntOrList> bl;  bl.push_back (b);
    IntOrList blist (bl);
    // cout << "- Comparing list <=> int: " << a.str() << " <=> " << blist.str() << endl;
    return IntOrListCompare (a, blist);
  }
  // Error: should not get here
  cerr << "Internal error comparing " << a.str() << " <=> " << b.str() << endl;
  return 0;
}

/// \brief Determine whether a is equal to b
//
bool operator== (const IntOrList& a, const IntOrList& b) {
  return IntOrListCompare (a, b) == 0;
}

/// \brief Determine whether a is less than b
//
bool operator< (const IntOrList& a, const IntOrList& b) {
  return IntOrListCompare (a, b) < 0;
}

/// \brief Determine whether a is less than b (function)
//
bool IntOrListLessThan (const IntOrList& a, const IntOrList& b) {
  return IntOrListCompare (a, b) < 0;
}

/// \brief Determine whether a is greater than b
//
bool operator> (const IntOrList& a, const IntOrList& b) {
  return IntOrListCompare (a, b) > 0;
}


/// \brief Calculate sum of indices of correctly ordered pairs
//
int SumIndicesCorrectlyOrderedPairs (const vector<string>& lines) {
  if (lines.size() < 2)  return 0;   // No pair available
  int numpairs = 0;   // count parsed pairs
  int sumcorrect = 0;   // sum of indices of correctly ordered pairs
  size_t i = 0;   // line counter
  while (i < lines.size() - 1) {
    // cout << "Reading line " << i << endl;
    IntOrList a;
    size_t nextpos = 0;
    if (!a.TryParse (lines[i], nextpos)) {
      cerr << "Failed to parse line " << i << ": " << lines[i] << endl;
      return sumcorrect;
    }
    // cout << "- Parsed " << nextpos << " characters to " << a.str() << endl;
    IntOrList b;
    nextpos = 0;
    if (!b.TryParse (lines[i+1], nextpos)) {
      cerr << "Failed to parse line " << i << ": " << lines[i+1] << endl;
      return sumcorrect;
    }
    // cout << "- Parsed " << nextpos << " characters to " << b.str() << endl;
    numpairs++;
    // Parsed successfully, now compare them
    int comp = IntOrListCompare (a, b);
    if (comp < 0)  sumcorrect += numpairs;
    i += 2;
    // Skip an empty line
    if (i < lines.size() && lines[i].empty())  i++;
  }
  cout << "Compared " << numpairs << " pairs" << endl;
  return sumcorrect;
}


/// \brief Build a packet list including divider packets and sort it
//
list<IntOrList> SortedPacketList (const vector<string> lines) {
  list<IntOrList> packetlist;
  size_t numlines = 0;
  // Parse all input packets
  for (string line : lines) {
    numlines++;
    if (line.empty())  continue;
    IntOrList iorl;  size_t cursor = 0;
    if (iorl.TryParse (line, cursor))
      packetlist.push_back (iorl);
    else
      cerr << "Error parsing line " << numlines << ": " << line << endl;
  }
  // Add divider packets
  list<IntOrList> l2;  l2.push_back (IntOrList (2));
  list<IntOrList> ll2;  ll2.push_back (l2);
  IntOrList div2 (ll2);
  list<IntOrList> l6;  l6.push_back (IntOrList (6));
  list<IntOrList> ll6;  ll6.push_back (l6);
  IntOrList div6 (ll6);
  packetlist.push_back (div2);  packetlist.push_back (div6);
  // Sort
  packetlist.sort (IntOrListLessThan);
  // - or? :   packetlist.sort (::operator<);
  // Return packet list
  return packetlist;
}


/// \brief Find indices of divider packets and multiply them to get the "decoder key"
//
unsigned int GetDecoderKey (const list<IntOrList> packetlist) {
  unsigned int div2index = 0, div6index = 0, index = 0;
  // Construct divider packets to compare against
  list<IntOrList> l2;  l2.push_back (IntOrList (2));
  list<IntOrList> ll2;  ll2.push_back (l2);
  IntOrList div2 (ll2);
  list<IntOrList> l6;  l6.push_back (IntOrList (6));
  list<IntOrList> ll6;  ll6.push_back (l6);
  IntOrList div6 (ll6);
  // Search list for divider packets
  for (IntOrList packet : packetlist) {
    index++;   // increment now because 1-based indices are needed
    if (IntOrListCompare (packet, div2) == 0)  div2index = index;
    if (IntOrListCompare (packet, div6) == 0) {
      div6index = index;
      break;   // div6 has to come after div6, so stop searching now
    }
  }
  cout << "Divider packets at indices " << div2index << ", " << div6index << endl;
  // Return the product -- if one or both dividers have not been found, returns 0
  return div2index * div6index;
}


int main () {
  cout << "--- Example ---" << endl;
  vector<string> inputlines = ReadLinesVector ("13-distress-lists-example.txt");
  cout << "Read " << inputlines.size() << " lines" << endl;
  int orderedcount = SumIndicesCorrectlyOrderedPairs (inputlines);
  cout << "* Sum of indices of correctly ordered pairs: " << orderedcount
    << " *" << endl;
  cout << endl;

  list<IntOrList> packets = SortedPacketList (inputlines);
  for (IntOrList packet : packets) {
    cout << packet.str() << endl;
  }
  unsigned int decoderkey = GetDecoderKey (packets);
  cout << "* Decoder key: " << decoderkey << " *" << endl;
  cout << endl;

  cout << "--- Puzzle 1: Sum of indices of correctly ordered pairs ---" << endl;
  inputlines = ReadLinesVector ("13-distress-lists-input.txt");
  cout << "Read " << inputlines.size() << " lines" << endl;
  orderedcount = SumIndicesCorrectlyOrderedPairs (inputlines);
  cout << "*** Sum of indices of correctly ordered pairs: " << orderedcount
    << " ***" << endl;
  cout << endl;

  cout << "--- Puzzle 2: Decoder key ---" << endl;
  packets = SortedPacketList (inputlines);
  cout << "Packets: " << packets.size() << endl;
  decoderkey = GetDecoderKey (packets);
  cout << "*** Decoder key: " << decoderkey << " ***" << endl;
  return 0;
}

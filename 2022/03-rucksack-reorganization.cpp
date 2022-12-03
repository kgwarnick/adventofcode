// Advent of Code 2022 Day 3 Rucksack Reorganization
// https://adventofcode.com/2022/day/3

#include "fileread.hpp"
#include <iostream>
#include <list>
#include <string>


using namespace std;


/// \brief Find the first character that is present in both halfs of the given string
/// \return The character that is found in both halfs of the string,
///   if no character appears in both halfs of the string
char FindItemPresentInBothHalfs (const string& itemstring) {
  for (char c : itemstring.substr (0, itemstring.size() / 2))
    if (itemstring.substr (itemstring.size() / 2) .find (c) != string::npos)
      return c;
  return ' ';
}


/// \brief Calculate item priority value
int GetItemPriority (const char item) {
  const string priorities = " abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  int n = priorities.find (item);
  return n != string::npos ? n : 0;
}


/// \brief Find the character present in both halfs of each line
//
string FindMisplacedItemInEachRucksack (list<string> rucksacklines) {
  int n = 0;
  string items = "";
  for (string s : rucksacklines) {
    n++;
    items += FindItemPresentInBothHalfs (s);
    // cout << n << "  " << s << "  " << FindItemPresentInBothHalfs (s) << endl;
  }
  return items;
}


/// \brief Calculate the sum of the priority value of all items in the string
//
int GetPrioritySum (const string& items) {
  int sum = 0;
  for (char c : items)  sum += GetItemPriority (c);
  return sum;
}


/// \brief Find the common character in the three lines
//
char GetCommonCharacter (const string& s1, const string& s2, const string& s3) {
  string commonchars;
  for (char c : s1) {
    // if this character is also present in the other two strings
    // and it is not the same as a character already found, then add it
    if ((s2.find (c) != string::npos) && (s3.find (c) != string::npos) &&
      (commonchars.find (c) == string::npos))
      commonchars += c;
  }
  if (commonchars.size() != 1)
    cerr << "Found unexpected number of common characters: expected 1, found "
         << commonchars.size() << ", " << commonchars << endl;
  return commonchars[0];
}


/// \brief Find the common character in every group of three lines
//
string FindCommonCharacterInGroupsOfThree (list<string> rucksacklines) {
  int n = 0;
  string items = "";
  list<string>::const_iterator siter = rucksacklines.cbegin ();
  while (siter != rucksacklines.cend ()) {
    n++;
    char badge = GetCommonCharacter (*siter++, *siter++, *siter++);
    items += badge;
    // cout << "Group " << n << " badge: " << badge << endl;
  }
  return items;
}



int main () {

  cout << "--- Examples ---" << endl;
  list<string> examples;
  examples.push_back ("vJrwpWtwJgWrhcsFMMfFFhFp");
  examples.push_back ("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL");
  examples.push_back ("PmmdzqPrVvPwwTWBwg");
  examples.push_back ("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn");
  examples.push_back ("ttgJtRGJQctTZtZT");
  examples.push_back ("CrZsJsPPZsGzwwsLwLmpwMDw");
  string misplaced = FindMisplacedItemInEachRucksack (examples);
  cout << "Misplaced items: " << misplaced << endl;
  cout << "Sum of priority value of misplaced items: " << GetPrioritySum (misplaced) << endl;
  string badgelist = FindCommonCharacterInGroupsOfThree (examples);
  cout << "Badges: " << badgelist << endl;
  cout << "Sum of priority value of badges: " << GetPrioritySum (badgelist) << endl;
  cout << endl;

  cout << "--- Puzzle 1: Find all misplaced items ---" << endl;
  list<string> lines = ReadLines ("03-rucksack-reorganization-input.txt");
  cout << "Read " << lines.size() << " lines" << endl;
  misplaced = FindMisplacedItemInEachRucksack (lines);
  cout << "Misplaced items: " << misplaced << endl;
  cout << "*** Sum of priority value of misplaced items: "
       << GetPrioritySum (misplaced) << endl;
  cout << endl;

  cout << "--- Puzzle 2: Find all badges for groups of three ---" << endl;
  badgelist = FindCommonCharacterInGroupsOfThree (lines);
  cout << "Badges: " << badgelist << endl;
  cout << "*** Sum of priority value of badges: "
       << GetPrioritySum (badgelist) << endl;
  return 0;
}

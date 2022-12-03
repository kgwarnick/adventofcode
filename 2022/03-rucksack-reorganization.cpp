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
  cout << "Sum: " << GetPrioritySum (misplaced) << endl << endl;

  cout << "--- Puzzle 1: Find all misplaced items ---" << endl;
  list<string> lines = ReadLines ("03-rucksack-reorganization-input.txt");
  cout << "Read " << lines.size() << " lines" << endl;
  misplaced = FindMisplacedItemInEachRucksack (lines);
  cout << "*** Sum of priority value of misplaced items: "
       << GetPrioritySum (misplaced) << endl;
  return 0;
}

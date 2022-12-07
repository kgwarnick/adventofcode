// Advent of Code 2022 Day 7: No Space Left On Device
// https://adventofcode.com/day/7

#include <iostream>
#include <list>
#include <map>
#include <string>
#include <sstream>
#include <utility>

#include "fileread.hpp"

using namespace std;

const list<string> ExampleLines = {
  "$ cd /",
  "$ ls",
  "dir a",
  "14848514 b.txt",
  "8504156 c.dat",
  "dir d",
  "$ cd a",
  "$ ls",
  "dir e",
  "29116 f",
  "2557 g",
  "62596 h.lst",
  "$ cd e",
  "$ ls",
  "584 i",
  "$ cd ..",
  "$ cd ..",
  "$ cd d",
  "$ ls",
  "4060174 j",
  "8033020 d.log",
  "5626152 d.ext",
  "7214296 k"
};


/// \brief Type for a directory entry, can be a regular file or a directory
//
class DirEntry {
 public:
  string name;
  unsigned long int size;
  map<string,DirEntry*> children;
  DirEntry* parent;
  bool IsDirectory;

  DirEntry (const string& entryname = "") : name (entryname), parent(0) { }

  /// \brief Calculate the size of this directory with all its children
  //
  unsigned long int CalcSize () {
    // If it is a regular entry return its size
    if (!IsDirectory)  return size;
    // If it is a directory return the sum of the childrens' sizes
    unsigned long int sumsize = 0;
    for (pair<string,DirEntry*> entry : children) {
      sumsize += entry.second->CalcSize ();
    }
    size = sumsize;   // Store the size
    return sumsize;
  }

  /// \brief Return the full path of this directory
  //
  string GetFullPath () {
    return (parent != 0 ? parent->GetFullPath() +
        (parent->name != "/" ? "/" : "")
      : "") + name;
  }

  /// \brief Print a listing of this directory and ots children
  //
  void PrintTree (bool withdirsize = false) {
    printtree (withdirsize, "", "  ");
  }

  /// \brief Find directories with certain maximum size
  //
  list<DirEntry*> FindDirectoriesMaxSize (unsigned long int maxsize);

 private:
  void printtree (bool withdirsize, const string& currindent, const string& addindent) {
    // print a line with information about this node
    cout << currindent << (IsDirectory ? "DIR  " : "FILE ")
      << (withdirsize ? CalcSize(): 0) << "  " << name
      << "  " << GetFullPath() << endl;
    // followed by the children
    for (pair<string,DirEntry*> child : children) {
      child.second->printtree (withdirsize, currindent + addindent, addindent);
    }
  }
};


class ShellSession {
 public:
  DirEntry* currdir;
  DirEntry* tree;

  ShellSession () : currdir (0), tree (0) { }

  /// \brief Change the current directory
  //
  void ChangeDir (const string& destdir) {
    if (destdir == "..") {
      if (currdir->parent != 0)  currdir = currdir->parent;  else currdir = tree;
      // cout << "- chdir up to " << currdir->name << endl;
    }
    else if (destdir == "/") {
      currdir = tree;
      // cout << "- chdir to root " << currdir->name << endl;
    }
    else {
      // find or create sub-directory
      map<string,DirEntry*>::iterator dirit = currdir->children.find (destdir);
      if (dirit != currdir->children.end()) {
        // cout << "- chdir to existing sub-dir \"" << destdir << "\"" << endl;
        currdir = dirit->second;
      }
      else {
        // cout << "- chdir to newly created sub-dir \"" << destdir << "\"" << endl;
        DirEntry* newsubdir = new DirEntry (destdir);
        newsubdir->IsDirectory = true;
        newsubdir->parent = currdir;
        currdir->children[destdir] = newsubdir;
        currdir = newsubdir;
      }
    }
  }

  /// \brief Run through the log and collect directories and files
  //
  void WalkDirTree (const list<string>& logfile) {
    bool dirlisting = false;   // currently in a directory listing
    if (currdir == 0) {
      currdir = new DirEntry ("/");
      currdir->IsDirectory = true;
      tree = currdir;
    }
    for (string line : logfile) {
      if (line.empty()) {
        cerr << "INFO: Skip empty line" << endl;
        continue;
      }
      else if (line[0] == '$' && line[1] == ' ') {
        stringstream ss (line);
        string prompt, command, arg;
        ss >> prompt >> command >> arg;
        if (command == "cd") {
          dirlisting = false;
          ChangeDir (arg);
        }
        if (command == "ls") {
          // cout << "- ls" << endl;
          dirlisting = true;
        }
      }
      else if (dirlisting && isdigit (line[0])) {
        stringstream ss (line);
        unsigned long int entrysize;
        string entryname;
        ss >> entrysize >> entryname;
        DirEntry* newfile = new DirEntry (entryname);
        newfile->IsDirectory = false;
        newfile->parent = currdir;
        newfile->size = entrysize;
        currdir->children[entryname] = newfile;
        // cout << "File: " << newfile->name << " with size " << newfile->size << endl;
      }
      else if (dirlisting && line.substr (0, 3) == "dir") {
        stringstream ss (line);
        string dirkeyword, entryname;
        ss >> dirkeyword >> entryname;
        DirEntry* newsubdir = new DirEntry (entryname);
        newsubdir->IsDirectory = true;
        newsubdir->parent = currdir;
        currdir->children[entryname] = newsubdir;
        // cout << "Sub-directory: " << entryname << endl;
      }
      else {
        cerr << "Don't know what to do with line: " << line
          << " -- dirlisting? " << (dirlisting ? "yes" : "no") << endl;
      }
    }
  }
};


int main () {
  cout << "--- Example ---" << endl;
  ShellSession sess;
  sess.WalkDirTree (ExampleLines);
  sess.tree->PrintTree (true);
  list<DirEntry*> smalldirs = sess.tree->FindDirectoriesMaxSize (100000);
  unsigned long sumsize = 0;
  for (DirEntry* sd : smalldirs) {
    cout << "Small dir: " << sd->name << ", size: " << sd->size << endl;
    sumsize += sd->size;
  }
  cout << "* Sum of sizes of small directories: " << sumsize << " *" << endl;
  cout << endl;

  cout << "--- Puzzle 1: Directories with sze at most 100000 ---" << endl;
  list<string> InputLines = ReadLines ("07-no-space-left-on-device-input.txt");
  cout << InputLines.size() << " lines read from file" << endl;
  ShellSession inputsess;
  inputsess.WalkDirTree (InputLines);
  cout << "tree root = " << inputsess.tree << endl;
  smalldirs = inputsess.tree->FindDirectoriesMaxSize (100000);
  sumsize = 0;
  for (DirEntry* sd : smalldirs) {
    sumsize += sd->size;
  }
  cout << "* Sum of sizes of small directories: " << sumsize << " *" << endl;
}


list<DirEntry*> DirEntry::FindDirectoriesMaxSize (unsigned long int maxsize) {
  list<DirEntry*> dirlist;   // List of matching results
  for (pair<string,DirEntry*> child : children) {
    // Only select directories
    if (child.second->IsDirectory) {
      // If it is small enough include it in the result list
      if (child.second->CalcSize() <= maxsize)
        dirlist.push_back (child.second);
      // Check sub-directories as well
      list<DirEntry*> matchingchildren =
        child.second->FindDirectoriesMaxSize (maxsize);
      for (DirEntry* childentry : matchingchildren) {
        dirlist.push_back (childentry);
      }
    }
  }
  return dirlist;
}

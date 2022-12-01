/// \file
/// \brief File reading helper functions

#include <fstream>
#include <list>
#include <string>

/// \brief Helper function to parse all lines of a file
/// \return A list of lines without the end-of-line character
std::list<std::string> ReadLines (const std::string& filename){
  std::ifstream ifs (filename);
  std::string s;
  std::list<std::string> sl;
  while (getline (ifs, s)) {
    sl.push_back (s);
  }
  return sl;
}

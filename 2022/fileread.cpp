/// \file
/// \brief File reading helper functions

#include <fstream>
#include <list>
#include <string>
#include <vector>
#include <algorithm>

/// \brief Helper function to parse all lines of a file
/// \return A list of lines without the end-of-line character
std::list<std::string> ReadLines (const std::string& filename) {
  std::ifstream ifs (filename);
  std::string s;
  std::list<std::string> sl;
  while (getline (ifs, s)) {
    sl.push_back (s);
  }
  return sl;
}


/// \brief Helper function to parse all lines of a file
/// \return A vector of lines without the end-of-line character
std::vector<std::string> ReadLinesVector (const std::string& filename) {
  // Read as a list first
  std::list<std::string> linelist = ReadLines (filename);
  // Then convert to a vector
  std::vector<std::string> linevec (linelist.size());
  std::transform (linelist.cbegin(), linelist.cend(), linevec.begin(),
    [] (const std::string& s) { return s; });
  return linevec;
}

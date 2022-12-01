
#include <list>
#include <string>
#include <iostream>

/// \brief Helper function to split a string into a list of string
/// \return A list of lines without the end-of-line character
std::list<std::string> SplitString (const std::string& s,
  const std::string& splitter){
  std::list<std::string> sl;
  std::string::size_type splitpos = 0;
  std::string::size_type nextpos = 0;
  while ((nextpos = s.find_first_of (splitter, splitpos)) != std::string::npos) {
    sl.push_back (s.substr (splitpos, nextpos - splitpos));
    splitpos = nextpos + splitter.size ();
  }
  sl.push_back (s.substr (splitpos));
  return sl;
}

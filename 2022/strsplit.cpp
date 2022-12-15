
#include <list>
#include <string>
#include <iostream>
#include <vector>
#include <algorithm>

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

std::vector<std::string> SplitStringVector (const std::string& s,
  const std::string& splitter) {
  // Read as a list
  std::list<std::string> strlist = SplitString (s, splitter);
  // And convert the list to a vector
  std::vector<std::string> strvec (strlist.size());
  std::transform (strlist.cbegin(), strlist.cend(), strvec.begin(),
    [] (const std::string& s) { return s; });
  return strvec;
}

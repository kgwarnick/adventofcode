#include <list>
#include <string>
#include <vector>

/// \brief Helper function to split a string into a list of strings
/// \param s         The string to split
/// \param splitter  The separator at which to split
/// \return A list of strings
//
std::list<std::string> SplitString (const std::string& s,
  const std::string& splitter);

/// \brief Helper function to split a string into a vector of strings
/// \param s         The string to split
/// \param splitter  The separator at which to split
/// \return A vector of strings
//
std::vector<std::string> SplitStringVector (const std::string& s,
  const std::string& splitter);

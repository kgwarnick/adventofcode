// Advent of Code 2015, Day 9 All in a Single Night
// https://adventofcode.com/2015/day/9

#include <iostream>
#include <iomanip>
#include <fstream>
#include <sstream>
#include <list>
#include <set>
#include <string>
#include <climits>


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


/// \brief Helper function to create a text representation of a list
template<typename T> std::string ListToString (std::list<T> l,
    const std::string& separator = ", ") {
  std::stringstream ss;
  for (typename std::list<T>::iterator it = l.begin(); it != l.end(); it++) {
    if (it != l.begin()) ss << separator;
    ss << *it;
  }
  return ss.str();
}

/// \brief Helper function to create a text representation of a set
template<typename T> std::string SetToString (std::set<T> s,
    const std::string& separator = ", ") {
  std::stringstream ss;
  for (typename std::set<T>::iterator it = s.begin(); it != s.end(); it++) {
    if (it != s.begin()) ss << separator;
    ss << *it;
  }
  return ss.str();
}


typedef struct {
  std::string location1;
  std::string location2;
  int distance;
} DistanceDef;


DistanceDef ParseDistanceDef (std::string distanceline) {
  DistanceDef dd;
  // location 1
  std::string::size_type spacepos = distanceline.find (' ');
  if (spacepos == std::string::npos)  return dd;
  dd.location1 = distanceline.substr (0, spacepos);
  // skip string " to "
  if (distanceline.substr (spacepos, 4) != " to ")  return dd;
  spacepos += 4;
  // location 2
  std::string::size_type spacenext = distanceline.find (' ', spacepos);
  if (spacepos == std::string::npos)  return dd;
  dd.location2 = distanceline.substr (spacepos, spacenext - spacepos);
  // skip string " = "
  if (distanceline.substr (spacenext, 3) != " = ")  return dd;
  spacepos = spacenext + 3;
  // distance
  dd.distance = std::stoi (distanceline.substr (spacepos));
  return dd;
}


std::list<DistanceDef> ParseDistanceList (std::list<std::string> lines) {
  std::list<DistanceDef> dl;
  for (std::string s : lines) {
    DistanceDef dd = ParseDistanceDef (s);
    dl.push_back (dd);
  }
  return dl;
}


int GetDistance (std::string from, std::string to, std::list<DistanceDef> distances) {
  for (DistanceDef dd : distances) {
    if ((dd.location1 == from && dd.location2 == to) ||
        (dd.location1 == to   && dd.location2 == from))  return dd.distance;
  }
  std::cerr << "Distance not found from " << from << " to " << to << std::endl;
  return 0;
}


int GetTotalDistance (std::list<std::string> stops, std::list<DistanceDef> distances) {
  int totdist = 0;
  for (std::list<std::string>::iterator stop = stops.begin ();
       stop != stops.end();  stop++) {
    std::list<std::string>::iterator nextstop = stop;
    nextstop++;
    if (nextstop == stops.end())  continue;   // break, no more hop to be done
    totdist += GetDistance (*stop, *nextstop, distances);
  }
  return totdist;
}


std::set<std::string> CollectLocations (std::list<DistanceDef> distlist) {
  std::set<std::string> locs;
  for (std::list<DistanceDef>::iterator diter = distlist.begin();
       diter != distlist.end();  diter++) {
    if (locs.count (diter->location1) < 1)  locs.insert (diter->location1);
    if (locs.count (diter->location2) < 1)  locs.insert (diter->location2);
  }
  return locs;
}


/// \brief Recursively calculate all permutations of the given set
template<typename T> std::list<std::list<T> > GetAllPermutationsRec (std::set<T> elements) {
  std::list<std::list<T> > permlist;
  if (elements.size() == 1) {
    // only one element, wrap it in a list and another list and return it
    std::list<T> onlyperm;
    onlyperm.push_back (*(elements.begin()));
    permlist.push_back (onlyperm);
    return permlist;
  }
  for (T elem : elements) {
    std::set<T> shorterset = elements;
    typename std::set<T>::iterator currelem = shorterset.find (elem);
    shorterset.erase (currelem);
    std::list<std::list<T> > shorterperms = GetAllPermutationsRec (shorterset);
    for (typename std::list<std::list<T> >::iterator permiter = shorterperms.begin();
         permiter != shorterperms.end();  permiter++) {
      std::list<T> neueperm = *permiter;   // copy shorter permutation
      neueperm.push_front (elem);   // and prepend the new element
      permlist.push_back (neueperm);   // then store this permutation in the list
    }
  }
  return permlist;
}


void CalculateRoutes (std::list<DistanceDef> distances,
    int& shortestDistance, std::list<std::string>& shortestRoute,
    int loglevel = 1) {
  // Print distance table
  if (loglevel >= 1) {
    std::cout << distances.size() << " Distance pairs" << std::endl;
    if (loglevel >= 2) {
      for (DistanceDef d : distances) {
        std::cout << "  " << std::right << std::setw(15) << d.location1 << " -- "
          << std::left << std::setw(15) << d.location2
          << " = " << std::right << std::setw(3) << d.distance << std::endl;
      }
    }
  }
  // Determine all locations from the distance table
  std::set<std::string> locations = CollectLocations (distances);
  if (loglevel >= 1) {
    std::cout << locations.size() << " Locations:  " << SetToString (locations)
      << std::endl;
  }
  // Get all possible travel routes
  std::list<std::list<std::string> > routes = GetAllPermutationsRec (locations);
  std::cout << routes.size() << " Possible travel routes" << std::endl;
  // Calculate travel distance for all routes
  int shortestdistance = INT_MAX;
  std::list<std::string> shortestroute;
  for (std::list<std::list<std::string> >::iterator lliter = routes.begin();
       lliter != routes.end();  lliter++) {
    int traveldist = GetTotalDistance (*lliter, distances);
    if (loglevel >= 3) {
      std::cout << "  " << ListToString (*lliter, " -> ")
                << "  => Total distance: " << traveldist << std::endl;
    }
    // check whether this route is the shorter than all earlier ones
    if (traveldist < shortestdistance) {
      shortestdistance = traveldist;
      shortestroute = *lliter;
    }
  }
  // Set output values
  shortestDistance = shortestdistance;
  shortestRoute = shortestroute;
}


int main () {

  std::cout << "--- Example: Shortest route ---" << std::endl;
  std::list<std::string> ExampleLines;
  ExampleLines.push_back ("London to Dublin = 464");
  ExampleLines.push_back ("London to Belfast = 518");
  ExampleLines.push_back ("Dublin to Belfast = 141");
  std::list<DistanceDef> distances = ParseDistanceList (ExampleLines);
  int shortestdistance;
  std::list<std::string> shortestroute;
  CalculateRoutes (distances, shortestdistance, shortestroute, 3);
  std::cout << "Shortest route:  " << ListToString (shortestroute, " -> ")
    << "  => Total travel distance: " << shortestdistance << std::endl;
  std::cout << std::endl;

  std::cout << "--- Aufgabe 1: Shortest route ---" << std::endl;
  std::string filename = "09-single-night-input.txt";
  std::list<std::string> lines = ReadLines (filename);
  std::cout << lines.size() << " Lines read from file: " << filename << std::endl;
  distances = ParseDistanceList (lines);

  CalculateRoutes (distances, shortestdistance, shortestroute, 2);
  std::cout << "*** Shortest route:  " << ListToString (shortestroute, " -> ")
    << "  => Total travel distance: " << shortestdistance << " ***" << std::endl;
}

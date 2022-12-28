// Advent of Code 2022 Day 25: Full of Hot Air
// https://adventofcode.com/2022/day/25

#include <cstring>
#include <string>
#include <list>
#include <iostream>
#include <iomanip>

using namespace std;

#include "fileread.hpp"


/// \brief Convert a SNAFU number to its long integer value
//
extern "C"
long int SnafuToLong (const char *snafu) {
  long int result = 0;
  for (const char *c = snafu; *c != '\0'; c++) {
    long int digval = *c == '2' ? 2 : (*c == '1' ? 1 :
      (*c == '-' ? -1 : (*c == '=' ? -2 : 0)));
    result = result * 5 + digval;
  }
  return result;
}

/// \brief Convert a long integer to a SNAFU representation
//
extern "C"
char *LongToSnafu (long int value, size_t maxchar, char *buffer) {
  // Construct number in temporary buffer
  const char snafudigits[6] = "=-012";
  char temp[maxchar];
  size_t digits;
  for (digits = 0; digits < maxchar && value != 0; digits++) {
    long digitval = (value + 2) % 5 - 2;
    // cout << "- Digit value: " << digitval << " -> digit \'"
    //   << snafudigits[digitval + 2] <<"\'" << endl;
    temp[maxchar - 1 - digits] = snafudigits[digitval + 2];
    if ((value - digitval) % 5 != 0)
      cerr << "Internal Error: Unexpected number not divisible by 5: "
        << value - digitval << endl;
    value = (value - digitval) / 5;
  }
  // Copy to output buffer
  for (size_t i = 0; i < digits; i++)
    buffer[i] = temp[maxchar - digits + i];
  // Special case for value zero
  if (digits == 0) { buffer[0] = '0'; digits = 1; }
  // Terminating null character only if enough space in buffer
  if (digits < maxchar)  buffer[digits] = '\0';
  // Also return the constructed number
  return buffer;
}


/// \brief Calculate the sum of the SNAFU numbers, as a SNAFU number
//
string SnafuSum (const list<string>& snafus, int loglevel = 0) {
  long sum = 0;
  for (const string& s: snafus) {
    long val = SnafuToLong (s.c_str());
    if (loglevel >= 2)
      cout << setw(24) << s << "  ->  " << setw(20) << val << endl;
    sum += val;
  }
  if (loglevel >= 1)
    cout << "- Sum of SNAFU numbers (decimal): " << sum << endl;
  char t[64];
  LongToSnafu (sum, 64, t);
  return string (t);
}


const long TestInts[] =
  { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 2022, 12345, 314159265 };

const list<string> TestSnafus = {
  "0",
  "1=-0-2", "12111",  "2=0=",   "21",     "2=01",
  "111",    "20012",  "112",    "1=-1=",  "1-12",
  "12",     "1=",     "122"
};


int main () {
  cout << "--- Tests ---" << endl;
  cout << "SNAFU to long" << endl;
  char b[32];
  for (size_t i = 0; i < sizeof (TestInts) / sizeof (TestInts[0]); i++)
    cout << setw(10) << TestInts[i] << "  ->  " << setw(12)
      << LongToSnafu (TestInts[i], 32, b) << endl;
  cout << endl;

  cout << "--- Example ---" << endl;
  string sumsnafu = SnafuSum (TestSnafus, 2);
  cout << "* Sum of SNAFU numbers: " << sumsnafu << " *" << endl;
  cout << endl;

  cout << "--- Puzzle 1 ---" << endl;
  list<string> inputlines = ReadLines ("25-full-of-hot-air-input.txt");
  sumsnafu = SnafuSum (inputlines, 1);
  cout << "*** Sum of SNAFU numbers: " << sumsnafu << " ***" << endl;
}

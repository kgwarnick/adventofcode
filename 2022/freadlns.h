/// \file
/// \brief File reading

#include <stdio.h>

/// \brief Read all lines from the file and store them in the char* array pointed to by lines
///
/// The array can be a pre-allocated array of size *maxlines, and will be reallocated as
/// necessary to contain all lines.  The actual number of lines read is stored in *numlines.
///
/// The array itself can/should be allocated on calling this function,
/// but no lines in the array should be allocated.  getline() will be called to read
/// each line, allocating memory as necessary.
/// After use, all the lines read (indicated by *numlines) as well as the array itself
/// should be deallocated by the caller.
///
/// \param filename  File name to read
/// \param maxlines  Pointer to the array size
/// \param numlines  Pointer to the number of lines read
/// \param lines     Pointer to char** to store the lines read from the file
/// \return The number of characters read or -1 on failure
//
ssize_t readlines (const char *filename, size_t *maxlines, size_t *numlines, char ***lines);

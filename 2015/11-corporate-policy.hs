-- Advent of Code 2015 Day 11 Corporate Policy
-- https://adventofcode.com/2015/day/11

import System.IO

-- Drop trailing spaces and newlines
dropTrailingSpaceAndNewLine s = if (last s /= ' ' && last s /= '\n') then s else init s

-- Does the string start with a straight of three letters
startsWithStraight s = (length s >= 3) && (s !! 1 == succ (s !! 0)) && (s !! 2 == succ (s !! 1))

-- Count how many straights are contained in the string
countStraights s = if length s < 3
  then 0
  else if startsWithStraight s
    then 1 + countStraights (drop 3 s)
    else 0 + countStraights (drop 1 s)

-- Does the string start with a pair of letters?
startsWithPair s = (length s >= 2) && (s !! 0 == s !! 1)

-- Count how many non-overlapping pairs are contained in the string
countNonOverlappingPairs s = if length s < 2
  then 0
  else if startsWithPair s
    then 1 + countNonOverlappingPairs (drop 2 s)
    else 0 + countNonOverlappingPairs (drop 1 s)

-- Find all non-overlapping pairs in the string and return them as a list of strings
getPairs s = if length s >= 2
  then (if startsWithPair s
    then ((take 2 s) : getPairs (drop 2 s))
    else getPairs (drop 1 s))
  else []

-- Find all different pairs in a string and return them as a list of strings
getDifferentPairs s = getUniqueElements (getPairs s)

-- Count how many different pairs are contained in the string
countDifferentPairs s = length (getDifferentPairs s)

-- Count how many different elements are contained in the list
countDifferentElements :: (Eq a) => [a] -> Int
countDifferentElements = length . getUniqueElements

-- Find all unique elements in a list (create a "set") and return them as a list
-- Note:  Use 'nub' from module Data.List instead of this function
getUniqueElements l = if length l > 1
  then if elem (last l) (init l)
    then (getUniqueElements (init l))
    else (getUniqueElements (init l)) ++ [ (last l) ]
  else l

-- Check whether the string only contains allowed characters
containsOnlyValidCharacters s = not (elem 'l' s || elem 'i' s || elem 'o' s)

-- Check whether the string is a valid password
isValidPassword s = countStraights s > 0 && containsOnlyValidCharacters s && countDifferentPairs s >= 2


-- Determine the successor character with wrap around from 'z' to 'a'
succChar c = if (c == 'z') then 'a'
  else if (c == 'Z') then 'A'
    else succ c

-- Determine whether incrementing the character will wrap around
incrCharDoesWrap c = (c == 'z' || c == 'Z')

-- Increment a string, handling wrap-around as necessary
-- * Increment the last character and combine it with the rest of the string
-- * If the increment leads to a wrap-around, combine the wrapped-around
--   character with the incremented string of the other characters
successorString s =
  if length s <= 1
    then [ (succChar (last s)) ]
    else if (incrCharDoesWrap (last s))
      then (successorString (init s)) ++ [ succChar (last s) ]
      else (init s) ++ [ succChar (last s) ]

-- Determine next valid password after a given string
nextValidPassword s =
  if isValidPassword (successorString s) then (successorString s)
  else nextValidPassword (successorString s)


-- Main Program

testcase1 = "hijklmmn"
testcase2 = "abbceffg"
testcase3 = "abbcegjk"
testcase4 = "abcdefgh"
testcase5 = "ghijklmn"

main = do
  putStrLn "--- Examples ---"
  putStrLn (testcase1 ++ " contains a straight?  " ++
    (if countStraights testcase1 > 0 then "Yes" else "No"))
  putStrLn (testcase1 ++ " contains only valid characters?  " ++
    (if containsOnlyValidCharacters testcase1 then "Yes" else "No"))
  putStrLn (testcase2 ++ " contains a straight?  " ++
    (if countStraights testcase2 > 0 then "Yes" else "No"))
  putStrLn (testcase2 ++ " contains at least two different pairs?  " ++
    (if countDifferentPairs testcase2 >= 2 then "Yes" else "No"))
  putStrLn (testcase3 ++ " contains at least two different pairs?  " ++
    (if countDifferentPairs testcase3 >= 2 then "Yes" else "No"))
  putStrLn ("Next valid password after " ++ testcase4 ++ ": " ++ nextValidPassword testcase4)
  putStrLn ("Next valid password after " ++ testcase5 ++ ": " ++ (nextValidPassword testcase5))
  putStrLn ""

  putStrLn "--- Puzzle 1: Santa's next password ---"
  fileHandle <- openFile "11-corporate-policy-input.txt" ReadMode
  inputText <- hGetContents fileHandle
  putStrLn ("input from file: " ++ dropTrailingSpaceAndNewLine inputText)
  hClose fileHandle
  putStrLn ("Next valid password after " ++ (dropTrailingSpaceAndNewLine inputText)
    ++ ": " ++ (nextValidPassword (dropTrailingSpaceAndNewLine inputText)))

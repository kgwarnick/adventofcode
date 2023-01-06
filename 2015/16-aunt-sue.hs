-- Advent of Code 2015 Day 16 Aunt Sue
-- https://adventofcode.com/2015/day/16

import System.IO
import qualified Data.Map as Map


-- Get first element of a triple
first :: (a, b, c) -> a
first (a, b, c) = a

-- Get second element of a triple
second :: (a, b, c) -> b
second (a, b, c) = b

-- Get third element of a triple
third :: (a, b, c) -> c
third (a, b, c) = c


-- Convert an association list with string values to one with integer values
strtointalist :: (Ord a) => [(a, String)] -> [(a, Int)]
strtointalist ((k,v):xs) = (k, read v::Int) : (strtointalist xs)
strtointalist [] = []

-- Create an association list from pairs of arguments.
-- Trailing colons are removed from the keys and
-- trailing commas are removed from the values.
-- Example input:  [ "key1:", "value1", "key2:", "value2" ]
propertypairs :: [String] -> [(String, String)]
propertypairs (x:y:ls) =
  (withouttrailingchar ':' x, withouttrailingchar ',' y) : propertypairs ls
propertypairs (_:[]) = []
propertypairs [] = []

-- Read property descriptions of the form "key: value" into an association list
-- Example input:  [ "key1: value1", "key2: value2" ]
propalist :: [String] -> [(String, Int)]
propalist (s:xs) = strtointalist (propertypairs (words s)) ++ propalist xs
propalist [] = []


-- Remove a character from the end of the string
withouttrailingchar :: Char -> String -> String
withouttrailingchar c s
  | last s == c = init s
  | otherwise = s


-- Extract the value from a Maybe or use a default value
maybeOrDefault :: Maybe a -> a -> a
maybeOrDefault Nothing def = def
maybeOrDefault (Just val) def = val


-- Parse a list of strings into a person description (name, number, properties)
strlisttoperson :: [String] -> (String, Int, Map.Map String Int)
strlisttoperson (x:n:xs) = (x, read (withouttrailingchar ':' n)::Int,
  Map.fromList (strtointalist (propertypairs xs)))
strlisttoperson (x:[]) = ("", 0, Map.fromList [])
strlisttoperson [] = ("", 0, Map.fromList [])

-- Read all person descriptions from a list of strings
personlist :: [String] -> [(String, Int, Map.Map String Int)]
personlist [] = []
personlist (s:[]) = strlisttoperson (words s) : []
personlist (s:xs) = (strlisttoperson (words s)) : (personlist xs)

-- Test whether a person matches a certain condition
testperscondition :: String -> Int -> (String, Int, Map.Map String Int) -> Bool
testperscondition key val pers
  | found == Nothing = True   -- No information about the person, accept it as match
  | otherwise = val == maybeOrDefault found (-1)
  where found = Map.lookup key (third pers)

-- Test whether the person matches the list of conditions
testmatchperson :: [(String, Int)] -> (String, Int, Map.Map String Int) -> Bool
testmatchperson ((k, v):conds) pers =
  (testperscondition k v pers) && (testmatchperson conds pers)
testmatchperson [] pers = True   -- empty condition always matches

-- Find persons with matching conditions in a list of people
findmatchingpersons :: ((a, b, c) -> Bool) -> [(a, b, c)] -> [b]
findmatchingpersons f (p:lp) = if (f p)
  then (second p) : (findmatchingpersons f lp)
  else (findmatchingpersons f lp)
findmatchingpersons pred [] = []


-- Input conditions
conditionalist = propalist [
  "children: 3",
  "cats: 7",
  "samoyeds: 2",
  "pomeranians: 3",
  "akitas: 0",
  "vizslas: 0",
  "goldfish: 5",
  "trees: 3",
  "cars: 2",
  "perfumes: 1"
  ]

-- Test set of 2 aunts
testLines = [
  "Unnamed 1: children: 2, trees 3, trailing-nonsense",
  "Noname 5: cats: 7, children: 3"
  ]
testPersons = personlist testLines

main = do
  putStrLn "--- Tests ---"
  putStrLn ("Condition assoclist:  " ++ show conditionalist)
  putStrLn ("Test persons:  " ++ (show testPersons))
  putStrLn ("Matching persons:  " ++
    (show (findmatchingpersons (testmatchperson conditionalist) testPersons)) )
  putStrLn ""

  putStrLn "--- Aufgabe 1 ---"
  fileHandle <- openFile "16-aunt-sue-input.txt" ReadMode
  inputText <- hGetContents fileHandle
  putStrLn ("Read lines: " ++ show (length (lines inputText)))
  putStrLn ("First person: " ++ show (head (personlist (lines inputText))))
  putStrLn ("Last person:  " ++ show (last (personlist (lines inputText))))
  putStrLn ("Matching persons:  " ++
    show (findmatchingpersons (testmatchperson conditionalist)
            (personlist (lines inputText))))
  hClose fileHandle

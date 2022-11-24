#! /usr/bin/env python

# Advent of Code 2015, Day 10 Elves Look, Elves Say
# https://adventofcode.com/2015/day/10

ExampleDiigits = "1"


def LookAndSayOnce (s: int) -> str:
    i = 0
    sentence: str = ""
    while i < len (s):
        j = 1   # Anzahl gleicher Ziffern
        while i + j < len (s) and s[i] == s[i + j]:  j += 1
        sentence += str (j) + s[i]
        i += j
    return sentence


def LookAndSayMany (startseq: str, howmany: int) -> str:
    currseq = startseq
    for i in range (howmany):  currseq = LookAndSayOnce (currseq)
    return currseq
        


if __name__ == "__main__":
    print ("211 ->", LookAndSayOnce ("211"))
    print ("1  -- 5x --> ", LookAndSayMany ("1", 5))
    print ()
    print ("--- Aufgabe 1 ---")
    with open ("10-elves-look-elves-say-input.txt", "rt") as infile:
        inputtext = infile.read().strip()
    print ("Input text:", inputtext)
    round40 = LookAndSayMany (inputtext, 40)
    print ("*** After 40 rounds: sentence length {} ***".format (len (round40)))

# -*- coding: UTF-8 -*-

# Advent of Code 2021, Day 3 Binary Diagnostic
# https://adventofcode.com/2021/day/3

from typing import Callable

beispieldiag = """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
"""

def bitliste (l: list, n: int):
    return [ int (m[n]) for m in l ]

def bits_zaehlen (l: list):
    # oder?:  return [ [ int(s) for s in i] for i in zip (*[ s.strip() for s in l ]) ]
    return [ bitliste (l, i) for i in range (len (l[0].strip())) ]

def bitwise_reduce (l, aktion):
    return [ aktion (i) for i in l ]

def gamma_rate (l: list) -> int:
    bitcounts = bitwise_reduce (bits_zaehlen (l), sum)
    gamma_bits = [ 1 if d > len (l) / 2 else 0 for d in bitcounts ]
    gamma = 0
    for b in gamma_bits: gamma = gamma * 2 + b
    return gamma

def epsilon_rate (l: list) -> int:
    bitcounts = bitwise_reduce (bits_zaehlen (l), sum)
    epsilon_bits = [ 1 if d < len (l) / 2 else 0 for d in bitcounts ]
    epsilon = 0
    for b in epsilon_bits: epsilon = epsilon * 2 + b
    return epsilon

def filter_list (l: list, pred: Callable):
    return [ e for e in l if pred (l) ]

def filter_most_common_bit (l: list[str], n: int):
    numbits = sum (bitliste (l, n))
    mostcommonbit = (1 if (numbits >= len (l) / 2) else 0)
    return [ e for e in l if int (e[n]) == mostcommonbit ]

def filter_least_common_bit (l: list[str], n: int):
    numbits = sum (bitliste (l, n))
    leastcommonbit = (1 if (numbits < len (l) / 2) else 0)
    return [ e for e in l if int (e[n]) == leastcommonbit ]

def oxygengenrate (l: list[str]):
    candidates = l
    i = 0
    while (len (candidates) > 1 and len (candidates[0]) > i):
        candidates = filter_most_common_bit (candidates, i)
        i += 1
    if (len (candidates) == 1):  return int (candidates[0], 2)
    return -1

def co2scrubberrate (l: list[str]) -> int:
    candidates = l
    i = 0
    while (len (candidates) > 1 and len (candidates[0]) > i):
        candidates = filter_least_common_bit (candidates, i)
        i += 1
    if (len (candidates) == 1):  return int (candidates[0], 2)
    return -1


print ("--- Beispiel ---")
zeilen = beispieldiag.split()
print ("example input: ", zeilen)
print ("bit counts per digit: ",
    bitwise_reduce (bits_zaehlen (beispieldiag.split()), sum))
gam = gamma_rate (beispieldiag.split ())
eps = epsilon_rate (beispieldiag.split ())
print ("gamma =", gam, ",  epsilon =", eps,
    ",  gamma * epsilon = fuel consumption = ", gam * eps)
oxggen = oxygengenrate (zeilen)
co2scr = co2scrubberrate (zeilen)
print ("oxygen generator rating =", oxggen, ",  CO2 scrubber rating =", co2scr,
    ",  life support rating =", oxggen * co2scr)
print ()

print ("--- Aufgabe 1: Fuel consumption ---")
inputfilename = "03-binary-diag-input.txt"
print ("input from: {}".format (inputfilename))
with open (inputfilename, "rt") as puzzleinput:
    lines = puzzleinput.readlines ()
print ("{} lines of diagnosis data". format (len (lines)))
print ("bit counts: {}".format (bitwise_reduce (bits_zaehlen (lines), sum)))
gam = gamma_rate (lines)
eps = epsilon_rate (lines)
print ("gamma rate = {}, epsilon rate = {}, fuel consumption = {}"
    .format (gam, eps, gam * eps))
print ()

print ("--- Aufgabe 2: Life support rating ---")
oxggen = oxygengenrate (lines)
co2scr = co2scrubberrate (lines)
print ("oxygen generator rating =", oxggen, ",  CO2 scrubber rating =", co2scr,
    ",  life support rating =", oxggen * co2scr)

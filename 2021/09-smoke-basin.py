#! /usr/bin/env python

# Advent of Code 2021 Day 9: Smoke Basin
# https://adventofcode.com/2021/day/9

examplebasin = """2199943210
3987894921
9856789892
8767896789
9899965678
"""

def has_lower_neighbour_left (x, y, basin):
    return x > 0 and x < len (basin[0]) and y >= 0 and y < len (basin) and basin[y][x-1] <= basin[y][x]

def has_lower_neighbour_right (x, y, basin):
    return x >= 0 and x < len (basin[0]) - 1 and y >= 0 and y < len (basin) and basin[y][x+1] <= basin[y][x]

def has_lower_neighbour_up (x, y, basin):
    return x >= 0 and x < len (basin[0]) and y > 0 and y < len (basin) and basin[y-1][x] <= basin[y][x]

def has_lower_neighbour_down (x, y, basin):
    return x >= 0 and x < len (basin[0]) and y >= 0 and y < len (basin) - 1 and basin[y+1][x] <= basin[y][x]

def has_lower_neighbours (x, y, basin):
    return has_lower_neighbour_left (x, y, basin) or has_lower_neighbour_right (x, y, basin) or \
        has_lower_neighbour_up (x, y, basin) or has_lower_neighbour_down (x, y, basin)

def get_low_points (basin: list[str]) -> tuple[int,int,str]:
    return [ (x, y, basin[y][x])
        for y in range (len (basin))
            for x in range (len (basin[0]))
                if not has_lower_neighbours (x, y, basin) ]

def get_risk_level (height: str) -> int:
    return int (height) + 1

def get_risk_levels (heights:list[str]):
    return map (lambda s: get_risk_level (s), heights)

print ("--- Example ---")
examplelowpoints = get_low_points (examplebasin.strip().split ('\n'))
print (f"low points: {examplelowpoints}")
examplerisklevels = list (get_risk_levels (map (lambda triple: triple[2], examplelowpoints)))
print (f"risk levels: {examplerisklevels}, sum = {sum (examplerisklevels)}")
print ("")

print ("--- Aufgabe 1: Low points ---")
inputbasin = open ("09-smoke-basin-input.txt").read().strip().split ('\n')
print (f"{len (inputbasin)} x {len (inputbasin[0])}")
inputlowpoints = get_low_points (inputbasin)
inputrisklevelsum = sum (get_risk_levels (map (lambda triple: triple[2], inputlowpoints)))
# print (f"Low points in input: {inputlowpoints}")
print (f"Sum of risk levels: {inputrisklevelsum}")

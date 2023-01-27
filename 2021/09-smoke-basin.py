#! /usr/bin/env python

# Advent of Code 2021 Day 9: Smoke Basin
# https://adventofcode.com/2021/day/9

import math
import matplotlib.pyplot

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

def get_largest_numbers (numlist: list, n: int) -> int:
    nl = numlist.copy()
    nl.sort (reverse = True)
    return nl[0:n]

class Basin:
    def __init__ (self, lowx: int, lowy: int, size = 0):
        self.x = lowx
        self.y = lowy
        self.lowest = '9'
        self.numpoints = size
    def __str__ (self):
        return f"Basin around ({self.x}, {self.y}) with size {self.numpoints}"
    def __repr__ (self):
        return f"Basin ({self.x}, {self.y}, {self.lowest}, {self.numpoints})"

def filled_basins (basin):
    width = len (basin[0])
    height = len (basin)
    # Keep track of which point belongs to which basin, indices starting from 0, "no basin" = -1
    basinmap = list ()
    for h in range (height):  basinmap.append ([ -1] * width )
    # print (basin)
    # Fill lowest points, then fill neighbours if equal or higher
    numcycles = 0
    changesdone = True
    numbasins = 0
    basinlist = list ()
    while changesdone:
        changesdone = False
        numcycles += 1
        for y in range (height):
            for x in range (width):
                # Decide points which are not yet part of a basin
                if basinmap[y][x] < 0 and basin[y][x] < '9':
                    # Equal or lower neighbour? -- Part of the same basin
                    if has_lower_neighbour_left (x, y, basin):
                        if basinmap[y][x-1] >= 0:
                            basinmap[y][x] = basinmap[y][x-1]
                            basinlist[basinmap[y][x-1]].numpoints += 1
                            changesdone = True
                            continue
                    if has_lower_neighbour_right (x, y, basin):
                        if basinmap[y][x+1] >= 0:
                            basinmap[y][x] = basinmap[y][x+1]
                            basinlist[basinmap[y][x+1]].numpoints += 1
                            changesdone = True
                            continue
                    if has_lower_neighbour_up (x, y, basin):
                        if basinmap[y-1][x] >= 0:
                            basinmap[y][x] = basinmap[y-1][x]
                            basinlist[basinmap[y-1][x]].numpoints += 1
                            changesdone = True
                            continue
                    if has_lower_neighbour_down (x, y, basin):
                        if basinmap[y+1][x] >= 0:
                            basinmap[y][x] = basinmap[y+1][x]
                            basinlist[basinmap[y+1][x]].numpoints += 1
                            changesdone = True
                            continue
                    # No neigbour at equal or lower height? -- A new low point of a basin was found
                    if not has_lower_neighbours (x, y, basin):
                        basinmap[y][x] = numbasins
                        numbasins += 1
                        b = Basin (x, y)
                        b.numpoints = 1
                        b.lowest = basin[y][x]
                        basinlist.append (b)
                        changesdone = True
    pass
    print (f"Cycles done: {numcycles}, basins found: {numbasins}")
    return basinlist, basinmap

def print_basinmap (heightmap: list[list[str]], basinmap:list[list[int]]) -> None:
    for l in range (len (basinmap)):
        for c in range (len (basinmap[l])):
            basindex = basinmap[l][c]
            localheight = heightmap[l][c]
            bascolor = f"{31 + basindex  % 6};40" if basindex >= 0 else "30;47"
            bascolor = f"38;5;{basindex  % 216};40" if basindex >= 0 else "30;47"
            print (f"\033[{bascolor}m{localheight}\033[m", end='')
        print ("")

def plot_basinmap (heightmap: list[list[str]], basinmap:list[list[int]]) -> None:
    fig, ax = matplotlib.pyplot.subplots ()
    intmap = [ [ int (s) for s in y ] for y in heightmap ]
    img = ax.imshow (intmap, cmap='magma')
    fig.colorbar (img, ax=ax)
    matplotlib.pyplot.show ()


print ("--- Example ---")
examplelowpoints = get_low_points (examplebasin.strip().split ('\n'))
print (f"low points: {examplelowpoints}")
examplerisklevels = list (get_risk_levels (map (lambda triple: triple[2], examplelowpoints)))
print (f"risk levels: {examplerisklevels}, sum = {sum (examplerisklevels)}")
print ("")
examplebaslist, examplebasmap = filled_basins (examplebasin.strip().split('\n'))
examplerisksum = sum (map (lambda bas: get_risk_level (bas.lowest), examplebaslist))
example3largest = get_largest_numbers (list (map (lambda bas: bas.numpoints, examplebaslist)), 3)
examplesizeprod = math.prod (example3largest)
print (f"Basins: {examplebaslist}")
print (f"Basin map: {examplebasmap}")
print_basinmap (examplebasin.strip().split ('\n'), examplebasmap)
plot_basinmap (examplebasin.strip().split ('\n'), examplebasmap)
print (f"Risk level sum: {examplerisksum}")
print (f"Largest numbers: {example3largest}")
print (f"Size product: {examplesizeprod}")
print ("")

print ("--- Aufgabe 1: Low points ---")
inputbasin = open ("09-smoke-basin-input.txt").read().strip().split ('\n')
print (f"{len (inputbasin)} x {len (inputbasin[0])}")
inputlowpoints = get_low_points (inputbasin)
inputrisklevelsum = sum (get_risk_levels (map (lambda triple: triple[2], inputlowpoints)))
# print (f"Low points in input: {inputlowpoints}")
print (f"Sum of risk levels: {inputrisklevelsum}")
print ("")

print ("--- Aufgabe 2: Basin size ---")
filled, fillmap = filled_basins (inputbasin)
print_basinmap (inputbasin, fillmap)
plot_basinmap (inputbasin, fillmap)
print (f"Filled basins: {len (filled)}")
risksum = sum (map (lambda bas: get_risk_level (bas.lowest), filled))
print (f"Risk level sum: {risksum}")
largest3sizeproduct = math.prod (get_largest_numbers (list (map (lambda bas: bas.numpoints, filled)), 3))
print (f"Product of size of 3 largest basins: {largest3sizeproduct}")

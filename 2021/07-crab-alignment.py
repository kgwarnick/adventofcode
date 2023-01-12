#! /usr/bin/env python

import matplotlib.pyplot
from matplotlib.markers import MarkerStyle
from matplotlib.textpath import TextPath
from matplotlib.transforms import Affine2D


examplepositions = "16,1,2,0,4,2,7,1,2,14"

def fuelconsumptionconstant (dist: int) -> int:
    return abs (dist)

def fuelconsumptionlinear (dist: int) -> int:
    return abs(dist) * (abs(dist) + 1) // 2   # sum of numbers 1 ... abs(dist)

# List of fuel needs
def fuelneeded (targetpos: int, positionlist: list[int], consumpfunc = fuelconsumptionconstant):
    return [ consumpfunc (t - targetpos) for t in positionlist ]

def totalfuelneeded (targetpos: int, positionstext: str, consumptionfunc = fuelconsumptionconstant):
    return sum (fuelneeded (targetpos, [ int(s) for s in positionstext.split(",") ], consumptionfunc))

def runpuzzle (positionlist: str, consumpfunc, verbose: bool = False) -> tuple[int, int|None, list[int], list[int]]:
    """Determine the best target x position minimising fuel consumption,
    using the specified consumption function (distance -> fuel consumption).

    Returns four values:  the best x position, the fuel requirement for this position,
      a list of tried x positions and a list of corresponding fuel requirements for these positions
    """
    positions = [ int(s) for s in positionlist.split(",") ]
    print ("Number of crab positions:", len (positions))
    minpos = min (positions)
    maxpos = max (positions)
    print ("Need to take positions into account from {0} to {1}".format (minpos, maxpos))
    bestpos = minpos - 1
    bestfuel = None
    poslist = list ()
    fuellist = list()
    for p in range (minpos, maxpos + 1):
        f = sum (fuelneeded (p, positions, consumpfunc))
        poslist.append (p)
        fuellist.append (f)
        if verbose:  print ("- Fuel needed for position {0}: {1}".format (p, f))
        if bestfuel == None or bestfuel > f:
            # better position found
            bestpos = p
            bestfuel = f
    return bestpos, bestfuel, poslist, fuellist

def plotfuelrequirements (pos1: list, req1: list, bestpos1: int, pos2: list, req2: list, bestpos2: int):
    """Plot two pairs of position to fuel mappings, and mark the best position for both cases."""
    fig, ax = matplotlib.pyplot.subplots ()
    ax.broken_barh ([(bestpos1 - 0.75, 1.5), (bestpos2 - 0.75, 1.5)],
                    (0, max (max (req1), max (req2))),
                    facecolors=("#80C080", "#C08080"))
    ax.plot (pos1, req1)
    ax.plot (pos2, req2)
    ax.set_yscale ("log")
    ax.set_title ("Fuel requirements")
    ax.set_xlabel ("Target x position")
    ax.set_ylabel ("Needed fuel")
    matplotlib.pyplot.show ()

def drawpositions (positionlist: str, targetpos1: int, targetpos2: int):
    positions = [ int(s) for s in positionlist.split(",") ]
    fig, ax = matplotlib.pyplot.subplots ()
    ax.broken_barh ([(targetpos1 - 0.75, 1.5)], (0, len (positions) + 1), color="#80C080")
    ax.broken_barh ([(targetpos2 - 0.75, 1.5)], (0, len (positions) + 1), color="#C08080")
    for i in range (len (positions)):
        ax.plot (positions[i], i + 0.75, marker=MarkerStyle (TextPath ((-4, 0), "X", size=12), transform=Affine2D().scale(5)), color="#8080C0")
    ax.set_xlabel ("x position")
    ax.set_ylabel ("crab index")
    ax.grid (True)
    matplotlib.pyplot.show ()

if __name__ == "__main__":
    print ("--- Example with constant fuel consumption ---")
    bestposex1, leastfuel1, poslistex1, fuellistex1 = runpuzzle (examplepositions, fuelconsumptionconstant, verbose=True)
    print ("Best position: {0} -> fuel needed: {1}".format (bestposex1, leastfuel1))
    print ("Position  1 -> fuel needed: ", totalfuelneeded ( 1, examplepositions, fuelconsumptionconstant))
    print ("Position  3 -> fuel needed: ", totalfuelneeded ( 3, examplepositions, fuelconsumptionconstant))
    print ("Position 10 -> fuel needed: ", totalfuelneeded (10, examplepositions, fuelconsumptionconstant))
    print ()

    print ("--- Aufgabe 1: Constant fuel per distance ---")
    positionstr = open ("07-crab-alignment-input.txt").read()
    bestpos1, leastfuel1, poslist1, fuellist1 = runpuzzle (positionstr, fuelconsumptionconstant, verbose=False)
    print ("*** Best position: {0} -> needs fuel: {1} ***".format (bestpos1, leastfuel1))
    print ()

    print ("--- Example with linearly increasing fuel consumption ---")
    bestposex2, leastfuel2, poslistex2, fuellistex2 = runpuzzle (examplepositions, fuelconsumptionlinear, verbose=True)
    print ("Best position: {0} -> fuel needed: {1}".format (bestposex2, leastfuel2))
    print ("Position  2 -> fuel needed: ", totalfuelneeded ( 2, examplepositions, fuelconsumptionlinear))
    print ()
    plotfuelrequirements (poslistex1, fuellistex1, bestposex1, poslistex2, fuellistex2, bestposex2)
    drawpositions (examplepositions, bestposex1, bestposex2)

    print ("--- Aufgabe 2: Fuel consumption increases with distance ---")
    fuel2forpos1 = totalfuelneeded (bestpos1, positionstr, fuelconsumptionlinear)
    print ("- Best position for constant fuel consumption: {0} -> now needs fuel: {1}".format (bestpos1, fuel2forpos1))
    bestpos2, leastfuel2, poslist2, fuellist2 = runpuzzle (positionstr, fuelconsumptionlinear, verbose=False)
    print ("*** Best position: {0} -> needs fuel: {1} ***".format (bestpos2, leastfuel2))
    plotfuelrequirements (poslist1, fuellist1, bestpos1, poslist2, fuellist2, bestpos2)
    drawpositions (positionstr, bestpos1, bestpos2)

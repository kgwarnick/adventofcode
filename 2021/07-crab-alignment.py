#! /usr/bin/env python

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

def runpuzzle (positionlist: str, consumpfunc, verbose: bool = False) -> tuple[int, int|None]:
    positions = [ int(s) for s in positionlist.split(",") ]
    print ("Number of crab positions:", len (positions))
    minpos = min (positions)
    maxpos = max (positions)
    print ("Need to take positions into account from {0} to {1}".format (minpos, maxpos))
    bestpos = minpos - 1
    bestfuel = None
    for p in range (minpos, maxpos + 1):
        f = sum (fuelneeded (p, positions, consumpfunc))
        if verbose:  print ("- Fuel needed for position {0}: {1}".format (p, f))
        if bestfuel == None or bestfuel > f:
            # better position found
            bestpos = p
            bestfuel = f
    return bestpos, bestfuel


if __name__ == "__main__":
    print ("--- Example with constant fuel consumption ---")
    bestpos1, leastfuel1 = runpuzzle (examplepositions, fuelconsumptionconstant, verbose=True)
    print ("Best position: {0} -> fuel needed: {1}".format (bestpos1, leastfuel1))
    print ("Position  1 -> fuel needed: ", totalfuelneeded ( 1, examplepositions, fuelconsumptionconstant))
    print ("Position  3 -> fuel needed: ", totalfuelneeded ( 3, examplepositions, fuelconsumptionconstant))
    print ("Position 10 -> fuel needed: ", totalfuelneeded (10, examplepositions, fuelconsumptionconstant))
    print ()

    print ("--- Aufgabe 1: Constant fuel per distance ---")
    positionstr = open ("07-crab-alignment-input.txt").read()
    bestpos1, leastfuel1 = runpuzzle (positionstr, fuelconsumptionconstant, verbose=False)
    print ("*** Best position: {0} -> needs fuel: {1} ***".format (bestpos1, leastfuel1))
    print ()

    print ("--- Example with linearly increasing fuel consumption ---")
    bestpos2, leastfuel2 = runpuzzle (examplepositions, fuelconsumptionlinear, verbose=True)
    print ("Best position: {0} -> fuel needed: {1}".format (bestpos2, leastfuel2))
    print ("Position  2 -> fuel needed: ", totalfuelneeded ( 2, examplepositions, fuelconsumptionlinear))
    print ()

    print ("--- Aufgabe 2: Fuel consumption increases with distance ---")
    fuel2forpos1 = totalfuelneeded (bestpos1, positionstr, fuelconsumptionlinear)
    print ("- Best position for constant fuel consumption: {0} -> now needs fuel: {1}".format (bestpos1, fuel2forpos1))
    bestpos2, leastfuel2 = runpuzzle (positionstr, fuelconsumptionlinear, verbose=False)
    print ("*** Best position: {0} -> needs fuel: {1} ***".format (bestpos2, leastfuel2))

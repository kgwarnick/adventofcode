#! /usr/bin/env python

examplepositions = "16,1,2,0,4,2,7,1,2,14"

# List of fuel needs
def fuelneeded (targetpos, positionlist):
    return [ abs(t - targetpos) for t in positionlist ]

def runpuzzle1 (positionlist: str, verbose: bool = False) -> tuple[int, int|None]:
    positions = [ int(s) for s in positionlist.split(",") ]
    print ("Number of crab positions:", len (positions))
    minpos = min (positions)
    maxpos = max (positions)
    print ("Need to take positions into account from {0} to {1}".format (minpos, maxpos))
    bestpos = minpos - 1
    bestfuel = None
    for p in range (minpos, maxpos + 1):
        f = sum (fuelneeded (p, positions))
        if verbose:  print ("- Fuel needed for position {0}: {1}".format (p, f))
        if bestfuel == None or bestfuel > f:
            # better position found
            bestpos = p
            bestfuel = f
    print ("Best position: {0} -> Fuel needed: {1}".format (bestpos, bestfuel))
    return bestpos, bestfuel


if __name__ == "__main__":
    print ("--- Example ---")
    print (runpuzzle1 (examplepositions, verbose=True))
    print ()

    print ("--- Aufgabe 1 ---")
    print (runpuzzle1 (open ("07-crab-alignment-input.txt").read()))

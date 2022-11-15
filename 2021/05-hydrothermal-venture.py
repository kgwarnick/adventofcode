# Advent of Code 2021, Day 5 Hydrothermal Venture
# https://adventofcode.com/2021/day/5

exampleinput = """0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2"""


loglevel = 1

def parselines (s: str):
    return [ tuple ( tuple ( int (c.strip()) for c in p.strip().split(',') ) for p in l.strip().split(' -> ') ) for l in s.split('\n') if len (l.strip()) > 0 ]

def ishoriz (seg) -> bool:
    return seg[0][1] == seg[1][1]

def isverti (seg) -> bool:
    return seg[0][0] == seg[1][0]

def isdiag (seg) -> bool:
    # for diagonals the difference in x is equal to +/- the difference in y -- UNTESTED
    return (seg[0][0] - seg[1][0] == seg[0][1] - seg[1][1]) or (seg[0][0] - seg[1][0] == - (seg[0][1] - seg[1][1]))

def pointsx (lineseg: tuple[tuple[int,int], tuple[int,int]]) -> list[tuple[int,int]]:
    """Punktliste entlang x-Richtung"""
    if lineseg[0][0] < lineseg[1][0]:
        startpt = 0; endpt = 1
    else:
        startpt = 1; endpt = 0
    return [ ( x, (lineseg[endpt][1] - lineseg[startpt][1]) * x // (lineseg[endpt][0] - lineseg[startpt][0]) + lineseg[startpt][1])
        for x in range (lineseg[startpt][0], lineseg[endpt][0] + 1) ]

def pointsy (lineseg: tuple[tuple[int,int], tuple[int,int]]) -> list[tuple[int,int]]:
    """Punktliste entlang y-Richtung"""
    if lineseg[0][1] < lineseg[1][1]:
        startpt = 0; endpt = 1
    else:
        startpt = 1; endpt = 0
    return [ ( (lineseg[endpt][0] - lineseg[startpt][0]) * y // (lineseg[endpt][1] - lineseg[startpt][1]) + lineseg[startpt][0], y )
        for y in range (lineseg[startpt][1], lineseg[endpt][1] + 1) ]


def ventpointshv (linesegments: list[tuple[tuple[int, int], tuple[int,int]]]) -> dict[tuple[int,int], int]:
    d = dict()
    for lseg in linesegments:
        if ishoriz (lseg):
            if (loglevel >= 2):  print (f"- Adding segment {lseg} as horizontal segment")
            for p in pointsx (lseg):
                if (loglevel >= 3):  print (f"- Adding point {p}")
                d[p] = d.get (p, 0) + 1
        if isverti (lseg):
            if (loglevel >= 2):  print (f"- Adding segment {lseg} as vertical segment")
            for p in pointsy (lseg):
                if (loglevel >= 3):  print (f"- Adding point {p}")
                d[p] = d.get (p, 0) + 1
    return d


if __name__ == "__main__":
    print ("--- Beispiel ---")
    loglevel = 3
    parsedlines = parselines (exampleinput)
    print (parsedlines)
    points = ventpointshv (parsedlines)
    print ("Points: {}".format (points))
    atleast2 = [ v for v in points if points[v] > 1 ]
    print (f"Dangerous points (at least 2 vents): {atleast2}, number: {len (atleast2)}")
    print ()

    print ("--- Aufgabe 1: Find dangerous points from horizontal and vertical vents ---")
    loglevel = 1
    parsedlines = parselines (open ("05-hydrothermal-venture-input.txt", "rt").read())
    print ("Vents: {}".format (len (parsedlines)));
    points = ventpointshv (parsedlines)
    print ("Points: {}".format (len (points)))
    atleast2 = [ v for v in points if points[v] > 1 ]
    print (f"Dangerous points (at least 2 vents): {len (atleast2)}")

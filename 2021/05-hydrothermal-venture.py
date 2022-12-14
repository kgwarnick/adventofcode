# Advent of Code 2021, Day 5 Hydrothermal Venture
# https://adventofcode.com/2021/day/5

import matplotlib.pyplot
import numpy

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

def lineval (x, xstart, xend, ystart, yend):
    """y-Wert für x-Wert auf einer Gerade von (xstart, ystart) nach (xend, yend) berechnen"""
    return (yend - ystart) * (x - xstart) // (xend - xstart) + ystart

def pointsx (lineseg: tuple[tuple[int,int], tuple[int,int]]) -> list[tuple[int,int]]:
    """Punktliste entlang x-Richtung"""
    if lineseg[0][0] < lineseg[1][0]:
        startpt = 0; endpt = 1
    else:
        startpt = 1; endpt = 0
    return [ (x, lineval (x, lineseg[startpt][0], lineseg[endpt][0], lineseg[startpt][1], lineseg[endpt][1]))
        for x in range (lineseg[startpt][0], lineseg[endpt][0] + 1) ]

def pointsy (lineseg: tuple[tuple[int,int], tuple[int,int]]) -> list[tuple[int,int]]:
    """Punktliste entlang y-Richtung"""
    if lineseg[0][1] < lineseg[1][1]:
        startpt = 0; endpt = 1
    else:
        startpt = 1; endpt = 0
    return [ (lineval (y, lineseg[startpt][1], lineseg[endpt][1], lineseg[startpt][0], lineseg[endpt][0]), y )
        for y in range (lineseg[startpt][1], lineseg[endpt][1] + 1) ]


def ventpointshvd (linesegments: list[tuple[tuple[int, int], tuple[int,int]]], withdiag: bool = False) -> dict[tuple[int,int], int]:
    """Dictionary erzeugen mit Zuordnung, wie viele Vents über jeden Punkt verlaufen:  (x, y) -> n
    
    Nur Punkte mit mindestens einem Vent sind aufgeführt"""
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
        if withdiag and isdiag (lseg):
            if (loglevel >= 2):  print (f"- Adding segment {lseg} as diagonal segment")
            # Nur 45-Grad-Diagonalen => Keine steilere Richtung, also kann entlang x oder y gegangen werden
            for p in pointsy (lseg):
                if (loglevel >= 3):  print (f"- Adding point {p}")
                d[p] = d.get (p, 0) + 1
    return d


def erzeugeventkarte (ventdict: dict[tuple[int,int], int], width: int = None, height: int = None) -> list[list[int]]:
    """Liste von Listen mit Werten zu jedem Punkt, auch Null-Werten, erzeugen aus Dictionary ohne Null-Werten"""
    # keine Breite oder Höhe angegeben, selbst bestimmen als größte x- oder y-Koordinate
    if (width == None):
        width = max (map (lambda punkt: punkt[0], ventdict))
    if (height == None):
        height = max (map (lambda punkt: punkt[1], ventdict))
    # Liste von Listen mit Anzahl erzeugen
    ventmap = list ()
    for y in range (height + 1):
        ventline = [ ventdict.get ((x, y), 0) for x in range (width + 1) ]
        ventmap.append (ventline)
    return ventmap

def zeichneventkarte (ventdict: dict[tuple[int,int], int], plottext = True, plotgraphics = False, width: int = None, height: int = None) -> None:
    ventmap = erzeugeventkarte (ventdict, width, height)
    # Karte in Textform ausgeben
    if plottext:
        for y in range (len (ventmap)):
            print (f"{y:3d}|", end="")
            # Zeichen wählen für Anzahl:  '1' ... '9' oder '.' für 0, '*' für 10 oder mehr
            ventstr = "".join (map (lambda v: str (v) if (v > 0 and v < 10) else '*' if v >= 10 else ".", ventmap[y]))
            print (ventstr, "|", sep="")
    # Karte mit matplotlib darstellen
    if (plotgraphics):
        fig, ax = matplotlib.pyplot.subplots ()
        ax.imshow (ventmap)
        matplotlib.pyplot.show ()


if __name__ == "__main__":
    print ("--- Beispiel ---")
    loglevel = 2
    parsedlines = parselines (exampleinput)
    print (parsedlines)
    points = ventpointshvd (parsedlines)
    print ("Points: {}, number: {}".format (points, len (points)))
    atleast2 = [ v for v in points if points[v] > 1 ]
    print (f"Dangerous points (at least 2 vents): {atleast2}, number: {len (atleast2)}")
    print ()
    pointswithdiag = ventpointshvd (parsedlines, True)
    print (f"Points including diagonal vents: {pointswithdiag}, number: {len (pointswithdiag)}")
    atleast2 = [ v for v in pointswithdiag if pointswithdiag[v] > 1 ]
    print (f"Dangerous points (at least 2 vents): {atleast2}, number: {len (atleast2)}")
    print ()
    zeichneventkarte (pointswithdiag, plottext=True, plotgraphics=True)

    print ("--- Aufgabe 1: Find dangerous points from horizontal and vertical vents ---")
    loglevel = 1
    parsedlines = parselines (open ("05-hydrothermal-venture-input.txt", "rt").read())
    print ("Vents: {}".format (len (parsedlines)));
    points = ventpointshvd (parsedlines)
    print ("Points: {}".format (len (points)))
    atleast2 = [ v for v in points if points[v] > 1 ]
    print (f"Dangerous points (at least 2 vents): {len (atleast2)}")
    print ()
    print ("--- Aufgabe 2: Find dangerous points also from diagonal vents ---")
    pointswithdiag = ventpointshvd (parsedlines, True)
    print (f"With diagonal vents: {len (pointswithdiag)}")
    atleast2 = [ v for v in pointswithdiag if pointswithdiag[v] > 1 ]
    print (f"Dangerous points (at least 2 vents): {len (atleast2)}")
    zeichneventkarte (pointswithdiag, plottext=False, plotgraphics=True)

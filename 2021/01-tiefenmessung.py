from typing import Callable


def änderungsrichtung (jetzt: int, vorher: int) -> int:
    """Ermittelt die Richtung der Änderung von der vorherigen zur jetzigen Messung:
     1  für größer geworden
    -1  für kleiner geworden
     0  keine Änderung
    """
    return 1 if jetzt > vorher else -1 if jetzt < vorher else 0

def änderungen (tiefe: list) -> list:
    # - Für Änderungsrichtung als { -1, 0, 1 }
    # return [ änderungsrichtung(tiefe[i], tiefe[i-1]) for i in range (1, len(tiefe)) ]
    # - Für Differenzen der Werte
    return [ tiefe[i] - tiefe[i-1] for i in range (1, len(tiefe)) ]


# Testfälle
tiefenbeispiel1 = [ 199, 200, 208, 210, 200, 207, 240, 269, 260, 263 ]
print ("Beispiel: {}".format (tiefenbeispiel1))
print ("- Anzahl Zunahmen:     ", len ([ ä for ä in änderungen (tiefenbeispiel1) if ä > 0 ]))
print ("- Anzahl Abnahmen:     ", len ([ ä for ä in änderungen (tiefenbeispiel1) if ä < 0 ]))
print ("- Anzahl ohne Änderung:", len ([ ä for ä in änderungen (tiefenbeispiel1) if ä < 0 ]))


# Aufgabe 1: Wie viele Zunahmen der Tiefe
print ()
print ("--- Aufgabe 1: Wie viele Zunahmen der Tiefen ---")
tiefenlog = [ int(line) for line in open("01-input.txt").readlines() ]
änderungslog = änderungen (tiefenlog)
print ("Tiefenmessungen: {}".format (len (tiefenlog)))
print ("Änderungen:      {}".format (len (änderungslog)))
print ("Anzahl tiefer:       {:4d}".format (len ([ i for i in änderungslog if i > 0 ])))
print ("Anzahl höher:        {:4d}".format (len ([ i for i in änderungslog if i < 0 ])))
print ("Anzahl unverändert:  {:4d}".format (len ([ i for i in änderungslog if i == 0 ])))

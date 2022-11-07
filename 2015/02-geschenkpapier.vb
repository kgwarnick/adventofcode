REM Advent of Code 2015, Day 2: I Was Told There Would Be No Math (Geschenke einpacken)
REM https://adventofcode.com/2015/day/2


Option Explicit


Module GeschenkeEinpacken

Function RechteckFläche (a As Integer, b As Integer) As Integer
  RechteckFläche = a * b
End Function

Function QuaderOberfläche (l As Integer, b As Integer, h As Integer) As Integer
  QuaderOberfläche = 2 * (l * b + l * h + b * h)
End Function

Function KleinsteQuaderFläche (l As Integer, b as Integer, h As Integer) As Integer
  If l >= b And l >= h Then
    KleinsteQuaderFläche = b * h
  Else If b >= l And b >= h Then
    KleinsteQuaderFläche = l * h
  Else
    KleinsteQuaderFläche = l * b
  End If
End Function


Function PapierBedarf (l As Integer, b As Integer, h As Integer) As Integer
  PapierBedarf = QuaderOberfläche (l, b, h) + KleinsteQuaderFläche (l, b, h)
End Function


Function BerechnePapierBedarf (Geschenke() As String) As Integer
  Dim SummePapier As Integer = 0
  For i As Integer = 1 To Geschenke.Length
    if Geschenke (i-1) <> "" Then
      Dim Maße() As String = Split (Geschenke(i-1), "x")
      ' Console.WriteLine ("{0}  {1,3} x {2,3} x {3,3:d}  -> Papier-Bedarf: {4,5}",
      '   i, Maße(0), Maße(1), Maße(2),
      '   PapierBedarf (Int (Maße(0)), Int (Maße(1)), Int (Maße(2))) )
      SummePapier = SummePapier + PapierBedarf (Int (Maße(0)), Int (Maße(1)), Int (Maße(2)))
    End If
  Next
  BerechnePapierBedarf = SummePapier
End Function


Sub Main()
  Console.WriteLine ("--- Beispiele ---")
  Console.WriteLine ("Papier-Bedarf für Geschenk mit Maßen 2x3x4: {0}",
    PapierBedarf (2, 3, 4))
  Console.WriteLine ("Papier-Bedarf für Geschenk mit Maßen 1x1x10: {0}",
    PapierBedarf (1, 1, 10))
  Dim fileContent As String
  fileContent = My.Computer.FileSystem.ReadAllText ("02-geschenkpapier-input.txt")
  Dim lines() As String = Split (fileContent, Chr$(10))   ' Aufteilen an Zeilenenden
  Console.WriteLine ()

  Console.WriteLine ("--- Aufgabe 1:  Gesamter Papierbedarf ---")
  Dim AnzahlGeschenke As Integer
  If (lines (lines.Length - 1) <> "") Then
    AnzahlGeschenke = lines.Length
  Else
    AnzahlGeschenke = lines.Length - 1   ' Leerzeile am Ende nicht zählen
  End If
  Console.WriteLine ("Papierbedarf für {0} Geschenke:  {1}",
    AnzahlGeschenke, BerechnePapierBedarf (lines))
End Sub

End Module

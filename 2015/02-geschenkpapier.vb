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


Function KleinsterFlächenUmfang (l As Integer, b As Integer, h As Integer) As Integer
  If l >= b And l >= h Then
    KleinsterFlächenUmfang = 2 * (b + h)
  Else If b >= l And b >= h Then
    KleinsterFlächenUmfang = 2 * (l + h)
  Else
    KleinsterFlächenUmfang = 2 * (l + b)
  End If
End Function


Function QuaderVolumen (l As Integer, b As Integer, h As Integer) As Integer
  QuaderVolumen = l * b * h
End Function


Function BandBedarf  (l As Integer, b As Integer, h As Integer) As Integer
  BandBedarf = KleinsterFlächenUmfang (l, b, h) + QuaderVolumen (l, b, h)
End Function


Function BerechnePapierBedarf (Maße(,) As Integer) As Integer
  Dim SummePapier As Integer = 0
  For i As Integer = LBound (Maße, 1) To UBound (Maße, 1)
    ' Console.WriteLine ("{0,4:d}  {1,3:d} x {2,3:d} x {3,3:d}  -> Papier-Bedarf: {4,5}",
    '   i + 1, Maße(i,0), Maße(i,1), Maße(i,2), PapierBedarf (Maße(i,0), Maße(i,1), Maße(i,2)) )
    SummePapier = SummePapier + PapierBedarf (Maße(i,0), Maße(i,1), Maße(i,2))
  Next
  BerechnePapierBedarf = SummePapier
End Function

Function BerechneBandBedarf (Maße(,) As Integer) As Integer
  Dim SummeBand As Integer = 0
  For i As Integer = LBound (Maße, 1) To UBound (Maße, 1)
    SummeBand = SummeBand + BandBedarf (Maße(i,0), Maße(i,1), Maße(i,2))
  Next
  BerechneBandBedarf = SummeBand
End Function


Sub Main()
  Console.WriteLine ("--- Beispiele ---")
  Console.WriteLine ("Papier-Bedarf für Geschenk mit Maßen 2x3x4: {0}",
    PapierBedarf (2, 3, 4))
  Console.WriteLine ("Papier-Bedarf für Geschenk mit Maßen 1x1x10: {0}",
    PapierBedarf (1, 1, 10))
  Console.WriteLine ("Band-Bedarf für Geschenk mit Maßen 2x3x4: {0}",
    BandBedarf (2, 3, 4))
  Console.WriteLine ("Band-Bedarf für Geschenk mit Maßen 1x1x10: {0}",
    BandBedarf (1, 1, 10))

  ' Dateiinhalt lesen und Liste von Geschenk-Maßen vorbereiten
  Dim fileContent As String = FileIO.FileSystem.ReadAllText ("02-geschenkpapier-input.txt")
  Dim zeilen() As String = Split (fileContent, Chr(10))   ' Aufteilen an Zeilenenden
  Dim AnzahlGeschenke As Integer
  If (zeilen (zeilen.Length - 1) <> "") Then
    AnzahlGeschenke = zeilen.Length
  Else
    AnzahlGeschenke = zeilen.Length - 1   ' Leerzeile am Ende nicht zählen
  End If
  Dim Maße(0 To AnzahlGeschenke - 1, 0 To 2) As Integer
  For i As Integer = 0 To AnzahlGeschenke - 1
    If zeilen (i) <> "" Then
      Dim maß() As String = Split (zeilen(i), "x")
      Maße (i, 0) = Int (maß (0)) : Maße (i, 1) = Int (maß (1)) : Maße (i, 2) = Int (maß (2))
    End If
  Next
  Console.WriteLine ()

  Console.WriteLine ("--- Aufgabe 1:  Gesamter Geschenkpapierbedarf ---")
  Console.WriteLine ("Papierbedarf für {0} Geschenke:  {1}",
    AnzahlGeschenke, BerechnePapierBedarf (Maße))
  Console.WriteLine ()

  Console.WriteLine ("--- Aufgabe 2:  Gesamter Geschenkbandbedarf ---")
  Console.WriteLine ("Bandbedarf für {0} Geschenke:  {1}",
    AnzahlGeschenke, BerechneBandBedarf (Maße))
  Console.WriteLine ()
End Sub

End Module

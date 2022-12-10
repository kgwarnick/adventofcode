' Advent of Code 2015 Day 14 Reindeer Olympics
' https://adventofcode.com/2015/day/14

Option Explicit

Module ReindeerOlympics


Public Structure Reindeer
  Public DeerName As String
  Public TopSpeed As Integer
  Public FlyTime As Integer
  Public RestTime As Integer

  Public Sub ParseCharacteristics (description As String)
    Dim s() As String = Split (description, " ")
    If (s.Length <> 15) Then
      Console.WriteLine ("Unexpected number of elements in reindeer description: " +
        "expected 15, found {0}", s.Length)
      Return
    End If
    DeerName = s(0)
    TopSpeed = Int (s(3))
    FlyTime = Int (s(6))
    RestTime = Int (s(13))
  End Sub

  Public Function GetDistance (Seconds As Integer) As Integer
    ' Calculate distance flown after "full fly + rest cycles"
    Dim NumFullCycles As Integer = Seconds \ (FlyTime + RestTime)
    Dim dist As Integer = NumFullCycles * FlyTime * TopSpeed
    ' Add distance from remaining flight seconds
    Dim RemainingSeconds As Integer = Seconds Mod (FlyTime + RestTime)
    If (RemainingSeconds > FlyTime) Then
      dist = dist + TopSpeed * FlyTime
    Else
      dist = dist + TopSpeed * RemainingSeconds
    End If
    ' Return the total distance travelled
    GetDistance = dist
  End Function

  Public Function GetAverageSpeed () As Double
    GetAverageSpeed = TopSpeed * FlyTime / (FlyTime + RestTime)
  End Function
End Structure


' Rennen durchführen über bestimmte Anzahl Sekunden
'
Sub RunRace (Rentiere() As Reindeer, NumSeconds As Integer)
  Dim WinnerName As String = "Nobodeer"
  Dim WinningDistance As Integer = 0
  For i as Integer = 0 To Rentiere.Length - 1
    Dim travelled as Integer = Rentiere(i).GetDistance (NumSeconds)
    Console.WriteLine ("{0,10}  {1,3} km/s for {2,3} s, rest {3,3} s " +
      "--> travelled {4} km",
      Rentiere(i).DeerName, Rentiere(i).TopSpeed, Rentiere(i).FlyTime,
      Rentiere(i).RestTime, travelled)
    If travelled > WinningDistance Then
      WinningDistance = travelled
      WinnerName = Rentiere(i).DeerName
    End If
  Next i
  Console.WriteLine ("Winner: {0} with distance {1} km",
    WinnerName, WinningDistance)
End Sub


' Rennen nach Punkten durchführen über bestimmte Anzahl Sekunden,
' Punkte für Führende in jeder Sekunde
'
Sub RunRaceByPoints (Rentiere() As Reindeer, NumSeconds As Integer)
  Dim Distance(Rentiere.Length - 1) As Integer
  Dim Points(Rentiere.Length - 1) As Integer
  Dim WinnerName As String
  Dim WinningDistance As Integer
  For t As Integer = 1 To NumSeconds
    WinnerName = "Nobodeer"
    WinningDistance = 0
    For i As Integer = 0 To Rentiere.Length - 1
      Distance(i) = Rentiere(i).GetDistance (t)
      If Distance(i) > WinningDistance Then
        WinningDistance = Distance(i)
        WinnerName = Rentiere(i).DeerName
      End If
    Next i
    ' Award one point to everyone in the lead
    For i As Integer = 0 To Rentiere.Length - 1
      If (Distance(i) >= WinningDistance) Then
        Points(i) = Points(i) + 1
      End If
    Next i
  Next t
  ' Final standing
  WinnerName = "Nobodeer"
  Dim WinningPoints As Integer = 0
  For i As Integer = 0 To Rentiere.Length - 1
    Console.WriteLine ("{0,10}  {1,4} points", Rentiere(i).DeerName, Points(i))
    If (Points(i) > WinningPoints) Then
      WinningPoints = Points(i)
      WinnerName = Rentiere(i).DeerName
    End If
  Next i
  Console.WriteLine ("Winner: {0} with {1} points",
    WinnerName, WinningPoints)
End Sub


Sub Main()
  Console.WriteLine ("--- Beispiel: Nach 1000 Sekunden ---")
  Dim Rentiere(1) As Reindeer
  Rentiere(0).ParseCharacteristics ("Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.")
  Rentiere(1).ParseCharacteristics ("Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.")
  RunRace (Rentiere, 1000)
  Console.WriteLine ("")

  RunRaceByPoints (Rentiere, 1000)
  Console.WriteLine ("")

  Console.WriteLine ("--- Aufgabe 1: Nach 2503 Sekunden, weiteste Entfernung ---")
  Dim fileContent As String = FileIO.FileSystem.ReadAllText ("14-reindeer-olympics-input.txt")
  Dim fileLines() As String = Split (fileContent, Chr(10))
  Dim numlines As Integer = fileLines.Length
  If (fileLines (fileLines.Length - 1) = "") Then
    numlines -= 1
  End If
  Console.WriteLine ("Read {0} lines", numlines)
  ReDim Rentiere(numlines - 1)
  For i as Integer = 0 To numlines - 1
    Rentiere(i).ParseCharacteristics (fileLines(i))
  Next i
  RunRace (Rentiere, 2503)
  Console.WriteLine ("")

  Console.WriteLine ("--- Aufgabe 2: Nach 2503 Sekunden, meiste Punkte ---")
  RunRaceByPoints (Rentiere, 2503)
  Console.WriteLine ("")

  Console.WriteLine ("--- Zusatz-Information: Durchschnittsgeschwindigkeiten auf langen Strecken ---")
  For i as Integer = 0 To numlines - 1
    Console.WriteLine ("{0,10}  {1,3} km/s for {2,3} s, rest {3,3} s " +
      "--> average {4} km/s",
      Rentiere(i).DeerName, Rentiere(i).TopSpeed, Rentiere(i).FlyTime,
      Rentiere(i).RestTime, Rentiere(i).GetAverageSpeed())
  Next
End Sub

End Module

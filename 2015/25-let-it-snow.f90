! Advent of Code 2015, Day 15: Let It Snow
! https://adventofcode.com/2015/day/25

Program LetItSnow

Implicit None
Integer (Kind=8) :: StartCode = 20151125
Integer (Kind=8) :: Multiplier = 252533
Integer (Kind=8) :: Divider = 33554393
! Verwendete Funktionen
Integer (Kind=8) :: NaechsterCode, FolgeCode
Integer          :: DiagNummer, CodeNummer
! Lokale Variablen
Integer          :: i

! Aufgabe 1: Code an bestimmter Position
Integer :: FreischaltCodeZeile = 3010
Integer :: FreischaltCodeSpalte = 3019

! Beispiel-Codes
Integer (Kind=8), Dimension(6, 6) :: BeispielCode
BeispielCode(1,:) = (/ 20151125, 18749137, 17289845, 30943339, 10071777, 33511524 /)
BeispielCode(2,:) = (/ 31916031, 21629792, 16929656,  7726640, 15514188,  4041754 /)
BeispielCode(3,:) = (/ 16080970,  8057251,  1601130,  7981243, 11661866, 16474243 /)
BeispielCode(4,:) = (/ 24592653, 32451966, 21345942,  9380097, 10600672, 31527494 /)
BeispielCode(5,:) = (/    77061, 17552253, 28094349,  6899651,  9250759, 31663883 /)
BeispielCode(6,:) = (/ 33071741,  6796745, 25397450, 24659492,  1534922, 27995004 /)

write(*,'(A)') "Beispiel-Codes: "
write(*,'(6(6(5X,I10),/))') (BeispielCode(i,:), i=1,6)

Write (*,'(/,A)') "--- Testfälle ---"

write(*,"(A,I0)") "Start-Code:   ", StartCode
write(*,"(A)")    "Folgende:"
Call TestEinigeCodesAusgeben (StartCode, 8, Multiplier, Divider)

write(*,"(/,A)") "Code-Nummern:"
Call TestCodeNummern ()

write(*,"(/,A)") "Berechnung bestimmter Codes:"
Call TestFolgeCode (StartCode, Multiplier, Divider)
Call TestCodeAnPosition (StartCode, Multiplier, Divider)

Write(*,'(/,A,I0,A,I0,A)') "--- Aufgabe 1: Code an Position (", &
  & FreischaltCodeZeile, ", ", FreischaltCodeSpalte, ") ---"
Write(*,*) "Diagonale:  ", DiagNummer (FreischaltCodeZeile, FreischaltCodeSpalte)
Write(*,*) "Code-Nummer:", CodeNummer (FreischaltCodeZeile, FreischaltCodeSpalte)
Write(*,*) "Code:       ", FolgeCode (StartCode, Multiplier, Divider, &
  & CodeNummer (FreischaltCodeZeile, FreischaltCodeSpalte) - 1)

EndProgram LetItSnow


Integer(Kind=8) Function NaechsterCode (LetzterCode, Multiplier, Divider)
  Implicit None
  Integer (Kind=8), Intent(In) :: LetzterCode, Multiplier, Divider
  NaechsterCode = modulo (LetzterCode * Multiplier, Divider)
EndFunction NaechsterCode


Subroutine TestEinigeCodesAusgeben (StartCode, Anzahl, Multiplier, Divider)
  Implicit None
  Integer (Kind=8), Intent(In) :: StartCode, Multiplier, Divider
  Integer,          Intent(In) :: Anzahl
  Integer (Kind=8) :: code
  Integer          :: i
  Integer (Kind=8) :: NaechsterCode
  code = StartCode
  Do i = 1, Anzahl
    code = NaechsterCode (code, Multiplier, Divider)
    Write (*,*) "- Code:", code
  EndDo
EndSubroutine TestEinigeCodesAusgeben


! Nummer der Diagonale, Zählung beginnend mit 0
! - Koordinate (1, 1) entspricht Diagonale 0, (2, 1) und (1, 2) entsprechen 1, usw.
Integer Function DiagNummer (Zeile, Spalte)
  Implicit None
  Integer :: Zeile, Spalte
  DiagNummer = Zeile + Spalte - 2
EndFunction DiagNummer

! Code-Nummer aus Koordinaten berechnen
Integer Function CodeNummer (Zeile, Spalte)
  Implicit None
  Integer :: Zeile, Spalte
  Integer :: i, j, m, n
  Integer :: DiagNummer, SummeZahlen1bisN
  CodeNummer = SummeZahlen1bisN (DiagNummer (Zeile, Spalte)) + Spalte
EndFunction CodeNummer

Integer Function SummeZahlen1bisN (N)
  Implicit None
  Integer :: N
  SummeZahlen1bisN = N * (N + 1) / 2
EndFunction SummeZahlen1bisN

Subroutine TestCodeNummern ()
  Integer :: CodeNummer
  write (*, "(A,I0)") "Code-Nummer für Koordinate (1,1):  ", CodeNummer (1, 1)
  write (*, "(A,I0)") "Code-Nummer für Koordinate (2,1):  ", CodeNummer (2, 1)
  write (*, "(A,I0)") "Code-Nummer für Koordinate (1,2):  ", CodeNummer (1, 2)
  write (*, "(A,I0)") "Code-Nummer für Koordinate (4,2):  ", CodeNummer (4, 2)
  write (*, "(A,I0)") "Code-Nummer für Koordinate (1,5):  ", CodeNummer (4, 2)
EndSubroutine

! n-ten Folge-Code berechnen
Integer(Kind=8) Function FolgeCode (StartCode, Multiplier, Divider, N)
  Integer(Kind=8), Intent(In) :: StartCode, Multiplier, Divider
  Integer,         Intent(In) :: N
  Integer          :: i
  Integer (Kind=8) :: code
  Integer (Kind=8) :: NaechsterCode
  code = StartCode
  Do i = 1, N
    code = NaechsterCode (code, Multiplier, Divider)
  EndDo
  FolgeCode = code
EndFunction FolgeCode

Subroutine TestFolgeCode (StartCode, Multiplier, Divider)
  Integer(Kind=8), Intent(In) :: StartCode, Multiplier, Divider
  Integer(Kind=8) :: FolgeCode
  write (*, "(A,I0)") "Übernächster Code nach Nummer 1:  ", &
    & FolgeCode (StartCode, Multiplier, Divider, 2)
EndSubroutine

Subroutine TestCodeAnPosition (StartCode, Multiplier, Divider)
  Integer(Kind=8), Intent(In) :: StartCode, Multiplier, Divider
  Integer(Kind=8) :: FolgeCode
  Integer         :: CodeNummer
  write (*, "(A,I0)") "Code an Position (6, 6):  ", &
    & FolgeCode (StartCode, Multiplier, Divider, CodeNummer (6, 6) - 1)
EndSubroutine TestCodeAnPosition 

!      Advent of Code 2015, Day 6 Probably a Fire Hazard
!      https://adventofcode.com/2015/day/6    

00100 Program ProbablyAFireHazard

00110 Implicit None
00120 Logical grid (0:999, 0:999)
00130 Integer anzahlan
00140 Integer i, x1, y1, x2, y2
00150 Character (len=256) dateiname
00160 Integer numanw
00165 Character (len=256) anweisungen (0:999)
00170 Logical parsedok
00180 Character (len=32) operation
C     Funktionen
00190 Integer CountLightsOn
00195 Logical TryParseCommand

00200 Write(*,*) "--- Beispiel ---"
00210 anweisungen (0) = "turn on 0,0 through 999,999"
00211 anweisungen (1) = "toggle 0,0 through 999,0"
00212 anweisungen (2) = "turn off 499,499 through 500,500"
00215 numanw = 3
00220 grid(:,:) = .False.   ! Turn off all lights
00230 Do i = 0, numanw - 1
00240 parsedok = TryParseCommand (anweisungen (i), operation,           &
     & x1, y1, x2, y2)
00245 Write (*,'(A,L3,A,A,A,4I4)') " Command parsed successfully? ",    &
     &  parsedok, ", Action: ", Trim(operation),                        &
     &  ", Corners:", x1, y1, x2, y2
00250 If (parsedok)                                                     &
     &  Call Schalten (x1, y1, x2, y2, 1000, 1000, grid, operation)
00255 anzahlan = CountLightsOn (1000, 1000, grid)
00260 Write(*,*) "-> Lights on:", anzahlan
00270 EndDo
00280 Write(*,*) ""

00300 Write(*,*) "--- Aufgabe 1 ---"
00310 dateiname = "06-fire-hazard-input.txt"
00315 Call ReadInputFile (dateiname, numanw, anweisungen)
00320 grid(:,:) = .False.   ! Turn off all lights
00330 Do i = 0, numanw - 1
00340 parsedok = TryParseCommand (anweisungen (i), operation,           &
     & x1, y1, x2, y2)
C0345 Write (*,'(A,L3,A,A,A,4I4)') " Command parsed successfully? ",    &
C    &  parsedok, ", Action: ", Trim(operation),                        &
C    &  ", Corners:", x1, y1, x2, y2
00350 If (parsedok)                                                     &
     &  Call Schalten (x1, y1, x2, y2, 1000, 1000, grid, operation)
00355 anzahlan = CountLightsOn (1000, 1000, grid)
C0360 Write(*,*) "-> Lights on:", anzahlan
00370 EndDo
00375 Write(*,*) "*** Ergebnis: Lights on:", anzahlan, "***"
00380 Write(*,*) ""

00500 EndProgram ProbablyAFireHazard


01000 Subroutine Schalten (x1, y1, x2, y2, nx, ny, feld, operation)
01010 Implicit None
01020 Integer x1, y1, x2, y2, nx, ny
C1030 Logical feld (nx, ny)
01030 Logical feld (0:999, 0:999)
01040 Integer i, j
01050 Character (len=32) operation
01110 Do i = x1, x2
01120 Do j = y1, y2
01130 If (operation == "turn on") Then
01135   feld (i, j) = .True.
01140 Else If (operation == "turn off") Then
01145   feld (i, j) = .False.
01150 Else If (operation == "toggle") Then
01155   feld (i, j) = .Not. feld (i, j)
01160 EndIf
01180 EndDo
01190 EndDo
01500 EndSubroutine

02000 Integer Function CountLightsOn (nx, ny, feld)
02010 Implicit None
02020 Integer nx, ny
02030 Logical feld (0:999, 0:999)
02040 Integer i, j, n
02045 n = 0
02050 Do i = 0, nx - 1
02060 Do j = 0, ny - 1
02070 If (feld (i, j))  n = n + 1
02080 EndDo
02085 EndDo
02088 CountLightsOn = n
02090 EndFunction

03000 Subroutine ReadInputFile (filename, numzeilen, zeilen)
03010 Character (len=*)   filename
03015 Integer             numzeilen
03020 Character (len=256) zeilen (0:999)
03025 Character (len=256) s
03030 Integer             readok
03040 Open (unit=11, file=filename, err=03100, status="OLD",            &
     & action="READ", iostat=readok)
03042 numzeilen = 0
03045 readok = 0
03050 Do While (readok == 0)
03060   Read (11,"(A)",end=03120) s
03062   If (readok /= 0)  exit
03066   zeilen (numzeilen) = s
03067   numzeilen = numzeilen + 1
03090 EndDo
03095 GoTo 03170
03100 Write (*,*) "Fehler beim Lesen"
03120 Write (*,"(A,A,A,I4)") " Datei ", Trim(filename),                 &
     &  " gelesen, Anweisungen: ", numzeilen
03170 Close (unit=11)
03190 EndSubroutine

C     Parse a command line into action and positions,
C     Returns True on success, False on failure
03200 Logical Function TryParseCommand (command, act, x1, y1, x2, y2)
03210 Character (len=256)   command
03215 Character (len=32)    act
03220 integer               x1, y1, x2, y2, cursor
03225 Logical TryParseAction, TryParseCorner
C     Set faiilure status
03230 TryParseCommand = .False.
03235 cursor = 1
C     Parse the action verb
03240 If (.Not. TryParseAction (command, cursor, act)) Then
03245   Return
03250 EndIf
C     Parse first coordinate pair
03260 If (.Not. TryParseCorner (command, cursor, x1, y1)) Then
03265   Return
03270 EndIf
C     Parse fixed word
03280 If (command(cursor:cursor+7) == "through ") Then
03285   cursor = cursor + 8
03290 Else
03295   Write(*,*) "Failed to read word 'through' in command"
03300   Return
03305 EndIf
C     Parse second coordinate pair
03310 If (.Not. TryParseCorner (command, cursor, x2, y2)) Then
03315   Return
03320 EndIf
C     If thispoint is reached the command could be parsed correctly
03330 TryParseCommand = .True.
03390 EndFunction

03400 Logical Function TryParseAction (command, cursor, act)
03410 Character (len=256)   command
03415 Character (len=32)    act
03420 Integer               cursor
03430 TryParseAction = .False.
03440 if (command(1:9) == "turn off ") Then
03445   act = "turn off"
03450   cursor = cursor + 9
03460 else If (command(1:8) == "turn on ") Then
03465   act = "turn on"
03470   cursor = cursor + 8
03480 Else If (command(1:7) == "toggle ") Then
03485   act = "toggle"
03490   cursor = cursor + 7
03500 Else
03510   return
03520 EndIf
03530 TryParseAction = .True.
03590 EndFunction

03600 Logical Function TryParseCorner (command, cursor, x, y)
03610 Character (len=256)   command
03615 Integer               cursor, x, y
03620 Integer               pos
03630 pos = cursor + Index (command (cursor:len(command)), ",") - 1
03635 Read (command (cursor:pos-1),*) x
03640 cursor = pos + 1
03650 pos = cursor + Index (command (cursor:len(command)), " ") - 1
03655 Read (command (cursor:pos-1),*) y
03660 cursor = pos + 1
03670 If (.True.) Then
03675   TryParseCorner = .True.
03680 Else
03685   TryParseCorner = .False.
03690 EndIf
03700 EndFunction TryParseCorner
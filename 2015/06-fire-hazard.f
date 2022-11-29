!      Advent of Code 2015, Day 6 Probably a Fire Hazard
!      https://adventofcode.com/2015/day/6    

00100 Program ProbablyAFireHazard

00110 Implicit None
00120 Logical grid (0:999, 0:999)
00125 Integer brightnessgrid (0:999, 0:999)
00130 Integer anzahlan, totbright
00140 Integer i, j, x1, y1, x2, y2
00150 Character*256 dateiname
00160 Integer numanw
00165 Character*256 anweisungen (0:999)
00170 Logical parsedok
00180 Character*32 operation
C     Funktionen
00190 Integer CountLightsOn, GetTotalBrightness
00195 Logical TryParseCommand

00200 Write(*,*) '--- Beispiel ---'
00210 anweisungen (0) = 'turn on 0,0 through 999,999'
00211 anweisungen (1) = 'toggle 0,0 through 999,0'
00212 anweisungen (2) = 'turn off 499,499 through 500,500'
00215 numanw = 3
C0220 grid(:,:) = .False.   ! Turn off all lights
00220 Do j = 0, 999
00222 Do i = 0, 999
00224 grid(i,j) = .False.   ! Turn off all lights
00226 End Do
00228 End Do
00230 Do i = 0, numanw - 1
00240 parsedok = TryParseCommand (anweisungen (i), operation,           &
     & x1, y1, x2, y2)
00245 Write (*,'(A,L3,A,A8,A,4I4)') ' Command parsed successfully? ',   &
     &  parsedok, ', Action: ', operation, ', Corners:', x1, y1, x2, y2
00250 If (parsedok)                                                     &
     &  Call Schalten (x1, y1, x2, y2, 1000, 1000, grid, operation)
00255 anzahlan = CountLightsOn (1000, 1000, grid)
00260 Write(*,*) '-> Lights on:', anzahlan
00270 EndDo
00280 Write(*,*)

00300 Write(*,*) '--- With brightness levels ---'
00310 anweisungen (0) = 'turn on 0,0 through 0,0'
00311 anweisungen (1) = 'toggle 0,0 through 999,999'
00315 numanw = 2
C0320 brightnessgrid(:,:) = 0   ! Turn off all lights
00320 Do j = 0, 999
00322 Do i = 0, 999
00324 brightnessgrid(i,j) = 0   ! Turn off all lights
00326 End Do
00328 End Do
00330 Do i = 0, numanw - 1
00340 parsedok = TryParseCommand (anweisungen (i), operation,           &
     & x1, y1, x2, y2)
00345 Write (*,'(A,L3,A,A8,A,4I4)') ' Command parsed successfully? ',   &
     &  parsedok, ', Action: ', operation, ', Corners:', x1, y1, x2, y2
00350 If (parsedok) Call TuneBrightness (                               &
     & x1, y1, x2, y2, 1000, 1000, brightnessgrid, operation)
00355 totbright = GetTotalBrightness (1000, 1000, brightnessgrid)
00360 Write(*,*) '-> Total brightness:', totbright
00370 EndDo
00380 Write(*,*)


00400 Write(*,*) '--- Aufgabe 1 On or Off ---'
00410 dateiname = '06-fire-hazard-input.txt'
00415 Call ReadInputFile (dateiname, numanw, anweisungen)
C0420 grid(:,:) = .False.   ! Turn off all lights
00420 Do j = 0, 999
00422 Do i = 0, 999
00424 grid(i,j) = .False.   ! Turn off all lights
00426 End Do
00428 End Do
00430 Do i = 0, numanw - 1
00440 parsedok = TryParseCommand (anweisungen (i), operation,           &
     & x1, y1, x2, y2)
C0445 Write (*,'(A,L3,A,A,A,4I4)') ' Command parsed successfully? ',    &
C    &  parsedok, ', Action: ', Trim(operation),                        &
C    &  ', Corners:', x1, y1, x2, y2
00450 If (parsedok)                                                     &
     &  Call Schalten (x1, y1, x2, y2, 1000, 1000, grid, operation)
00460 EndDo
00470 anzahlan = CountLightsOn (1000, 1000, grid)
00475 Write(*,*) '*** Ergebnis: Lights on:', anzahlan, '***'
00480 Write(*,*)

00500 Write(*,*) '--- Aufgabe 2 Brightness Levels ---'
C0520 brightnessgrid(:,:) = 0   ! Turn off all lights
00520 Do j = 0, 999
00522 Do i = 0, 999
00524 brightnessgrid(i,j) = 0   ! Turn off all lights
00526 End Do
00528 End Do
00530 Do i = 0, numanw - 1
00540 parsedok = TryParseCommand (anweisungen (i), operation,           &
     & x1, y1, x2, y2)
00550 If (parsedok) Call TuneBrightness (                               &
     & x1, y1, x2, y2, 1000, 1000, brightnessgrid, operation)
00560 EndDo
00570 totbright = GetTotalBrightness (1000, 1000, brightnessgrid)
00575 Write(*,*) '*** Ergebnis: Total Brightness:', totbright, '***'
00580 Write(*,*)

00800 End


01000 Subroutine Schalten (x1, y1, x2, y2, nx, ny, feld, operation)
01010 Implicit None
01020 Integer x1, y1, x2, y2, nx, ny
01030 Logical feld (0:nx-1, 0:ny-1)
C1030 Logical feld (0:999, 0:999)
01040 Integer i, j
01050 Character*32 operation
01110 Do i = x1, x2
01120 Do j = y1, y2
01130 If (operation .eq. 'turn on') Then
01135   feld (i, j) = .True.
01140 Else If (operation .eq. 'turn off') Then
01145   feld (i, j) = .False.
01150 Else If (operation .eq. 'toggle') Then
01155   feld (i, j) = .Not. feld (i, j)
01160 End If
01180 End Do
01190 End Do
01200 End

01000 Subroutine TuneBrightness (x1, y1, x2, y2, nx, ny, feld,          &
     & operation)
01010 Implicit None
01020 Integer x1, y1, x2, y2, nx, ny
01030 Integer feld (0:nx-1, 0:ny-1)
C1030 Logical feld (0:999, 0:999)
01040 Integer i, j
01050 Character*32 operation
01110 Do i = x1, x2
01120 Do j = y1, y2
01130 If (operation .eq. 'turn on') Then
01135   feld (i, j) = feld (i, j) + 1
01140 Else If (operation .eq. 'turn off') Then
01150   If (feld (i, j) .gt. 1) Then
01155     feld (i, j) = feld (i, j) - 1
01160   Else
01165     feld (i, j) = 0
01170   EndIf
01180 Else If (operation .eq. 'toggle') Then
01185   feld (i, j) = feld (i, j) + 2
01190 EndIf
01200 EndDo
01210 EndDo
01250 End

02000 Function CountLightsOn (nx, ny, feld)
02010 Implicit None
02015 Integer CountLightsOn
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
02090 End

02100 Integer Function GetTotalBrightness (nx, ny, feld)
C2110 Implicit None
02120 Integer nx, ny
02130 Integer feld (0:nx-1, 0:ny-1)
02140 Integer i, j, n
02145 n = 0
02150 Do i = 0, nx - 1
02160 Do j = 0, ny - 1
02170 n = n + feld (i, j)
02180 EndDo
02185 EndDo
02188 GetTotalBrightness = n
02190 End

03000 Subroutine ReadInputFile (filename, numzeilen, zeilen)
03010 Character*256       filename
03015 Integer             numzeilen
03020 Character*256       zeilen (0:999)
03025 Character*256       s
03030 Integer             readok
03040 Open (unit=11, file=filename, err=03100, status='OLD',            &
     & action='READ', iostat=readok)
03042 numzeilen = 0
03045 readok = 0
03050 Do While (readok .eq. 0)
03060   Read (11,'(A)',end=03120) s
03062   If (readok .ne. 0)  exit
03066   zeilen (numzeilen) = s
03067   numzeilen = numzeilen + 1
03090 EndDo
03095 GoTo 03170
03100 Write (*,*) 'Fehler beim Lesen'
03120 Write (*,'(A,A,A,I4)') ' Datei ', filename,                       &
     &  ' gelesen, Anweisungen: ', numzeilen
03170 Close (unit=11)
03190 End

C     Parse a command line into action and positions,
C     Returns True on success, False on failure
03200 Logical Function TryParseCommand (command, act, x1, y1, x2, y2)
03210 Character*256         command
03215 Character*32          act
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
03280 If (command(cursor:cursor+7) .eq. 'through ') Then
03285   cursor = cursor + 8
03290 Else
03295   Write(*,*) 'Failed to read word ''through'' in command'
03300   Return
03305 EndIf
C     Parse second coordinate pair
03310 If (.Not. TryParseCorner (command, cursor, x2, y2)) Then
03315   Return
03320 EndIf
C     If thispoint is reached the command could be parsed correctly
03330 TryParseCommand = .True.
03390 End

03400 Logical Function TryParseAction (command, cursor, act)
03410 Character*256         command
03415 Character*32          act
03420 Integer               cursor
03430 TryParseAction = .False.
03440 if (command(1:9) .eq. 'turn off ') Then
03445   act = 'turn off'
03450   cursor = cursor + 9
03460 else If (command(1:8) .eq. 'turn on ') Then
03465   act = 'turn on'
03470   cursor = cursor + 8
03480 Else If (command(1:7) .eq. 'toggle ') Then
03485   act = 'toggle'
03490   cursor = cursor + 7
03500 Else
03510   return
03520 EndIf
03530 TryParseAction = .True.
03590 End

03600 Logical Function TryParseCorner (command, cursor, x, y)
03610 Character*256         command
03615 Integer               cursor, x, y
03620 Integer               pos
03630 pos = cursor + Index (command (cursor:len(command)), ',') - 1
03635 Read (command (cursor:pos-1),*) x
03640 cursor = pos + 1
03650 pos = cursor + Index (command (cursor:len(command)), ' ') - 1
03655 Read (command (cursor:pos-1),*) y
03660 cursor = pos + 1
03670 If (.True.) Then
03675   TryParseCorner = .True.
03680 Else
03685   TryParseCorner = .False.
03690 EndIf
03700 End

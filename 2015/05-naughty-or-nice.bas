1000 REM Advent of Code 2015, Day 5, Doesn't He Have Intern-Elves For This?
1005 REM https://adventofcode.com/2015/day/5
1010 REM
1015 REM GWBasic 2.02 erwartet sowohl diesen Quelltext als auch
1020 REM die Eingabedatei mit Zeilenenden als CR+LF
1025 REM
1050 GOSUB 1901   ' Kurztests
1100 PRINT "--- Beispiel ---"
1110 DIM WOERTER$(1024)
1115 WOERTER$(1) = "aei"
1116 WOERTER$(2) = "xazegov"
1117 WOERTER$(3) = "aeiouaeiouaeiou"
1118 WOERTER$(4) = "abcdde"
1119 WOERTER$(5) = "aabbccdd"
1120 WOERTER$(6) = "ugknbfddgicrmopn"
1121 WOERTER$(7) = "aaa"
1122 WOERTER$(8) = "jchzalrnumimnmhp"
1124 WOERTER$(9) = "haegwjzuvuyypxyu"
1125 WOERTER$(10) = "dvszwmarrgswjxmb"
1130 ANZAHLWOERTER% = 10
1140 PRINT ANZAHLWOERTER%; "W”rter -> Nice? (Vokale, Doppelte, Verbotene)"
1150 FOR W = 1 TO ANZAHLWOERTER%
1155 WORT$ = WOERTER$(W)
1160 GOSUB 2500
1165 IF NICE% THEN NICE$ = "Nice" ELSE NICE$ = "Naughty"
1170 PRINT W, WORT$; " -> "; NICE$, ANZAHLVOKALE%; ANZAHLDOPPELBUCHSTABEN%; ANZAHLVERBOTENEKOMBIS%
1180 NEXT
1190 PRINT
1195 GOSUB 1300   ' Beispiele fr Teil 2
1200 REM
1201 PRINT "--- Aufgabe 1: How many strings are nice? ---"
1210 DATEINAME$ = "05-in.txt" : GOSUB 3000
1215 PRINT "W”rter:"; ANZAHLWOERTER%
1220 NUMNICE% = 0 : NUMNAUGHTY% = 0
1230 PRINT ANZAHLWOERTER%; "W”rter -> Nice? (Vokale, Doppelte, Verbotene)"
1240 FOR W = 1 TO ANZAHLWOERTER%
1245 WORT$ = WOERTER$(W)
1250 GOSUB 2500
1255 IF NICE% THEN NICE$ = "Nice" ELSE NICE$ = "Naughty"
1260 IF (NICE%) THEN NUMNICE% = NUMNICE% + 1 ELSE NUMNAUGHTY% = NUMNAUGHTY% + 1
1265 PRINT W, WORT$; " -> "; NICE$, ANZAHLVOKALE%; ANZAHLDOPPELBUCHSTABEN%; ANZAHLVERBOTENEKOMBIS%
1270 NEXT
1280 PRINT "*** Ergebnis:  Nice ="; NUMNICE%; ",  Naughty ="; NUMNAUGHTY%
1285 PRINT
1290 GOTO 1400   ' Aufgabe 2
1300 REM
1301 PRINT "--- Beispiele 2 ---"
1311 WOERTER$(1) = "qjhvhtzxzqqjkmpb"
1312 WOERTER$(2) = "xxyxx"
1313 WOERTER$(3) = "uurcxstgmygtbstg"
1314 WOERTER$(4) = "ieodomkazucvgmuy"
1320 ANZAHLWOERTER% = 4
1330 PRINT ANZAHLWOERTER%; "W”rter -> Nice? (Doppelpaare, Buchstaben-Zangen)"
1350 FOR W = 1 TO ANZAHLWOERTER%
1355 WORT$ = WOERTER$(W)
1360 GOSUB 2800
1365 IF NICE% THEN NICE$ = "Nice" ELSE NICE$ = "Naughty"
1370 PRINT W, WORT$; " -> "; NICE$, ANZAHLDOPPELPAARE%; ANZAHLZANGEN%
1380 NEXT
1390 PRINT
1395 RETURN
1400 REM
1401 PRINT "--- Aufgabe 2: New rules ---"
1405 REM Wortliste sollte von vorheriger Aufgabe noch vorhanden sein
1420 NUMNICE% = 0 : NUMNAUGHTY% = 0
1430 PRINT ANZAHLWOERTER%; "W”rter -> Nice? (Doppelpaare, Buchstaben-Zangen)"
1440 FOR W = 1 TO ANZAHLWOERTER%
1445 WORT$ = WOERTER$(W)
1450 GOSUB 2800
1455 IF NICE% THEN NICE$ = "Nice" ELSE NICE$ = "Naughty"
1460 IF (NICE%) THEN NUMNICE% = NUMNICE% + 1 ELSE NUMNAUGHTY% = NUMNAUGHTY% + 1
1465 PRINT W, WORT$; " -> "; NICE$, ANZAHLDOPPELPAARE%; ANZAHLZANGEN%
1470 NEXT
1480 PRINT "*** Ergebnis:  Nice ="; NUMNICE%; ",  Naughty ="; NUMNAUGHTY%
1490 GOTO 1990   ' Ende
1900 REM
1901 PRINT "--- Kurztests ---"
1910 WORT$ = "abcdefghhijjkl"
1920 GOSUB 2000   ' Vokale z„hlen
1925 PRINT WORT$; " -> Anzahl Vokale:            "; ANZAHLVOKALE%
1930 GOSUB 2100   ' Doppel-Buchstaben z„hlen
1935 PRINT WORT$; " -> Anzahl Doppel-Buchstaben: "; ANZAHLDOPPELBUCHSTABEN%
1940 GOSUB 2200   ' Verbotene Buchstaben-Kombinationen z„hlen
1945 PRINT WORT$; " -> Anzahl verbotene Buchstaben-Kombinationen: "; ANZAHLVERBOTENEKOMBIS%
1950 PRINT
1960 RETURN
1990 END
2000 REM
2001 REM Vokale z„hlen
2002 REM Input:  Wort$
2003 REM Output: AnzahlVokale%
2004 REM
2010 ANZAHLVOKALE% = 0
2015 FOR I = 1 TO LEN(WORT$)
2020 IF MID$ (WORT$, I, 1) = "a" OR MID$ (WORT$, I, 1) = "e" OR MID$ (WORT$, I, 1) = "i" OR MID$ (WORT$, I, 1) = "o" OR MID$ (WORT$, I, 1) = "u" THEN ANZAHLVOKALE% = ANZAHLVOKALE% + 1
2050 NEXT
2090 RETURN
2100 REM
2101 REM Doppelbuchstaben z„hlen
2102 REM Input:  Wort$
2103 REM Output: AnzahlDoppelBuchstaben%
2104 REM
2110 ANZAHLDOPPELBUCHSTABEN% = 0
2115 FOR I = 1 TO LEN(WORT$) - 1
2120 IF MID$ (WORT$, I, 1) = MID$ (WORT$, I + 1, 1) THEN ANZAHLDOPPELBUCHSTABEN% = ANZAHLDOPPELBUCHSTABEN% + 1
2150 NEXT I
2190 RETURN
2200 REM
2201 REM Verbotene Buchstaben-Kombinationen z„hlen
2202 REM Input:  Wort$
2203 REM Output: AnzahlVerboteneKombis%
2204 REM
2210 ANZAHLVERBOTENEKOMBIS% = 0
2215 FOR I = 1 TO LEN(WORT$) - 1
2220 IF MID$ (WORT$, I, 2) = "ab" OR MID$ (WORT$, I, 2) = "cd" OR MID$ (WORT$, I, 2) = "pq" OR MID$ (WORT$, I, 2) = "xy" THEN ANZAHLVERBOTENEKOMBIS% = ANZAHLVERBOTENEKOMBIS + 1
2250 NEXT
2290 RETURN
2500 REM
2501 REM Auf "Nice String" testen
2502 REM Input:  Wort$
2503 REM Output: Nice% (bool)
2504 REM
2510 GOSUB 2000 : GOSUB 2100 : GOSUB 2200
2515 NICE% = (ANZAHLVOKALE% > 2) AND (ANZAHLDOPPELBUCHSTABEN% > 0) AND (ANZAHLVERBOTENEKOMBIS% <= 0)
2550 RETURN
2600 REM
2601 REM Doppelpaare z„hlen (Mehrfach auftretende Kombination aus zwei Buchstaben)
2602 REM Input:  Wort$
2603 REM Output: AnzahlDoppelpaare%
2604 REM
2610 ANZAHLDOPPELPAARE% = 0
2615 FOR I = 1 TO LEN (WORT$) - 3   ' Letztes Paar sp„testens in den letzten 4 Zeichen
2620 FOR J = I + 2 TO LEN (WORT$) - 1
2630 IF MID$ (WORT$, I, 2) = MID$ (WORT$, J, 2) THEN ANZAHLDOPPELPAARE% = ANZAHLDOPPELPAARE% + 1
2640 NEXT J
2650 NEXT I
2690 RETURN
2700 REM
2701 REM Buchstabenpaare mit einem weiteren Buchstaben dazwischen z„hlen
2702 REM Input:  Wort$
2704 REM
2710 ANZAHLZANGEN% = 0
2715 FOR I = 1 TO LEN (WORT$) - 2
2730 IF MID$ (WORT$, I, 1) = MID$ (WORT$, I + 2, 1) THEN ANZAHLZANGEN% = ANZAHLZANGEN% + 1
2750 NEXT I
2790 RETURN
2800 REM
2801 REM Naughty or Nice nach zweiter Definition
2802 REM Input:  Wort$
2803 REM Output: Nice% (bool)
2804 REM
2810 GOSUB 2600 : GOSUB 2700
2815 NICE% = (ANZAHLDOPPELPAARE% > 0) AND (ANZAHLZANGEN% > 0)
2850 RETURN
3000 REM
3001 REM Eingabedatei lesen
3002 REM Input:  Dateiname$
3003 REM Output: Woerter$(1024), AnzahlWoerter%
3004 REM
3010 OPEN "I", #1, DATEINAME$
3015 ANZAHLWOERTER% = 0
3020 WHILE NOT EOF(1)
3025 LINE INPUT #1, S$
3030 ANZAHLWOERTER% = ANZAHLWOERTER% + 1
3035 WOERTER$(ANZAHLWOERTER%) = S$
3070 WEND
3085 CLOSE #1
3090 RETURN

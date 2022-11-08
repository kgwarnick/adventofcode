(* Advent of Code 2015, Day 3 Perfectly Spherical Houses in a Vacuum *)
(* https://adventofcode.com/2015/day/3 *)

Program SphericalHouses;
Type Position = Record
    x: LongInt;   (* Spalte *)
    y: LongInt;   (* Zeile  *)
    n: LongInt;   (* Wie oft vorbei gekommen *)
  End;
  TPositionArray = Array[0..4095] Of Position;
  PPositionArray = ^TPositionArray;


Procedure ZaehlePosition (x, y: LongInt; Var PosListe: PPositionArray;
  Var PosAnzahl: Integer);
Var i: Integer;
Begin
  (* WriteLn ('Suche Position: ', x, ', ', y); *)
  i := 0;
  While (i < PosAnzahl) Do Begin
    if (PosListe^[i].x = x) And (PosListe^[i].y = y) Then Break;
    i := i + 1;
  End;
  If (i >= PosAnzahl) Then Begin
    (* Neuen Eintrag aufnehmen *)
    (* WriteLn ('Neuer Eintrag ', i); *)
    PosAnzahl := i + 1;
    PosListe^[i].x := x;  PosListe^[i].y := y;  PosListe^[i].n := 1;
  End Else Begin
    PosListe^[i].n := PosListe^[i].n + 1;
    (* WriteLn ('Inkrement Array-Index ', i, ' zu neuem Wert ', PosListe^[i].n); *)
  End
End;


(* Einzelnes Kommando befolgen *)
Procedure KommandoAuswerten (kommando: Char; var x, y: LongInt;
  PosListe: PPositionArray; Var PosAnzahl: Integer);
Begin
  If (kommando = '^') Then y := y + 1;
  If (kommando = 'v') Then y := y - 1;
  If (kommando = '<') Then x := x - 1;
  If (kommando = '>') Then x := x + 1;
  If (kommando In [ '^', 'v', '<', '>']) Then
    ZaehlePosition (x, y, PosListe, PosAnzahl)
  Else WriteLn ('- Ignored character: ', Ord(kommando), ' -');
End;


(* Mehrere Kommandos befolgen *)
Procedure KommandosAuswerten (kommandos: String; var x, y: LongInt;
  PosListe: PPositionArray; Var PosAnzahl: Integer);
Var i: Integer;
Begin
  For i := 1 To Length (kommandos) Do Begin
    KommandoAuswerten (kommandos[i], x, y, PosListe, PosAnzahl);
  End;
End;


Procedure AnweisungenFolgen (Anweisungen: String);
Var BesuchtePositionen: Integer;
    SchonBesucht: PPositionArray;
    i: Integer;
    x: LongInt;
    y: LongInt;
Begin
  GetMem (SchonBesucht, 4096 * SizeOf(Position));
  BesuchtePositionen := 0;
  x := 0;   (* Start-Spalte *)
  y := 0;   (* Start-Zeile  *)
  ZaehlePosition (x, y, SchonBesucht, BesuchtePositionen);   (* Start-Position mitzählen *)
  KommandosAuswerten (Anweisungen, x, y, SchonBesucht, BesuchtePositionen);   (* Weitere Anweisungen ausführen *)
  WriteLn ('End-Position:  (', x, ', ', y, ');  Häuser erreicht:  ', BesuchtePositionen);
  For i := 0 To BesuchtePositionen - 1 Do
    WriteLn (i, '  (', SchonBesucht^[i].x, ', ', SchonBesucht^[i].y, ') -> ',
      SchonBesucht^[i].n, 'x');
  FreeMem (SchonBesucht, 4096 * SizeOf(Position));
End;


(* Anweisungen abwechselnd von zwei Akteuren ausführen,
   mit gemeinsamer Liste von besuchten Orten *)
Procedure AnweisungenFolgenZuZweit (Anweisungen: String);
Var AnzahlOrte: Integer;
    BesuchteOrte: PPositionArray;
    i: Integer;
    x1, x2: LongInt;
    y1, y2: LongInt;
Begin
  GetMem (BesuchteOrte, 4096 * SizeOf(Position));
  AnzahlOrte := 0;
  x1 := 0;  y1 := 0;   (* Start-Spalte und Zeile, 1. Akteur *)
  x2 := 0;  y2 := 0;   (* Start-Spalte und Zeile, 2. Akteur *)
  ZaehlePosition (x1, y1, BesuchteOrte, AnzahlOrte);   (* Start-Position mitzählen *)
  ZaehlePosition (x2, y2, BesuchteOrte, AnzahlOrte);   (* Start-Position mitzählen *)
  For i := 1 To Length (Anweisungen) Do Begin
    If (i Mod 2 = 1)
      Then KommandoAuswerten (Anweisungen[i], x1, y1, BesuchteOrte, AnzahlOrte)
      Else KommandoAuswerten (Anweisungen[i], x2, y2, BesuchteOrte, AnzahlOrte);
  End;
  WriteLn ('End-Position Akteur 1:  (', x1, ', ', y1, '),  ',
           'End-Position Akteur 2:  (', x2, ', ', y2, '),  ',
           'Häuser erreicht:  ', AnzahlOrte);
  For i := 0 To AnzahlOrte - 1 Do
    WriteLn (i, '  (', BesuchteOrte^[i].x, ', ', BesuchteOrte^[i].y, ') -> ',
      BesuchteOrte^[i].n, 'x');
  FreeMem (BesuchteOrte, 4096 * SizeOf(Position));
End;


Procedure DateiLesenUndFolgen (DateiName: String);
Var Datei: File Of Char;
    i: Integer;
    x: LongInt;
    y: LongInt;
    c: Char;
    BesuchtePositionen: Integer;
    SchonBesucht: PPositionArray;
Begin
  Assign (Datei, DateiName);
  FileMode := 0;
  Reset (Datei);
  x := 0;   (* Start-Spalte *)
  y := 0;   (* Start-Zeile  *)
  GetMem (SchonBesucht, 4096 * SizeOf(Position));
  BesuchtePositionen := 0;
  ZaehlePosition (x, y, SchonBesucht, BesuchtePositionen);   (* Start-Position mitzählen *)
  i := 0;
  While Not EoF (Datei) Do Begin
    (* Einzelne Zeichen lesen, zwar langsam, aber lange Zeilen sind kein Problem.
       Dateityp 'Text' könnte nur Zeilen bis 255 Zeichen lesen *)
    Read (Datei, c);
    (* WriteLn (i, '  ', c); *)
    KommandosAuswerten (c, x, y, SchonBesucht, BesuchtePositionen);
    i := i + 1;
  End;
  Close (Datei);
  WriteLn ('End-Position:  (', x, ', ', y, '),  Häuser erreicht: ', BesuchtePositionen);
  (*
  For i := 0 To BesuchtePositionen - 1 Do
    WriteLn (i, '  (', SchonBesucht^[i].x, ', ', SchonBesucht^[i].y, ') -> ',
    SchonBesucht^[i].n);
  *)
  FreeMem (SchonBesucht, 4096 * SizeOf(Position));
  (* Eleganter?
  New (SchonBesucht);
  Dispose (SchonBesucht);
  *)
End;


(* Anweisungen aus Datei abwechselnd von zwei Akteuren ausführen,
   mit gemeinsamer Liste von besuchten Orten *)
Procedure DateiLesenUndFolgenZuZweit (DateiName: String);
Var Datei: File Of Char;
    i: Integer;
    x1, x2: LongInt;
    y1, y2: LongInt;
    c: Char;
    AnzahlOrte: Integer;
    BesuchteOrte: PPositionArray;
Begin
  Assign (Datei, DateiName);
  FileMode := 0;
  Reset (Datei);
  GetMem (BesuchteOrte, 4096 * SizeOf(Position));
  AnzahlOrte := 0;
  x1 := 0;  y1 := 0;   (* Start-Spalte und Zeile, 1. Akteur *)
  x2 := 0;  y2 := 0;   (* Start-Spalte und Zeile, 2. Akteur *)
  ZaehlePosition (x1, y1, BesuchteOrte, AnzahlOrte);   (* Start-Position mitzählen *)
  ZaehlePosition (x2, y2, BesuchteOrte, AnzahlOrte);   (* Start-Position mitzählen *)
  i := 0;
  While Not EoF (Datei) Do Begin
    Read (Datei, c);
    If (i Mod 2 = 0)
      Then KommandoAuswerten (c, x1, y1, BesuchteOrte, AnzahlOrte)
      Else KommandoAuswerten (c, x2, y2, BesuchteOrte, AnzahlOrte);
    i := i + 1;
  End;
  Close (Datei);
  WriteLn ('End-Position Santa: (', x1, ', ', y1, '),  ',
    'End-Position Robo-Santa: (', x2, ', ', y2, ')');
  WriteLn ('Zusammen ', i, ' Schritte gemacht,  Häuser erreicht: ', AnzahlOrte);
  FreeMem (BesuchteOrte, 4096 * SizeOf(Position));
End;


Procedure Beispiele;
Begin
  WriteLn ('* Anweisungen:  >');  AnweisungenFolgen ('>');
  WriteLn ('* Anweisungen:  ^>v<');  AnweisungenFolgen ('^>v<');
  WriteLn ('* Anweisungen:  ^v^v^v^v^v');  AnweisungenFolgen ('^v^v^v^v^v');
End;

Procedure BeispieleZuZweit;
Begin
  WriteLn ('* Anweisungen:  ^v');  AnweisungenFolgenZuZweit ('^v');
  WriteLn ('* Anweisungen:  ^>v<');  AnweisungenFolgenZuZweit ('^>v<');
  WriteLn ('* Anweisungen:  ^v^v^v^v^v');  AnweisungenFolgenZuZweit ('^v^v^v^v^v');
End;

Begin
  WriteLn ('--- Beispiele ---');
  Beispiele;
  WriteLn;

  WriteLn ('--- Aufgabe 1: Wie viele Häuser wurden mindestens einmal erreicht ---');
  DateiLesenUndFolgen ('housesin.txt');
  WriteLn;

  WriteLn ('--- Beispiele Teil 2 ---');
  BeispieleZuZweit;
  WriteLn;

  WriteLn ('--- Aufgabe 2: Santa wird von Robo-Santa unterstützt ---');
  DateiLesenUndFolgenZuZweit ('housesin.txt');
End.

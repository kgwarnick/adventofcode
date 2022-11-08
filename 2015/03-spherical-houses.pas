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


Procedure KommandosAuswerten (kommandos: String; var x, y: LongInt;
  PosListe: PPositionArray; Var PosAnzahl: Integer);
Var i: Integer;
Begin
  (* WriteLn ('  - Anweisungen: ', Length (kommandos)); *)
  For i := 1 To Length (kommandos) Do Begin
    If (kommandos[i] = '^') Then y := y + 1;
    If (kommandos[i] = 'v') Then y := y - 1;
    If (kommandos[i] = '<') Then x := x - 1;
    If (kommandos[i] = '>') Then x := x + 1;
    If (kommandos[i] In [ '^', 'v', '<', '>']) Then ZaehlePosition (x, y, PosListe, PosAnzahl);
    (* If (kommandos[i] = chr(10)) Then Write ('Ignored: "', kommandos[i], '"'); *)
  End;
  (* ZaehlePosition (x, y, PosListe, PosAnzahl); *)
  (* WriteLn ('Schritte: ', PosAnzahl); *)
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


Procedure DateiLesenUndFolgen (DateiName: String);
Var Datei: File Of Char;
    i: Integer;
    x: LongInt;
    y: LongInt;
    c: Char;
    (* SchonGewesen: Array[0..255] Of Position; *)
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

Procedure Beispiele;
Begin
  WriteLn ('* Anweisungen:  >');  AnweisungenFolgen ('>');
  WriteLn ('* Anweisungen:  ^>v<');  AnweisungenFolgen ('^>v<');
  WriteLn ('* Anweisungen:  ^v^v^v^v^v');  AnweisungenFolgen ('^v^v^v^v^v');
End;


Begin
  WriteLn ('--- Beispiele ---');
  Beispiele;
  WriteLn;

  WriteLn ('--- Aufgabe 1: Wie viele Häuser wurden mindestens einmal erreicht ---');
  DateiLesenUndFolgen ('housesin.txt');
End.

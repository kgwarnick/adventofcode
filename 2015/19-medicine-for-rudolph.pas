(* Advent of Code 2015, Day 19 Medicine for Rudolph *)
(* https://adventofcode.com/2015/day/19 *)

Program MedicineForRudolph;

Uses Strings;

Const MaxZeilen: Integer = 4096;
      MaxRegeln: Integer = 4096;
      MaxMolekuele: Integer = 16;

Type ReplacementRule = Record
    startpt: String;
    replmt: String;
  End;
  TZeilen = Array[0..4095] Of PChar;
  PZeilen = ^TZeilen;
  TRegeln = Array[0..4095] Of ReplacementRule;
  PRegeln = ^TRegeln;
  TMolekuele = Array[0..15] Of PChar;
  PMolekuele = ^TMolekuele;


(** Leerzeichen am Anfang und Ende der Zeichenfolge entfernen *)
Function Trimm (s: String): String;
Var a, b: Integer;
Begin
  a := 1;
  b := Length (s);
  While (s[b] = ' ')  Do b := b - 1;
  While (s[a] = ' ')  Do a := a + 1;
  Trimm := Copy (s, a, b - a + 1);
End;


(** Datei lesen und als Array von Zeilen speichern *)
Procedure DateiLesen (DateiName: String;
  var AnzZeilen: Integer; var Zeilen: PZeilen);
Var Datei: File Of Char;
    s: PChar;
    c: Char;
    i: Integer;
Begin
  Assign (Datei, DateiName);
  FileMode := 0;
  Reset (Datei);
  AnzZeilen := 0;
  While Not EoF (Datei) And (AnzZeilen < MaxZeilen - 1) Do Begin
    s := stralloc (1024);
    i := 0;
    c := Chr (0);
    s^ := Chr (0);
    While (Not EoF (Datei)) And (i < 1023) And (c <> Chr (10)) Do Begin
      Read (Datei, c);
      if (c <> Chr (10)) And (c <> Chr (10)) Then Begin
        s[i] := c;
      End;
      i := i + 1;
    End;
    s[i] := Chr (0);
    Zeilen^[AnzZeilen] := s;
    AnzZeilen := AnzZeilen + 1;
  End;
  Close (Datei);
End;


(** Regeln und Moleküle aus Zeilen lesen *)
Procedure ZeilenVerstehen (AnzZeilen: Integer; Zeilen: PZeilen;
  var AnzRegeln: Integer; var Regeln: PRegeln;
  var AnzMolekuele: Integer; var Molekuele: PMolekuele);
Var i: Integer;
    p: Integer;
    s: PChar;
    rulefrom, ruleto: String;
Begin
  AnzRegeln := 0;
  AnzMolekuele := 0;
  For i := 0 To AnzZeilen - 1 Do Begin
    s := Zeilen^[i];
    p := Pos ('=>', s);
    If (Length (Trimm (s)) = 0) Then Begin
      WriteLn (i, ' Leerzeile');
    End
    Else If (p > 0) Then Begin
      rulefrom := Trimm (Copy (s, 1, p - 1));
      ruleto := Trimm (Copy (s, p + 2, Length (s) - p - 1));
      WriteLn (i, ' Regel ', AnzRegeln,
        ': (', rulefrom, ') --> (', ruleto, ')');
      If (AnzRegeln < MaxRegeln - 1) Then Begin
        Regeln^[AnzRegeln].startpt := rulefrom;
        Regeln^[AnzRegeln].replmt := ruleto;
        AnzRegeln := AnzRegeln + 1;
      End Else Begin
        WriteLn ('WARNUNG: Kein Platz mehr für weitere Regeln');
      End;
    End
    Else Begin
      If (AnzMolekuele < MaxMolekuele - 1) Then Begin
        Molekuele^[AnzMolekuele] := s;
        WriteLn (i, ' Molekül ', AnzMolekuele, ': ', s);
        AnzMolekuele := AnzMolekuele + 1;
      End Else Begin
        WriteLn (i, ' Molekül ignoriert, kein Platz mehr: ', s);
      End
    End;
  End;
End;


(** In s ab Stelle start die nächsten count Zeichen durch t ersetzen.
    Das Ergebnis ist ein mit stralloc neu angelegtes PChar-Objekt *)
Function Ersetzen (s: PChar; start: Integer; count: Integer; t: PChar): PChar;
Var r: PChar;
Begin
  r := stralloc (strlen (s) - count + strlen (t) + 1);
  strlcopy (r, s, start);
  strcat (r, t);
  strcat (r, s + start + count);
  Ersetzen := r;
End;


(** Zeichenfolge in Array aufnehmen, sofern noch nicht vorhanden.
    Rückgabewert gibt an, ob die Zeichenfolge neu war und aufgenomen wurde.
    Das PChar-Objekt wird beim Aufnehmen ins Array kopiert, ist also
    unabhängiig von der Variable `neu` *)
Function MerkenWennNeu (var anz: Integer; var ziele: Array Of PChar;
  neu: PChar): Boolean;
Var i: Integer;
Begin
  For i := 0 To anz - 1 Do Begin
    If (strcomp (ziele[i], neu) = 0) Then Begin
      MerkenWennNeu := False;
      Exit;
    End;
  End;
  ziele[anz] := strnew (neu);
  anz := anz + 1;
  MerkenWennNeu := True;
End;


(** Eine Regel auf ein Molekül anwenden an allen möglichen Stellen *)
Procedure Ersetzungen (var anz: Integer; var ziele: Array Of PChar;
  regel: ReplacementRule; molek: PChar);
Var c: PChar;
    r, targ: PChar;
    t: PChar;
    foundnew: Boolean;
Begin
  (* Mögliche Positionen suchen *)
  r := stralloc (4096);
  targ := stralloc (4096);
  strpcopy (r, regel.startpt);
  strpcopy (targ, regel.replmt);
  c := strpos (molek, r);
  While (c <> Nil) Do Begin
    t := Ersetzen (molek, c - molek, strlen (r), targ);
    foundnew := MerkenWennNeu (anz, ziele, t);
    (* WriteLn ('  [', foundnew, '] Replace at ', c - molek, ': ', c, ' -> ', t); *)
    strdispose (t);
    c := strpos (c + strlen (r), r);
  End;
  strdispose (r);
  strdispose (targ);
End;


(** Alle Ersetzungen für ein Molekül ausprobieren *)
Procedure KombinationenTesten (var anz: Integer; var ziele: Array Of PChar;
  nregeln: Integer; regeln: PRegeln; molekuel: PChar);
Var i: Integer;
Begin
  anz := 0;
  For i := 0 To nregeln - 1 Do Begin
    Ersetzungen (anz, ziele, regeln^[i], molekuel);
  End;
End;


Procedure ExampleTargets;
Var n, i, m, nregeln, nmolek: Integer;
    zl: PZeilen;
    regeln: PRegeln;
    molek: PMolekuele;
    nz: Integer;
    ziele: Array[0..9999] Of PChar;
Begin
  New (zl);   (* GetMem (zl, 4096 * SizeOf (PChar)); *)
  New (regeln);
  New (molek);
  n := 6;
  zl^[0] := 'H => HO';
  zl^[1] := 'H => OH';
  zl^[2] := 'O => HH';
  zl^[3] := 'HOH';
  zl^[4] := 'HOHOHO';
  zl^[5] := 'H2O';
  ZeilenVerstehen (n, zl, nregeln, regeln, nmolek, molek);
  WriteLn (nregeln, ' Regel(n), ', nmolek, ' Molekül(e)');
  For m := 0 To nmolek - 1 Do Begin
    KombinationenTesten (nz, ziele, nregeln, regeln, molek^[m]);
    WriteLn (molek^[m], ': ', nz, ' Ziel-Moleküle gefunden');
    For i := 0 To nz - 1 Do  WriteLn ('- ', ziele[i]);
  End;
  Dispose (zl);   (* FreeMem (zl, 4096 * SizeOf (PChar)); *)
  Dispose (regeln);
  Dispose (molek);
End;


Var n, i, nregeln, nmolek: Integer;
    zl: PZeilen;
    regeln: PRegeln;
    molek: PMolekuele;
    nz: Integer;
    ziele: Array[0..9999] Of PChar;

Begin
  WriteLn ('--- Beispiele ---');
  ExampleTargets;
  WriteLn ();

  WriteLn ('--- Aufgabe 1: Ein Schritt ---');
  New (zl);   (* GetMem (zl, 4096 * SizeOf (PChar)); *)
  New (regeln);
  New (molek);
  n := 0;
  nregeln := 0;
  nmolek := 0;
  DateiLesen ('19-medicine-for-rudolph-input.txt', n, zl);
  WriteLn ('Zeilen gelesen: ', n);
  ZeilenVerstehen (n, zl, nregeln, regeln, nmolek, molek);
  WriteLn (nregeln, ' Regel(n), ', nmolek, ' Molekül(e)');
  KombinationenTesten (nz, ziele, nregeln, regeln, molek^[0]);
  WriteLn (nz, ' Ziel-Moleküle gefunden');
  For i := 0 To nz - 1 Do Begin
    WriteLn ('- ', ziele[i]);
  End;

  For i := 0 To (n - 1) Do  strdispose (zl^[i]);
  Dispose (zl);   (* FreeMem (zl, 4096 * SizeOf (PChar)); *)
  Dispose (regeln);
  Dispose (molek);
End.

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
        i := i + 1;
      End;
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
    (* foundnew: Boolean; *)
Begin
  (* Mögliche Positionen suchen *)
  r := stralloc (4096);
  targ := stralloc (4096);
  strpcopy (r, regel.startpt);
  strpcopy (targ, regel.replmt);
  c := strpos (molek, r);
  While (c <> Nil) Do Begin
    t := Ersetzen (molek, c - molek, strlen (r), targ);
    (* foundnew := *) MerkenWennNeu (anz, ziele, t);
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
  (* anz := 0; *)
  For i := 0 To nregeln - 1 Do Begin
    Ersetzungen (anz, ziele, regeln^[i], molekuel);
  End;
End;


(** Satz an erreichbaren Molekülen bestimmen *)
Procedure ErreichbareProdukte (var anz: Integer; var ziele: Array Of PChar;
  nregeln: Integer; regeln: PRegeln;
  nmolek: Integer; molekuele: Array Of PChar);
Var i, m: Integer;
Begin
  anz := 0;
  (* Aus allen vorhandenen Molekülen die möglichen Nachfolger bestimmen *)
  For m := 0 To nmolek - 1 Do Begin
    (* Jedes Molekül mit jeder Umwandlungs-Regel behandeln *)
    For i := 0 To nregeln - 1 Do Begin
      Ersetzungen (anz, ziele, regeln^[i], molekuele[m]);
    End;
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
    nz := 0;
    KombinationenTesten (nz, ziele, nregeln, regeln, molek^[m]);
    WriteLn (molek^[m], ': ', nz, ' Ziel-Moleküle gefunden');
    For i := 0 To nz - 1 Do  WriteLn ('- ', ziele[i]);
  End;
  Dispose (zl);   (* FreeMem (zl, 4096 * SizeOf (PChar)); *)
  Dispose (regeln);
  Dispose (molek);
End;


(** Syntheseweg suchen und alle möglichen Produkte in jedem Schritt bestimmen.
    Suche in die Breite.  Nicht praktikabel für größere Moleküle. *)
Function FindShortestSynthesis (startmol: PChar; target: PChar;
  nregeln: Integer; regeln: PRegeln; maxsteps: Integer): Integer;
Var numsteps, numreag, numprod, m: Integer;
    reagents: Array[0..99999] Of PChar;
    products: Array[0..99999] Of PChar;
    zielerreicht: Boolean;
Begin
  numsteps := 0;
  zielerreicht := False;
  (* Start mit einem Ausgangsmolekül *)
  numreag := 1;
  reagents[0] := strnew (startmol);   (* Kopie, damit später dispose erlaubt *)
  While (Not zielerreicht) And (numsteps < maxsteps) Do Begin
    (* Neuen Satz an möglichen Molekülen ermitteln *)
    ErreichbareProdukte (numprod, products, nregeln, regeln,
      numreag, reagents);
    WriteLn ('- Schritt ', numsteps, ': ', numprod, ' Produkte');
    (* Alte Ausgangsmoleküle verwerfen *)
    For m := 0 To numreag - 1 Do  strdispose (reagents[m]);
    (* Den neuen Satz Moleküle als nächste Ausgangsmoleküle speichern *)
    numreag := numprod;
    reagents := products;
    (* Schritte zählen *)
    numsteps := numsteps + 1;
    (* Zielprodukt gefunden? *)
    For m := 0 To numreag - 1 Do
      If (strcomp (reagents[m], target) = 0) Then zielerreicht := True;
  End;
  (* Synthesereste entsorgen *)
  For m := 0 To numreag - 1 Do  strdispose (reagents[m]);
  (* Rückgabewert setzen *)
  If (zielerreicht) Then FindShortestSynthesis := numsteps
    Else FindShortestSynthesis := maxsteps + 1;
End;


(** Rekursive Suche nach Syntheseweg *)
Function SyntheseSucheRek (startmol: PChar; target: PChar;
  nregeln: Integer; regeln: PRegeln; maxsteps: Integer;
  depth: Integer; var maxdepth: Integer): Integer;
Var numsteps, beststeps, numreag, numprod, m, mtemp: Integer;
    reagents: Array[0..1] Of PChar;
    products: Array[0..499] Of PChar;
Begin
  If (depth > maxdepth)  Then maxdepth := depth;
  (* Start mit einem Ausgangsmolekül *)
  numreag := 1;
  reagents[0] := strnew (startmol);   (* Kopie, damit später dispose erlaubt *)
  (* Mögliche nächste Schritte testen *)
  ErreichbareProdukte (numprod, products, nregeln, regeln,
    numreag, reagents);
  strdispose (reagents[0]);   (* Ausgangsprodukt verwerfen *)
  beststeps := maxsteps + 1;
  For mtemp := 0 To numprod - 1 Do Begin
    m := mtemp;   (* Normale Reihenfolge der Regeln *)
    (* m := numprod - mtemp - 1; *)   (* Rückwärts *)
    (* m := Random (numprod); *)   (* Zufällig *)
    (* Ziel erreicht? *)
    If (strcomp (products[m], target) = 0) Then Begin
      (* WriteLn ('Gefunden bei Tiefe ', depth, ', maxdepth = ', maxdepth); *)
      beststeps := 1;   (* Dieser eine Schritt hat gereicht *)
      Break;
    End;
    (* Rekursiv *)
    numsteps := SyntheseSucheRek (products[m], target, nregeln, regeln,
      maxsteps - 1, depth + 1, maxdepth);
    If (numsteps > 0) Then Begin
      numsteps := 1 + numsteps;   (* Der eigene Testschritt und die Rekursion *)
      If (numsteps < beststeps) Then beststeps := numsteps;
    End;
  End;
  (* Produkte verwerfen *)
  For m := 0 To numprod - 1 Do strdispose (products[m]);
  (* Lösung erhalten? *)
  if (beststeps <= maxsteps)  Then SyntheseSucheRek := beststeps
  Else SyntheseSucheRek := 0;   (* Keine Lösung *)
End;


(** Syntheseweg suchen ausgehend vom Zielmolekül.
    Auch nicht praktikabel für größere Moleküle.
    Suche in die Tiefe durch Rekursion. *)
Function FindRetroSynthesis (startmol: PChar; target: PChar;
  nregeln: Integer; regeln: PRegeln; maxsteps: Integer): Integer;
Var i, mdepth: Integer;
    rrules: PRegeln;
Begin
  (* Regeln umkehren *)
  New (rrules);
  For i := 0 To nregeln - 1 Do Begin
    rrules^[i].startpt := regeln^[i].replmt;
    rrules^[i].replmt  := regeln^[i].startpt;
  End;
  (* und Syntheseweg von Ziel zu Start suchen *)
  mdepth := 0;
  FindRetroSynthesis :=
    SyntheseSucheRek (target, startmol, nregeln, rrules, maxsteps, 1, mdepth);
  Dispose (rrules);
End;


(** Zufallsfolge von Zahlen von 0 bis (n-1) erzeugen *)
Procedure Zufallsfolge (n: Integer; Var zahlen: Array Of Integer);
Var i, r, tmp: Integer;
Begin
  For i := 0 To n - 1 Do  zahlen[i] := i;
  For i := 0 To n - 2 Do Begin
    r := Random (n - i);
    If (r > 0) Then Begin
      tmp := zahlen[i];
      zahlen[i] := zahlen[i+r];
      zahlen[i+r] := tmp;
    End;
  End;
End;


(** Schritte zur Erzeugung des Zielmoleküls durch Ausprobieren bestimmen.
 *  Mehrfaches Ausprobieren mit Anwendung der Reaktionsregeln in zufälliger
 *  Reihenfolge.
 *  Systematische Suche mit FindShortestSynthesis würde zu lange dauern,
 *  zumindest für viele Inputs;  siehe auch Erklärungen bzw. Lösungen hier:
 *  - https://www.reddit.com/r/adventofcode/comments/3xflz8
 *  - https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/
 *  - https://www.reddit.com/r/adventofcode/comments/3xglof/comment/cy4glgc/
 *  - https://www.reddit.com/r/adventofcode/comments/3xflz8/comment/cy4puwb/
 *)
Function Ausprobieren (startmol: PChar; target: PChar;
  nregeln: Integer; regeln: PRegeln;
  maxsteps: Integer; maxtries: Integer): Integer;
Var numsteps, beststeps, numtries, r: Integer;
    reached: Boolean;
    lastmol, currmol, temp, targ, stpt, c: PChar;
    abfolge: Array[0..99] Of Integer;
Begin
  beststeps := maxsteps + 1;
  For numtries := 0 To maxtries - 1 Do Begin
    numsteps := 0;
    reached := False;
    (* Regeln in Gegenrichtung betrachten, d.h. vom Zielmolekül aus *)
    currmol := strnew (target);
    (* Noch kein letztes Molekül am Anfang der Schleife *)
    lastmol := strnew ('...');
    While ((Not reached) And (numsteps < maxsteps) And
        (strcomp (lastmol, currmol) <> 0)) Do Begin
      (* Das aktuelle Molekül speichern als letztes betrachtetes *)
      strdispose (lastmol);
      lastmol := strnew (currmol);
      (* Regeln in zufälliger Reihenfolge probieren *)
      Zufallsfolge (nregeln, abfolge);
      For r := 0 To nregeln - 1 Do Begin
        stpt := stralloc (32);
        targ := stralloc (32);
        strpcopy (targ, regeln^[abfolge[r]].replmt);
        strpcopy (stpt, regeln^[abfolge[r]].startpt);
        c := strpos (currmol, targ);
        If (c <> Nil) Then Begin
          temp := currmol;
          currmol := Ersetzen (currmol, c - currmol, strlen (targ), stpt);
          numsteps := numsteps + 1;
          strdispose (temp);
          (* WriteLn ('--- Neues Molekül: ', currmol); *)
          If (strcomp (currmol, startmol) = 0) Then Begin
            reached := True;
            (* WriteLn ('Lösung mit ', numsteps, ' Schritten'); *)
            Break;
          End;
        End;
        strdispose (stpt);
        strdispose (targ);
      End;   (* For (alle Regeln) *)
    End;   (* While (not reached) *)
    strdispose (lastmol);
    strdispose (currmol);
    If (reached) Then Begin
      WriteLn ('[Try ', numtries, '/', maxtries, '] Lösung mit ',
        numsteps, ' Schritten gefunden');
      If (numsteps < beststeps) Then beststeps := numsteps;
    End;
  End;
  WriteLn ();
  If (beststeps <= maxsteps) Then Ausprobieren := beststeps
    Else Ausprobieren := 0;
End;


Procedure ExampleSynthesis;
Var i, m, n, nregeln, nmolek: Integer;
    zl: PZeilen;
    regeln: PRegeln;
    molek: PMolekuele;
Begin
  New (zl);   (* GetMem (zl, 4096 * SizeOf (PChar)); *)
  New (regeln);
  New (molek);
  n := 8;
  zl^[0] := 'e => H';
  zl^[1] := 'e => O';
  zl^[2] := 'H => HO';
  zl^[3] := 'H => OH';
  zl^[4] := 'O => HH';
  zl^[5] := 'HOH';
  zl^[6] := 'HOHOHO';
  zl^[7] := 'H2O';
  ZeilenVerstehen (n, zl, nregeln, regeln, nmolek, molek);
  WriteLn (nregeln, ' Regel(n), ', nmolek, ' Molekül(e)');
  For m := 0 To nmolek - 1 Do Begin
    (* Vorwärts-Suche in die Breite *)
    i := FindShortestSynthesis ('e', molek^[m], nregeln, regeln, 10);
    if ((i <= 10) And (i > 0)) Then
      WriteLn ('e -?-> ', molek^[m], ' (Vorwärts): Weg mit ',
        i, ' Schritten gefunden')
    Else
      WriteLn ('e -?-> ', molek^[m],
        ' (Vorwärts): Kein Weg mit maximal 10 Schritten gefunden');
    (* Rückwärts-Suche in die Tiefe *)
    i := FindRetroSynthesis ('e', molek^[m], nregeln, regeln, 10);
    if ((i <= 10) And (i > 0)) Then
      WriteLn ('e -?-> ', molek^[m], ' (Rückwärts): Weg mit ',
        i, ' Schritten gefunden')
    Else
      WriteLn ('e -?-> ', molek^[m],
        ' (Rückwärts): Kein Weg mit maximal 10 Schritten gefunden');
  End;
End;


Var i, m, n, nregeln, nmolek: Integer;
    zl: PZeilen;
    regeln: PRegeln;
    molek: PMolekuele;
    nz: Integer;
    ziele: Array[0..9999] Of PChar;

Begin
  WriteLn ('--- Beispiel: Ein Schritt zur Kalibrierung ---');
  ExampleTargets;
  WriteLn ();

  WriteLn ('--- Beispiel: Syntheseweg suchen ---');
  ExampleSynthesis;
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
  WriteLn ();

  WriteLn ('--- Aufgabe 2: Syntheseweg ---');
  Randomize;
  For m := 0 To nmolek - 1 Do Begin
    i := Ausprobieren ('e', molek^[m], nregeln, regeln, 250, 10000);
    if (i > 0) Then
      WriteLn ('e -?-> ', molek^[m], ': Weg mit ', i, ' Schritten gefunden')
    Else
      WriteLn ('e -?-> ', molek^[m],
        ': Kein Weg mit maximal 10 Schritten gefunden');
  End;

  For i := 0 To (n - 1) Do  strdispose (zl^[i]);
  Dispose (zl);   (* FreeMem (zl, 4096 * SizeOf (PChar)); *)
  Dispose (regeln);
  Dispose (molek);
End.

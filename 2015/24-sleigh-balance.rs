// Advent of Code 2015, Day 25: It Hangs in the Balance
// https://adventofcode.com/2015/day/24


// Hinweis:
//   Die Funktionen sind mit dem Ziel geschrieben, auch für mehrfach
//   auftretende Zahlen ("Gewichte") in der Eingabe zu funktionieren.
//   Da das in der tatsächlichen Eingabe nicht der Fall ist, ist diese
//   Eigenschaft ungetestet.
//
//   Außerdem sollte dadurch das wiederholte Mapping von Indizes zu Gewichten
//   unnötig sein und es könnte direkt mit den Gewichten gearbeitet werden.
//   - Zum Beispiel mit std::collections::HashSet und der zugehörigen Methode
//     difference statt der eigenen Funktion vek_ohne_index?


use std::fs;


/// Elemente aus Vektor per Index herausnehmen
//
fn vek_ohne_index<T: Copy> (vorher: &Vec<T>, raus: &Vec<usize>) -> Vec<T> {
    let mut i = 0;
    let mut neu = Vec::new();
    for idx in 0..vorher.len() {
        if i >= raus.len() || raus[i] != idx { neu.push(vorher[idx]); }
        else if i < raus.len() { i += 1; }
    }
    neu
}


/// Nächste Kombination mit passender Länge erzeugen
///
/// Es wird versucht, vom Anfang des Zahlenvorrats so lange Zahlen
/// hinzuzunehmen, bis die Summe erreicht ist.
/// Wenn die aktuelle Kombination noch nicht die vorgesehene Summe erreicht,
/// wird versucht, ein weiteres Element hinzuzufügen.
/// Wenn kein Element mehr passt, wird das aktuell letzte ersetzt
/// (wieder weggenommen und andere noch nicht getestete Elemente Richtung
/// Ende des Zahlenvorrats ausprobiert).
///
/// Die aktuelle Kombination und der Rückgabewert sind Indizes auf den
/// Zahlenvorrat und nicht direkt die Werte!
///
/// Der übergebene Zahlenvorrat muss/sollte(?) absteigend sortiert sein,
/// damit das Verfahren funktioniert.
///
/// # Beispiel
///
/// Mit dem Zahlenvorrat [4, 3, 2, 1], der aktuellen Kombination [0, 1] und
/// der beabsichtigten Summe 6:
/// * Die Index-Kombination [0, 2] entspricht den Zahlen [4, 2]
/// * Die 2 wird weggenommen und stattdessen Index 3 getestet,
///   also die Kombination [4, 1].
/// * Es kann keine weitere Zahl hinzugefügt werden, weil Index 3 = Zahl 1
///   die letzte zur Verfügung stehende Zahl war.
/// * Also wird Index 0 = Zahl 4 entfernt und als nächstes Kombinationen mit
///   Index 1 = Zahl 3 getestet
/// * Erste Index-Kombination [1, 2] entspricht [3, 2].
///   Die Summe ist noch zu klein.
/// * Es wird versucht, eine weitere Zahl hinzuzufügen.  Einzig verfügbare
///   ist an Index 3 die Zahl 1.  Sie passt und ergibt die Summe 6.
/// * Damit ist der Rückgabewert die Index-Kombination [1, 2, 3],
///   die der Zahlen-Kombination [3, 2, 1] entspricht.
///
/// # Parameter
///
/// reservoir: Vorrat an Zahlen zur Auswahl
/// aktuell:   Die aktuelle Kombination als Indizes auf den Vorrat
/// zielsumme: Die mit der Auswahl zu erreichende Summe
///
/// Rückgabewert:  Die nächste Kombination mit der entsprechenden Summe oder
///                der leere Vektor [], wenn keine solche Kombination mehr
///                gefunden werden konnte.
//
fn nächste_auswahl (reservoir: &Vec<u32>, aktuell: &Vec<usize>, zielsumme: u32) -> Vec<usize> {
    let mut testvec = aktuell.to_vec();
    let mut testsumme = aktuell.iter()
        .map(|a| reservoir[*a])
        .reduce(|acc, el| acc + el)
        .unwrap_or(0);
    // Zuerst ein Element wegnehmen
    // TODO Weitere Elemente wegnehmen, wenn die Summe zu groß ist?
    //      Dann könnte auch von ungültigen Kombinationen gestartet werden?
    let mut letzterindex;
    if testvec.len() > 0 {
        letzterindex = testvec.pop().unwrap();
        testsumme -= reservoir[letzterindex];
        letzterindex += 1;
        if (letzterindex < reservoir.len() - 1) &&
            (reservoir[letzterindex] == reservoir[letzterindex+1]) {
            letzterindex += 1;
        }
    }
    else {
        letzterindex = 0;
    }
    // Eine neue Kombination finden
    loop {
        // Index außerhalb, nächste Stelle verändern
        if letzterindex >= reservoir.len() {
            // Kein Element mehr zum Wegnehmen, Ende
            if testvec.is_empty()  { break; }
            // Sonst weiteres Element bewegen
            letzterindex = testvec.pop().unwrap();
            testsumme -= reservoir[letzterindex];
            letzterindex += 1;
            continue;
        }
        // println!("- Testvektor: {testvec:?} : {letzterindex} -> {:?} (Summe: {testsumme})",
        //     testvec.iter().map(|idx| reservoir[*idx]).collect::<Vec<u32>>());
        // Neues Element passt und die Summe ist immer noch kleiner
        if testsumme + reservoir[letzterindex] < zielsumme {
            testvec.push (letzterindex);
            testsumme += reservoir[letzterindex];
            letzterindex += 1;
            continue;
        }
        // Neues Element passt genau, um die Zielsumme zu erreichen
        else if testsumme + reservoir[letzterindex] == zielsumme {
            testvec.push (letzterindex);
            break;
        }
        // Element hat nicht gepasst, weiter probieren
        letzterindex += 1;
    }
    // Neue Kombination in Form von Indizes zurückgeben
    testvec
}


/// Feststellen, ob auch die nach Wahl einer Kombination verbleibende
/// Zahlenmenge noch in Blöcke mit der beabsichtigten Summe aufteilbar ist
//
fn rest_aufteilbar (zusammen: &Vec<u32>, auswahl: &Vec<usize>, zielsumme: u32) -> bool {
    let verbleibend = vek_ohne_index (&zusammen, &auswahl);
    let neuewahl = nächste_auswahl (&verbleibend, &Vec::new(), zielsumme);
    // println!("  - Rest: {verbleibend:?} -> Mögliche Aufteilung: {neuewahl:?} -> {:?}",
    //     neuewahl.iter().map(|i| verbleibend[*i]).collect::<Vec<u32>>());
    // Rekursiv überprüfen, ob der neue Rest immer noch aufteilbar ist
    if !neuewahl.is_empty() {
        let restsumme : u32 = vek_ohne_index (&verbleibend, &neuewahl)
            .iter() .sum();
        if restsumme > zielsumme {
            // println!("- Rekursiver Test auf Aufteilung für Summe: {}", restsumme);
            rest_aufteilbar (&verbleibend, &neuewahl, zielsumme)
        }
        else { true }
    }
    else { false }
}


/// Beste Kombination (kürzeste mit kleinstem Produkt der Zahlen) finden
fn kombinationen_testen (vorrat: &Vec<u32>, zielsumme: u32) -> Vec<u32> {
    // Beste Kombination speichern
    // - Startwert = "Schlechteste Kombination" = alle Zahlen verwenden
    let mut beste_anzahl = vorrat.len();
    let mut beste_auswahl = Vec::new();
    let mut kleinstes_produkt = u64::MAX;
    let mut sortiert = vorrat.to_vec();
    sortiert.sort_unstable_by(|a, b| b.cmp(a));
    let mut testauswahl: Vec<usize> = Vec::with_capacity (vorrat.len());
    testauswahl = nächste_auswahl (&sortiert, &testauswahl, zielsumme);
    while testauswahl.len() > 0 {
        // println!("- Test-Auswahl: {testauswahl:?} -> {:?}",
        //     testauswahl.iter() .map(|idx| sortiert[*idx]) .collect::<Vec<u32>>());
        // Kürzere Kombination gefunden?
        if testauswahl.len() < beste_anzahl {
            // println!("- Kürzere Auswahl: {testauswahl:?}");
            // Nachprüfen, dass der Rest gleichmäßig aufteilbar ist
            if rest_aufteilbar (&sortiert, &testauswahl, zielsumme) {
                beste_auswahl = testauswahl.to_vec();
                beste_anzahl = testauswahl.len();
                kleinstes_produkt = testauswahl.iter()
                    .map(|idx| u64::from (sortiert[*idx]))
                    .product();
            }
            else {
                println!("  - Rest kann nicht aufgeteilt werden");
            }
        }
        // Gleich lange Kombination: "Quantum Entanglement" vergleichen:
        // Besseres "Quantum Entanglement" = kleineres Produkt der Zahlen
        else if testauswahl.len() == beste_anzahl {
            let testprodukt = testauswahl.iter()
                .map(|idx| u64::from (sortiert[*idx]))
                .product();
            if testprodukt < kleinstes_produkt {
                // println!("- Besseres Quantum Entanglement: {testauswahl:?}");
                // Nachprüfen, dass der Rest gleichmäßig aufteilbar ist
                if rest_aufteilbar (&sortiert, &testauswahl, zielsumme) {
                    beste_auswahl = testauswahl.to_vec();
                    kleinstes_produkt = testprodukt;
                }
                else {
                    println!("  - Rest kann nicht aufgeteilt werden");
                }
            }
        }
        // Nächste Test-Kombination bestimmen
        testauswahl = nächste_auswahl (&sortiert, &testauswahl, zielsumme);
    }
    // Rückgabe als Liste der Werte, nicht der Indizes!
    beste_auswahl.iter() .map(|idx| sortiert[*idx]) .collect()
}


/// Testfall für vek_ohne_index ausführen
//
fn test_vekohneidx (ganz: &Vec<u32>, ohneindizes: &Vec<usize>, erwartet: &Vec<u32>) {
    let bleibt = vek_ohne_index (ganz, ohneindizes);
    println!("Vektor-Ohne-Index:  {ganz:?} \\ index {ohneindizes:?} = {bleibt:?},  erwartet: {erwartet:?}");
    assert_eq!(erwartet, &bleibt);
}


/// Testfall für nächste_auswahl ausführen
//
fn test_nachfolger (vorrat: &Vec<u32>, aktuell: &Vec<usize>, zielsumme: u32,
        nächster: &Vec<usize>) {
    let tatsächlich = nächste_auswahl(vorrat, aktuell, zielsumme);
    println!("Nachfolger:  Startmenge {vorrat:?} :  {aktuell:?} -> {nächster:?} ? -- {tatsächlich:?}");
    assert_eq!(nächster, &tatsächlich);
}


/// Kurze Testfälle für einige Funktionen
//
fn tests () {
    test_vekohneidx(&vec![8, 7, 6, 5], &vec![1, 3], &vec![8, 6]);
    test_vekohneidx(&vec![8, 7, 6, 5], &vec![0], &vec![7, 6, 5]);
    test_nachfolger(&vec![4, 3, 2, 1], &vec![0, 1], 6, &vec![0, 2]);
    test_nachfolger(&vec![4, 3, 2, 1], &vec![0, 2], 6, &vec![1, 2, 3]);
    test_nachfolger(&vec![4, 3, 2, 1], &vec![1, 2, 3], 6, &vec![]);
}


/// Ideale Konfiguration bestimmen
//
fn ideale_konfiguration (gewichtsliste: &Vec<u32>, abteile: u32) -> Vec<u32> {
    let mut gewichte = gewichtsliste.to_vec();
    gewichte.sort_by(|a, b| b.cmp(a));
    let gewichtsumme: u32 = gewichtsliste.iter().sum();
    println! ("Summe der Gewichte: {gewichtsumme}");
    kombinationen_testen (&gewichte, gewichtsumme/abteile)
}


fn main () {
    println! ("--- Kleine Tests ---");
    tests();
    println! ();

    println! ("--- Beispiel ---");
    let mut gewichte = vec![1, 2, 3, 4, 5,  7, 8, 9, 10, 11];
    gewichte.sort_by (|a, b| b.cmp(a));
    let anordnung = ideale_konfiguration (&gewichte, 3);
    let quantentangle : u32 = anordnung.iter().product();
    println!("Beste Anordnung für 3 Abteile: {anordnung:?} -> Quantum Entanglement: {quantentangle}");
    let anordnung = ideale_konfiguration (&gewichte, 4);
    let quantentangle : u32 = anordnung.iter().product();
    println!("Beste Anordnung für 4 Abteile: {anordnung:?} -> Quantum Entanglement: {quantentangle}");
    println! ();

    println!("--- Aufgabe 1: Ideale Anordnung für 3 Abteile ---");
    let dateiinhalt = fs::read_to_string ("24-sleigh-balance-input.txt")
        .expect ("Unable to read file");
    let gewichte : Vec<u32> = dateiinhalt .trim() .split('\n')
        .map(|x| x.trim() .parse() .expect("Fehler bei Zahl-Umwandlung"))
        .collect();
    println!("Eingelesene Gewichte:  {gewichte:?}");
    let anordnung = ideale_konfiguration (&gewichte, 3);
    let quantentangle = anordnung.iter().map(|n| u64::from(*n)).product::<u64>();
    println!("Beste Anordnung für 3 Abteile: {anordnung:?} -> Quantum Entanglement: {quantentangle}");
    println! ();

    println!("--- Aufgabe 2: Ideale Anordnung für 4 Abteile ---");
    let anordnung = ideale_konfiguration (&gewichte, 4);
    let quantentangle = anordnung.iter().map(|n| u64::from(*n)).product::<u64>();
    println!("Beste Anordnung für 4 Abteile: {anordnung:?} -> Quantum Entanglement: {quantentangle}");
}

// Advent of Code 2015 Day 12 Accounting Elves
// https://adventofcode.com/2015/day/12

const examples = [ "[1,2,3]", "{\"a\":2,\"b\":4}", "[[[3]]]", "{\"a\":{\"b\":4},\"c\":-1}",
  "{\"a\":[-1,1]}", "[-1,{\"a\":1}]", "[]", "{}",
  "[1,{\"c\":\"red\",\"b\":2},3]", "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}", "[1,\"red\",5]" ]

var inputtext

function JsonQuersumme (obj, includered = true) {
  let n = 0
  if (obj instanceof Array) {
    // Alle Elemente gehen in die Quersumme ein
    for (var i = 0; i < obj.length; i++) {
      n += JsonQuersumme (obj[i], includered);
    }
  }
  else if (obj instanceof Object) {
    // Alle Propertys gehen in die Quersumme ein
    for (var p in obj) {
      // Wenn der Wert "red" ist, das ganze Objekt ignorieren, wenn gefordert
      if (!includered && (obj[p] == "red")) {
        // console.log ("Ignoriere " + p + " -> " + obj[p]);
        return 0;
      }
      // Schlüssel und Wert gehen in die Quersumme ein
      n += JsonQuersumme (p, includered) + JsonQuersumme (obj[p], includered);
    }
  }
  else if (typeof obj == "string") {
    // Zeichenfolgen haben Zahlenwert 0
    return 0;
  }
  else if (typeof obj == "number") {
    // Zahlen gehen in die Quersumme ein
    n = n + obj;
  }
  else {
    document.getElementById("Ausgabe").innerHTML += "Unexpected type in JSON structure: " + typeof obj + ", " + obj + "<br/>";
  }
  return n;
}


function QuersummeBerechnen () {
  let ausg = document.getElementById("Ausgabe")

  ausg.innerHTML += "--- Beispiele ---<br/>";
  for (var ex in examples) {
    j = JSON.parse (examples[ex]);
    ausg.innerHTML += ex + "&ensp; " + examples[ex] + " &emsp; " + JsonQuersumme (j) + "<br/>";
  }
  ausg.innerHTML += "<br/>"
  ausg.innerHTML += "";
  ausg.innerHTML += "Ignoriere alle Objekte mit Wert \"red\" in beliebiger Property<br/>";
  for (var ex in examples) {
    j = JSON.parse (examples[ex]);
    ausg.innerHTML += ex + "&ensp; " + examples[ex] + " &emsp; " + JsonQuersumme (j, false) + "<br/>";
  }
  ausg.innerHTML += "<br/>"

  let inputfile = document.getElementById("inputfilename").files[0];
  if (inputfile == null) {
    ausg.innerHTML += "keine Datei geladen";
    return;
  }
  let reader = new FileReader();
  reader.readAsText (inputfile)
  reader.onload = function() {
    console.log("File read successfully");
    inputtext = reader.result;
    document.getElementById("Eingabe").innerHTML = reader.result;
    ausg.innerHTML += "--- Aufgabe 1 ---<br/>";
    ausg.innerHTML += "Zeichen gelesen: " + inputtext.length + "<br/>";
    j = JSON.parse (reader.result);
    ausg.innerHTML += "*** Summe aller Zahlen:  " + JsonQuersumme (j) + " ***<br/>";
    ausg.innerHTML += "<br/>"

    ausg.innerHTML += "--- Aufgabe 2: Ignoeriere alle Objekte mit Werten, die \"red\" sind ---<br/>";
    ausg.innerHTML += "*** Summe aller Zahlen:  " + JsonQuersumme (j, false) + " ***<br/>";
  }
  reader.loadstart = function() {
    console.log("load started")
  }
  reader.progress = function() {
    console.log("progress")
  }
  reader.onerror = function() {
    console.log(reader.error)
  }
}

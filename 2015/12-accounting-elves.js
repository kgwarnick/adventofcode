// Advent of Code 2015 Day 12 Accounting Elves
// https://adventofcode.com/2015/day/12

const examples = [ "[1,2,3]", "{\"a\":2,\"b\":4}", "[[[3]]]", "{\"a\":{\"b\":4},\"c\":-1}",
  "{\"a\":[-1,1]}", "[-1,{\"a\":1}]", "[]", "{}" ]

var inputtext

function JsonQuersumme (obj) {
  let n = 0
  if (obj instanceof Array) {
    for (var i = 0; i < obj.length; i++) {
      // Alle Elemente gehen in die Quersumme ein
      n += JsonQuersumme (obj[i]);
    }
  }
  else if (obj instanceof Object) {
    for (var p in obj) {
      // Schlüssel und Wert gehen in die Quersumme ein
      n += JsonQuersumme (p) + JsonQuersumme (obj[p]);
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
    // document.getElementById("Ausgabe").innerHTML += "Summer aller Zahlen: "
    j = JSON.parse (examples[ex]);
    ausg.innerHTML += ex + "&ensp; " + examples[ex] + " &emsp; " + JsonQuersumme (j) + "<br/>";
  }
  ausg.innerHTML += "<br/>"

  ausg.innerHTML += "--- Aufgabe 1 ---<br/>";
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
    ausg.innerHTML += "Read " + inputtext.length + " characters<br/>";
    j = JSON.parse (reader.result);
    ausg.innerHTML += "*** Summer aller Zahlen:  " + JsonQuersumme (j) + " ***<br/>";
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

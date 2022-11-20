// Advent of Code 2015, Day 8 Matchsticks
// https://adventofcode.com/2015/day/8

namespace karlgeorg.adventskal {

public class MatchsticksEscaping {

    public static string[] ProvidedExamples = {
      "\"\"", "\"abc\"", "\"aaa\\\"aaa\"", "\"\\x27\""
    };
    public static string[] MoreExamples = {
      "\"\x41\x42 = \\\"\\x41\\x42\\\"\""   // AB = "\x41\x42"
    };


    public static string Escape (string valueString) {
        System.Text.StringBuilder target =
            new System.Text.StringBuilder (valueString.Length * 4 / 3);
        // Add opening quote
        target.Append ('\"');
        // Copy characters to the target string, escaping where necessary
        for (int i = 0; i < valueString.Length; i++) {
            if (valueString[i] == '\\') {
              target.Append ("\\\\");
            }
            else if (valueString[i] == '\"') {
              target.Append ("\\\"");
            }
            else {
              target.Append (valueString[i]);
            }
        }
        // Add closing quote and return the built string
        target.Append ('\"');
        return target.ToString ();
    }

    public static string Unescape (string codeString) {
        if (codeString.Length < 2 || codeString[0] != '\"' ||
            codeString[codeString.Length-1] != '\"') {
            throw new System.Exception ($"Invalid quote: {codeString}");
        }
        // Ignore surrounding quotes
        string temp = codeString.Substring (1, codeString.Length - 2);
        System.Text.StringBuilder target =
            new System.Text.StringBuilder(temp.Length);
        int i = 0;
        while (i < temp.Length) {
          // System.Console.WriteLine ($"index: {i}, character {temp[i]}");
          if (temp[i] != '\\') {
            target.Append (temp[i]);  i++;  continue;
          }
          if (i + 1 < temp.Length && temp[i+1] == '\\') {
            // Escape backslash
            target.Append ('\\');  i += 2;  continue;
          }
          if (i + 1 < temp.Length && temp[i+1] == '\"') {
            // Escape double quote character
            target.Append ('\"');  i += 2;  continue;
          }
          if (temp.Length > i + 3 && temp[i+1] == 'x') {
            // Replace ASCII characters given numerically as \xNN
            byte charCode = System.Convert.ToByte (temp.Substring (i + 2, 2), 16);
            target.Append ((char)charCode);  i += 4;  continue;
          }
          System.Console.WriteLine (
              $"Unsupported escape sequence: {temp[i+1]}");
          i++;
        }
        return target.ToString();
    }

    public static int RunEscapeTest (string[] strlist, int logLevel = 1) {
        int valueLen = 0, codeLen = 0;
        foreach (string s in strlist) {
            string e = Escape (s);
            valueLen += s.Length;  codeLen += e.Length;
            if (logLevel >= 2)  System.Console.WriteLine (
                $"escape: {s} ({s.Length}) -> {e} ({e.Length})");
        }
        if (logLevel >= 1)  System.Console.WriteLine (
            $"code length {codeLen} -> value length {valueLen}");
        return codeLen - valueLen;
    }

    public static int RunUnescapeTest (string[] strlist, int logLevel = 1) {
        int valueLen = 0, codeLen = 0;
        foreach (string s in strlist) {
            string u = Unescape (s);
            codeLen += s.Length;  valueLen += u.Length;
            if (logLevel >= 2)  System.Console.WriteLine (
                $"unescape: {s} ({s.Length}) -> {u} ({u.Length})");
        }
        if (logLevel >= 1)  System.Console.WriteLine (
            $"code length {codeLen} -> value length {valueLen}");
        return codeLen - valueLen;
    }

    public static void Main () {

        System.Console.WriteLine ("--- Example Unescaping ---");
        int result = RunUnescapeTest (ProvidedExamples, 2);
        System.Console.WriteLine (
            $"-> Difference in value and code length: {result}");
        System.Console.WriteLine ();

        System.Console.WriteLine ("--- Weitere Beispiele ---");
        result = RunUnescapeTest (MoreExamples, 2);
        System.Console.WriteLine (
            $"-> Difference in value and code length: {result}");
        System.Console.WriteLine ();

        System.Console.WriteLine ("--- Aufgabe 1: Escaping overhead ---");
        string[] inputLines = System.IO.File.ReadAllLines (
            "08-matchsticks-input.txt");
        result = RunUnescapeTest (inputLines, 1);
        System.Console.WriteLine (
            $"-> Difference in value and code length: {result}");
        System.Console.WriteLine ();

        System.Console.WriteLine ("--- Example Escaping ---");
        result = RunEscapeTest (ProvidedExamples, 2);
        System.Console.WriteLine (
            $"-> Difference in value and code length: {result}");
        System.Console.WriteLine ();

        System.Console.WriteLine ("--- Weitere Beispiele ---");
        result = RunEscapeTest (MoreExamples, 2);
        System.Console.WriteLine (
            $"-> Difference in value and code length: {result}");
        System.Console.WriteLine ();

        System.Console.WriteLine ("--- Aufgabe 2: Escaping overhead ---");
        result = RunEscapeTest (inputLines);
        System.Console.WriteLine (
            $"-> Difference in value and code length: {result}");
        System.Console.WriteLine ();
    }
}

}

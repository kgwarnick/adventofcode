// Advent of Code 2021, Day 8 Seven Segment Search
// https://adventofcode.com/2021/day/8


namespace karlgeorg.adventskal;



public class SevenSegments {

    public static readonly string ExampleInput =
      "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n" +
      "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n" +
      "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n" +
      "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n" +
      "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n" +
      "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n" +
      "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n" +
      "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n" +
      "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n" +
      "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce\n";

    public static readonly string InputFileName = "08-seven-segments-input.txt";

    public class PatternNote {
        public string[] patterns;
        public string[] outputdigits;
        public PatternNote(string wirepatterntext, string digitpatterntext) {
            patterns = wirepatterntext.Split(' ', System.StringSplitOptions.TrimEntries|System.StringSplitOptions.RemoveEmptyEntries);
            outputdigits = digitpatterntext.Split(' ', System.StringSplitOptions.TrimEntries|System.StringSplitOptions.RemoveEmptyEntries);
        }
    }

    public static PatternNote[] ParseInput(string inputtext) {
        string[] lines = inputtext.Split('\n', StringSplitOptions.RemoveEmptyEntries|StringSplitOptions.TrimEntries);
        PatternNote[] patternnotes = new PatternNote[lines.Length];
        for (int i = 0; i < lines.Length; i++) {
            string[] sa = lines[i].Split('|', System.StringSplitOptions.TrimEntries|System.StringSplitOptions.RemoveEmptyEntries);
            if (sa.Length != 2) throw new System.Exception ("Invalid input line, does not contain two parts separated by '|' :  " + lines[i]);
            patternnotes[i] = new PatternNote (sa[0], sa[1]);
        }
        return patternnotes;
    }

    /// <summary>Check whether the number of segments could describe a specific digit</summary>
    public static bool CouldPatternBe (int maybeDigit, string pattern) {
        switch (maybeDigit) {
            case 0:  return pattern.Length == 6;
            case 1:  return pattern.Length == 2;
            case 2:  return pattern.Length == 5;
            case 3:  return pattern.Length == 5;
            case 4:  return pattern.Length == 4;
            case 5:  return pattern.Length == 5;
            case 6:  return pattern.Length == 6;
            case 7:  return pattern.Length == 3;
            case 8:  return pattern.Length == 7;
            case 9:  return pattern.Length == 6;
            default:  Console.WriteLine ($"Refusing to check for digit '{maybeDigit}'");  return false;
        }
    }

    /// <summary>Return whether the pattern has a unique number of segments that can only be one number</summary>
    public static bool IsDigitWithUniqueNumberOfSegments (string pattern) =>
        CouldPatternBe (1, pattern) || CouldPatternBe (4, pattern) || CouldPatternBe (7, pattern) || CouldPatternBe (8, pattern);

    public static int CountDigitsWithUniqueNumberOfSegments (IEnumerable<PatternNote> panos) {
        int n = 0;
        foreach (PatternNote pano in panos) {
            foreach (string pa in pano.outputdigits) {
                if (IsDigitWithUniqueNumberOfSegments (pa))  n++;
            }
        }
        return n;
    }

    public static void Main() {
        Console.WriteLine ("--- Example ---");
        PatternNote[] notes = ParseInput(ExampleInput);
        Console.WriteLine ($"Number of output digits with unique number of segments (digits 1, 4, 7, 8): {CountDigitsWithUniqueNumberOfSegments (notes)}" + Environment.NewLine);

        Console.WriteLine ("--- Aufgabe 1: Easy digits (1, 4, 7, 8) ---");
        string RealInput = File.ReadAllText (InputFileName);
        notes = ParseInput (RealInput);
        Console.WriteLine ($"Number of pattern notes in input: {notes.Length}");
        Console.WriteLine ($"*** Number of output digits with unique number of segments (digits 1, 4, 7, 8): {CountDigitsWithUniqueNumberOfSegments (notes)} ***");
    }
}
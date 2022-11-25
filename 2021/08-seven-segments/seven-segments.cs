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


    public static bool GetDigit6PatternAndSegCF (IEnumerable<string> patternsWith6Segments, string digit1pattern,
        out string digit6pattern, out char segc, out char segf) {

        digit6pattern = "";
        segf = ' ';
        segc = ' ';
        // In the group of digits with six segments (0, 6, 9):
        // Only digit 6 does not contain segment c (that is part of digit 1)
        foreach (string pa6s in patternsWith6Segments) {
            // If the set difference of the test pattern with the pattern of digit 1 has four elements,
            // it must contain all segments of digit 1 and is either digit 0 or 9;
            // if the length of the difference is 5 elements, there was only one segment of digit 1 contained,
            // so it must be the pattern of digit 6 and segment c and f are identified
            IEnumerable<char> diff61 = pa6s.Except (digit1pattern);
            if (diff61.Count() == 5) {
                digit6pattern = pa6s;
                IEnumerable<char> intersec61 = digit6pattern.Intersect (digit1pattern);
                segf = intersec61 .Single();
                segc = digit1pattern.Except (intersec61) .Single();
            } else if (diff61.Count() != 4) {
                throw new System.Exception ($"Unexpected pattern found when testing for digit 6: {pa6s} (checked against pattern for digit 1: {digit1pattern})");
            }
        }
        return digit6pattern != "";
    }


    public static bool GetDigit5PatternAndSegE (IEnumerable<string> patternsWith5Segments, string digit6pattern,
        out string digit5pattern, out char sege) {

        digit5pattern = "";
        sege = ' ';
        // A digit 5 is a digit 6 without segment e
        foreach (string pa5s in patternsWith5Segments) {
            // The set difference has to be segment e; the intersection of 5 and 6 is again digit 5
            IEnumerable<char> intersec56 = pa5s.Intersect (digit6pattern);
            if (intersec56.Count() == pa5s.Length) {
                digit5pattern = pa5s;
                sege = digit6pattern.Except (pa5s) .Single();
            }
        }
        return digit5pattern != "";
    }


    // Digit segments:
    //    aaa
    //   b   c
    //    ddd
    //   e   f
    //    ggg
    public static Dictionary<char,char> DeduceWireMapping (string[] wirepatterns) {
        // The mapping from wires a...g to segments a...g
        Dictionary<char,char> segmap = new Dictionary<char, char> ();
        // Digit patterns with unique number of segments
        string digit1pattern = wirepatterns.Single (s => CouldPatternBe (1, s));
        string digit4pattern = wirepatterns.Single (s => CouldPatternBe (4, s));
        string digit7pattern = wirepatterns.Single (s => CouldPatternBe (7, s));
        string digit8pattern = wirepatterns.Single (s => CouldPatternBe (8, s));
        // Digit patterns with non-unique number of segments
        IEnumerable<string> patternsWith5Segments = wirepatterns.Where (s => s.Length == 5);
        IEnumerable<string> patternsWith6Segments = wirepatterns.Where (s => s.Length == 6);
        if (patternsWith5Segments.Count() != 3)
            throw new System.Exception ($"Expected to find 3 digits with five segments but found: {patternsWith5Segments.Count()}");
        if (patternsWith6Segments.Count() != 3)
            throw new System.Exception ($"Expected to find 3 digits with six segments but found: {patternsWith6Segments.Count()}");
        // Find output wire for segment a:  not contained in digit 1 but in digit 7
        char sega = digit7pattern.Except(digit1pattern).Single();
        segmap[sega] = 'a';
        // Identify digit 6 and segments c and f
        string digit6pattern = "";
        char segc = ' ';
        char segf = ' ';
        if (!GetDigit6PatternAndSegCF (patternsWith6Segments, digit1pattern, out digit6pattern, out segc, out segf)
            || digit6pattern == "")  throw new Exception ("Could not determine pattern for digit 6");
        segmap[segc] = 'c';
        segmap[segf] = 'f';
        // Identify digit 5 and segment e
        string digit5pattern = "";
        char sege = ' ';
        if (!GetDigit5PatternAndSegE (patternsWith5Segments, digit6pattern, out digit5pattern, out sege)
            || digit5pattern == "")  throw new Exception ("Could not determine pattern for digit 5");
        segmap[sege] = 'e';
        // In the group of digits with 5 segments (2, 3, 5):  Only digit 2 does not contain segment f
        string digit2pattern = patternsWith5Segments.Where (s => !s.Contains (segf)).Single ();
        // The other segment not contained in digit 2 is segment b
        char segb = "abcdefg".Except (digit2pattern) .Where (c => c != segf) .Single ();
        segmap[segb] = 'b';
        // Digit 3 is the remaining pattern in the group with 5 segments
        string digit3pattern = patternsWith5Segments.Where (s => (s != digit2pattern && s != digit5pattern)) .Single ();
        // Digit 9 is the only digit with 6 segments that does not contain segment e
        string digit9pattern = patternsWith6Segments.Single (s => !s.Contains (sege));
        // Digit 0 is the last remaining pattern with 6 segments
        string digit0pattern = patternsWith6Segments.Where (s => (s != digit6pattern && s != digit9pattern)) .Single ();
        // The difference of digits 9 and 4 is segments a and g, a is already known, get g
        char segg = digit9pattern.Except (digit4pattern) .Single ((char c) => (c != sega));
        segmap[segg] = 'g';
        // The difference of digits 8 and 0 is segment d
        char segd = digit8pattern.Except (digit0pattern) .Single ();
        segmap[segd] = 'd';
        // All segments are known now
        return segmap;
    }

    public static IEnumerable<char> GetSegmentsForWires (string wires, Dictionary<char,char> wireToSegmentMap)
        => wires.Select ((char c) => (wireToSegmentMap[c]));

    public static int GetDigitForSegments (string wires, Dictionary<char,char> wireToSegmentMap) {
        List<char> segmentlist = GetSegmentsForWires (wires, wireToSegmentMap) .ToList ();
        segmentlist.Sort();
        string segments = new string (segmentlist.ToArray());
        if (segments == "abcefg")  return 0;
        if (segments == "cf")  return 1;
        if (segments == "acdeg")  return 2;
        if (segments == "acdfg")  return 3;
        if (segments == "bcdf")  return 4;
        if (segments == "abdfg")  return 5;
        if (segments == "abdefg")  return 6;
        if (segments == "acf")  return 7;
        if (segments == "abcdefg")  return 8;
        if (segments == "abcdfg")  return 9;
        return -1;
    }

    public static int DecodeNote (PatternNote pano) {
        Dictionary<char,char> segmentDict = DeduceWireMapping (pano.patterns);
        int n = 0;
        foreach (string s in pano.outputdigits) {
            int outdigit = GetDigitForSegments (s, segmentDict);
            n = n * 10 + outdigit;
        }
        return n;
    }

    public static int DecodeNotes (PatternNote[] notes, int loglevel = 1) {
        int sum = 0;
        foreach (PatternNote note in notes) {
            int n = DecodeNote (note);
            if (loglevel >= 2)
                Console.WriteLine ($"{string.Join (" ", note.patterns)}  {string.Join (" ", note.outputdigits)}  ->  {n}");
            sum += n;
        }
        return sum;
    }

    public static void Main() {
        Console.WriteLine ("--- Example: Count digits with unique number of segments ---");
        PatternNote[] notes = ParseInput(ExampleInput);
        Console.WriteLine ($"Number of output digits with unique number of segments (digits 1, 4, 7, 8): {CountDigitsWithUniqueNumberOfSegments (notes)}" + Environment.NewLine);

        Console.WriteLine ("--- Example: Decoding Numbers ---");
        string OneLiner = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf";
        PatternNote[] onelinenotes = ParseInput (OneLiner);
        Dictionary<char,char> segmentDict = DeduceWireMapping (onelinenotes[0].patterns);
        Console.WriteLine($"One-line example:  {OneLiner}  ->  Number: {DecodeNote (onelinenotes[0])}");
        Console.WriteLine ();

        int sum = DecodeNotes (notes, loglevel: 2);
        Console.WriteLine ();
        Console.WriteLine ($"Sum for larger example: {sum}");
        Console.WriteLine ();

        Console.WriteLine ("--- Aufgabe 1: Easy digits (1, 4, 7, 8) ---");
        string RealInput = File.ReadAllText (InputFileName);
        notes = ParseInput (RealInput);
        Console.WriteLine ($"Number of pattern notes in input: {notes.Length}");
        Console.WriteLine ($"*** Number of output digits with unique number of segments (digits 1, 4, 7, 8): {CountDigitsWithUniqueNumberOfSegments (notes)} ***");
        Console.WriteLine ();

        Console.WriteLine ("--- Aufgabe 2: Decoding the digits ---");
        sum = DecodeNotes (notes);
        Console.WriteLine ($"*** Sum of all 4-digit numbers:  {sum}");
    }
}

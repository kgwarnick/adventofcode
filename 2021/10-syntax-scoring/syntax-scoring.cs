// Advent of Code 2021, Day 10 Syntax Scoring
// https://adventofcode.com/2021/day/10

namespace karlgeorg.adventskal;

public class SyntaxScoring {

    public enum SyntaxCheckResult { UNKNOWN, INVALID, INCOMPLETE, FINISHED };

    public class CheckResultInfo {
        public int length = 0;
        public SyntaxCheckResult result = SyntaxCheckResult.UNKNOWN;
        public string? missing;
        public CheckResultInfo (int len, SyntaxCheckResult res, string? miss = null) {
            length = len;  result = res;  missing = miss;
        }
        public override string ToString () =>
            missing != null ?
                string.Format ("({0}, {1}, \"{2}\")", length, result, missing) :
                string.Format ("({0}, {1})", length, result);
    }

    public static readonly string ExampleLinesLegal =
        "()" + Environment.NewLine +
        "[]" + Environment.NewLine +
        "([])" + Environment.NewLine +
        "{()()()}" + Environment.NewLine +
        "<([{}])>" + Environment.NewLine +
        "[<>({}){}[([])<>]]" + Environment.NewLine +
        "(((((((((())))))))))" + Environment.NewLine;
    public static readonly string ExampleLinesIncomplete =
        "([]";
    public static readonly string ExampleLinesCorrupted =
        "(]" + Environment.NewLine +
        "{()()()>" + Environment.NewLine +
        "(((()))}" + Environment.NewLine +
        "<([]){()}[{}])" + Environment.NewLine +
        "({}[]>";
    public static readonly string ExampleInput =
        "[({(<(())[]>[[{[]{<()<>>" + Environment.NewLine +
        "[(()[<>])]({[<{<<[]>>(" + Environment.NewLine +
        "{([(<{}[<>[]}>{[]{[(<()>" + Environment.NewLine +
        "(((({<>}<{<{<>}{[]{[]{}" + Environment.NewLine +
        "[[<[([]))<([[{}[[()]]]" + Environment.NewLine +
        "[{[{({}]{}}([{[{{{}}([]" + Environment.NewLine +
        "{<[[]]>}<{[{[{[]{()[[[]" + Environment.NewLine +
        "[<(<(<(<{}))><([]([]()" + Environment.NewLine +
        "<{([([[(<>()){}]>(<<{{" + Environment.NewLine +
        "<{([{{}}[<[[[<>{}]]]>[]]";
    public static readonly string InputFileName = "10-syntax-scoring-input.txt";

    // Abkürzungen
    public const SyntaxCheckResult UNKNOWN = SyntaxCheckResult.UNKNOWN;
    public const SyntaxCheckResult INVALID = SyntaxCheckResult.INVALID;
    public const SyntaxCheckResult INCOMPLETE = SyntaxCheckResult.INCOMPLETE;
    public const SyntaxCheckResult FINISHED = SyntaxCheckResult.FINISHED;

    public static CheckResultInfo CheckLine (string line) {
        if (line.Length == 0) return new CheckResultInfo (0, FINISHED);
        char waitchar = ' ';
        int pos = 0;
        while (line.Length > pos) {
            if (line[pos] == '(')  waitchar = ')';
            else if (line[pos] == '[')  waitchar = ']';
            else if (line[pos] == '{')  waitchar = '}';
            else if (line[pos] == '<')  waitchar = '>';
            else return new CheckResultInfo (pos, UNKNOWN);
            pos++;
            if (line.Length <= pos)
                return new CheckResultInfo (pos, INCOMPLETE, waitchar.ToString());
            // Ende suchen
            while (line.Length > pos) {
                if (line[pos] == waitchar) { pos++; waitchar = ' '; break; }
                // Anderes Zeichen:  Öffnende Klammer rekursiv suchen
                CheckResultInfo testresult = CheckLine (line.Substring (pos));
                if (testresult.result == FINISHED) {
                    pos += testresult.length; break; }
                if (testresult.result == UNKNOWN && line[pos + testresult.length] == waitchar) {
                    pos += testresult.length + 1; waitchar = ' '; break; }
                else if (testresult.result == UNKNOWN) {
                    return new CheckResultInfo (pos + testresult.length, INVALID,
                        testresult.missing + waitchar);
                }
                else
                    return new CheckResultInfo (pos + testresult.length,
                        testresult.result,
                        (testresult.missing ?? "") + waitchar.ToString());
            }
        }
        return new CheckResultInfo (pos, waitchar == ' ' ? FINISHED : INCOMPLETE, waitchar.ToString());
    }


    public static int CorruptionScore (char illegalchar) =>
        illegalchar == ')' ? 3 : illegalchar == ']' ? 57 :
        illegalchar == '}' ? 1197 : illegalchar == '>' ? 25137 : 0;


    public static void TestOutput() {
        Console.WriteLine ("--- Legal lines ---");
        foreach (string line in ExampleLinesLegal.Split (Environment.NewLine)) {
            Console.WriteLine ("{0} -> {1}", line, CheckLine (line));
        }
        Console.WriteLine (Environment.NewLine + "--- Incomplete lines ---");
        foreach (string line in ExampleLinesIncomplete.Split (Environment.NewLine)) {
            Console.WriteLine ("{0} -> {1}", line, CheckLine (line));
        }
        Console.WriteLine (Environment.NewLine + "--- Corrupted lines ---");
        foreach (string line in ExampleLinesCorrupted.Split (Environment.NewLine)) {
            Console.WriteLine ("{0} -> {1}", line, CheckLine (line));
        }
    }

    public static Tuple<int, long> CalculateScores (string inputlines) {
        int errorscore = 0;
        List<long> complscores = new List<long> ();
        var linelen = inputlines.Split (Environment.NewLine,
            StringSplitOptions.RemoveEmptyEntries)
            .Select (s => s.Length) .Max ();
        foreach (string line in inputlines.Split (
                Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)) {
            CheckResultInfo result = CheckLine (line);
            Console.WriteLine ("{0}  -> {1}", line.PadRight (linelen), result);
            if (result.result == INVALID) {
                int linescore = CorruptionScore (line[result.length]);
                errorscore += linescore;
            }
            if (result.result == INCOMPLETE) {
                long complscore = 0;
                foreach (char c in (result.missing ?? "")) {
                    complscore = complscore * 5 +
                        (c == ')' ? 1 : c == ']' ? 2 : c == '}' ? 3 : c == '>' ? 4 : 0);
                }
                complscores.Add (complscore);
            }
        }
        complscores.Sort();
        long middlecompl = complscores[complscores.Count / 2];
        return new Tuple<int, long> (errorscore, middlecompl);
    }

    public static void Main() {
        TestOutput ();
        Console.WriteLine (Environment.NewLine + "--- Example lines ---");
        Tuple<int,long> examplescore = CalculateScores (ExampleInput);
        Console.WriteLine ("Error score, completion score: " + examplescore);

        Console.WriteLine (Environment.NewLine + "--- Puzzle Lines ---");
        Tuple<int,long> puzzlescore = CalculateScores (
            File.ReadAllText (InputFileName));

        Console.WriteLine (Environment.NewLine + "--- Aufgabe 1: Corruption Score ---");
        Console.WriteLine ("Error score: " + puzzlescore.Item1);

        Console.WriteLine (Environment.NewLine + "--- Aufgabe 2: Completion Score ---");
        Console.WriteLine ("Completion score: " + puzzlescore.Item2);
    }
}

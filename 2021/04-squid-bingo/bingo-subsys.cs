// Advent of Code 2021, Day 4 Bingo with a Giant Squid
//

namespace karlgeorg.adventskal;

public class BingoSubsystem {

    public static readonly string exampleinput =
        "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n" +
        "22 13 17 11  0\n" +
         "8  2 23  4 24\n" +
        "21  9 14 16  7\n" +
        " 6 10  3 18  5\n" +
         "1 12 20 15 19\n\n" +
         "3 15  0  2 22\n" +
         "9 18 13 17  5\n" +
        "19  8  7 25 23\n" +
        "20 11 10 24  4\n" +
        "14 21 16 12  6\n\n" +
        "14 21 17 24  4\n" +
        "10 16 15  9 19\n" +
        "18  8 23 26 20\n" +
        "22 11 13  6  5\n" +
        " 2  0 12  3  7\n";

    public static bool Setup(out List<int> numberList, out List<BingoBoard> boardList, StringReader sr) {
        numberList = new List<int>();
        boardList = new List<BingoBoard> ();
        string? numberLine = sr.ReadLine();
        if (numberLine == null)  return false;   // Setup failed
        numberList = numberLine.Split(',', StringSplitOptions.RemoveEmptyEntries|StringSplitOptions.TrimEntries).Select(s => int.Parse(s)).ToList<int>();
        // Console.WriteLine("Numbers: {0}", numberList);
        if (!string.IsNullOrWhiteSpace(sr.ReadLine())) {
            Console.WriteLine("Empty line missing, stop reading input");
            return false;   // Setup failed
        }
        boardList = new List<BingoBoard> ();
        bool anotherone = true;
        while (anotherone) {
            try {
                BingoBoard bb = new BingoBoard(sr);
                boardList.Add(bb);
                // Console.WriteLine("Bingo Board:\n{0}", bb);
            }
            catch (Exception ex) {
                Console.WriteLine($"End of input or error occurred (exception: {ex.Message}), stop reading bingo boards");
                anotherone = false;
                break;
            }
            if (!string.IsNullOrWhiteSpace(sr.ReadLine())) {
                Console.WriteLine("Empty line missing, stop reading input");
                return false;   // Setup failed
            }
        }
        return true;   // Setup successful
    }

    /// <summary>Draw the numbers until everyone is finished</summary>
    public static void Run(List<int> numbers, List<BingoBoard> bblist) {
        Console.WriteLine($"Drawing at most {numbers.Count} numbers for {bblist.Count} boards");
        int lastNumber;
        List<BingoBoard> finalBoards = new List<BingoBoard>();
        int finalNumber;
        List<BingoBoard> winnerBoards = Play(bblist, numbers, out lastNumber, out finalNumber, out finalBoards);
        Console.WriteLine($"Last drawn number: {lastNumber}, winners: {winnerBoards.Count}");
        foreach (BingoBoard winner in winnerBoards) {
            int summeFrei = winner.GetUnmarked().Aggregate((sum, n) => n + sum);
            Console.WriteLine($"Winner:  Sum of unmarked numbers: {summeFrei} -> final score = {lastNumber * summeFrei}");
        }
        foreach (BingoBoard winner in finalBoards) {
            int summeFrei = winner.GetUnmarked().Aggregate((sum, n) => n + sum);
            Console.WriteLine($"Last winner:  Sum of unmarked numbers: {summeFrei} -> final score = {finalNumber * summeFrei}");
        }
    }

    public static void Main() {
        Console.WriteLine("--- Beispiel ---");
        List<BingoBoard> bblist = new List<BingoBoard>();
        List<int> numberlist = new List<int>();
        if (Setup(out numberlist, out bblist, new StringReader(exampleinput))) {
            Run(numberlist, bblist);
        }
        Console.WriteLine();

        Console.WriteLine("--- Aufgabe 1/2: Find the firts/last winning board ---");
        if (Setup(out numberlist, out bblist, new StringReader(File.ReadAllText("04-bingo-subsys-input.txt")))) {
            Run(numberlist, bblist);
        }
    }

    /// <summary>Play a Bingo round</summary>
    /// <returns>the list winning boards, the number drawn last is stored in winningNumber</return>
    public static List<BingoBoard> Play(List<BingoBoard> boards, List<int> numberSequence,
        out int winningNumber, out int lastWinningNumber, out List<BingoBoard> lastFinishers) {

        List<BingoBoard> winners = new List<BingoBoard>();
        winningNumber = -1;
        lastWinningNumber = -1;
        lastFinishers = new List<BingoBoard>();
        int numFinished = 0;
        foreach (int n in numberSequence) {
            Console.WriteLine($"Next number: {n}");
            List<BingoBoard> finishers = new List<BingoBoard>();   // Finishers for this round
            foreach (BingoBoard b in boards) {
                if (b.IsFinished) continue;   // Skip testing already finished boards
                if (b.nextNumber(n)) {
                    finishers.Add(b);
                    numFinished++;
                    if (winningNumber < 0) {
                        winners.Add(b);
                    }
                }
            }
            if (winners.Count > 0 && winningNumber < 0) {
                Console.WriteLine("We have one or more winners!");
                winningNumber = n;
            }
            if (numFinished >= boards.Count) {
                Console.WriteLine("Everyone has finished their board now!");
                lastFinishers = finishers;
                lastWinningNumber = n;
                break;
            }
        }
        return winners;
    }
}

public class BingoBoard {
    public readonly int Lines = 5;
    public readonly int Columns = 5;
    private int[,] board;
    private bool[,] marked;
    public bool IsFinished { get; private set; }

    public BingoBoard(System.IO.StringReader reader) {
        board = new int[5,5];
        marked = new bool[5,5];
        IsFinished = false;
        int elementsread = 0;
        for (int i = 0; i < 5; i++) {
            string? line = reader.ReadLine();
            if (line == null) throw new Exception("Unexpected end of file");
            int[] elements = line.Split(' ', StringSplitOptions.RemoveEmptyEntries|StringSplitOptions.TrimEntries)
                .Select (s => int.Parse(s)).ToArray();
            foreach (int el in elements) {
                if (elementsread >= 25) {
                    Console.WriteLine("Ignoring excess elements starting from {0} to end of line", string.Join(", ", el));
                }
                board[elementsread / 5, elementsread % 5] = el;
                elementsread++;
            }
        }
    }

    /// <returns>How often the number was found</returns>
    public int markNumber(int number) {
        int occurred = 0;
        for (int l = 0; l < 5; l++) {
            for (int c = 0; c < 5; c++) {
                if (board[l,c] == number) {
                    marked[l,c] = true;
                    occurred++;
                }
            }
        }
        return occurred;
    }

    public bool isBingo() {
        // Test all 5 lines and columns
        for (int i = 0; i < 5; i++) {
            if (marked[i,0] && marked[i,1] && marked[i,2] && marked[i,3] && marked[i,4]) {
                IsFinished = true;
                return true;
            }
            if (marked[0,i] && marked[1,i] && marked[2,i] && marked[3,i] && marked[4,i]) {
                IsFinished = true;
                return true;
            }
        }
        return false;
    }

    public bool nextNumber(int number) {
        markNumber(number);
        return isBingo();
    }

    public List<int> GetUnmarked() {
        List<int> unmarked = new List<int>();
        for (int l = 0; l < Lines; l++) {
            for (int c = 0; c < Columns; c++) {
                if (!marked[l,c]) unmarked.Add(board[l,c]);
            }
        }
        return unmarked;
    }

    public override string ToString() {
        string output = "";
        for (int l = 0; l < 5; l++) {
            output += string.Format("{0,3} {1,3} {2,3} {3,3} {4,3}\n", board[l,0], board[l,1], board[l,2], board[l,3], board[l,4]);
        }
        return output;
    }
}

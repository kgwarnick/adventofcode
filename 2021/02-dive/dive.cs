namespace karlgeorg.adventskal;

public class TauchBoot {

    // Read a list of commands into an array of tuples (direction, distance)
    public static Tuple<string, int>[] CommandReader(string[] commands) =>
        commands.Select((c) => new Tuple<string,int>(c.Split()[0], int.Parse(c.Split()[1]))).ToArray();

    // Read the command list from a file with one command per line
    public static Tuple<string, int>[] CommandReader(string filename) => CommandReader(File.ReadAllLines(filename));

    // Calculate the new position for a single command specified as direction and distance
    public static Tuple<int, int> CalculatePositionFromXY(Tuple<int, int> currentPos, string command, int distance) {
        int dx = 0, dy = 0;
        switch (command) {
            case "forward": dx = distance;  break;
            case "down": dy = distance;  break;
            case "up": dy = - distance;  break;
            default: throw new ArgumentException($"Unsupported command: {command}");
        }
        return new Tuple<int, int> (currentPos.Item1 + dx, currentPos.Item2 + dy);
    }

    // Calculate the new position for a list of commands given as direction and distance pairs
    public static Tuple<int, int> CalculatePositionFromXY(Tuple<string, int>[] Anweisungen, Tuple<int, int>? StartPos) {
        Tuple<int, int> position = StartPos ?? new Tuple<int, int> (0, 0);
        foreach (Tuple<string, int> anweisung in Anweisungen) {
            position = CalculatePositionFromXY(position, anweisung.Item1, anweisung.Item2);
        }
        return position;
    }


    public static readonly string[] BeispielAnweisungen = new string[] { "forward 5", "down 5", "forward 8", "up 3", "down 8", "forward 2" };

    public static void Main() {


        Console.WriteLine("--- Beispiel ---");
        Tuple<string,int>[] kommandos = CommandReader(BeispielAnweisungen);
        Console.WriteLine($"Anweisungen: {kommandos.Count()} ... {string.Join<Tuple<string,int>>(", ", kommandos)}");
        Tuple<int, int> endPosition = CalculatePositionFromXY(kommandos, new Tuple<int, int> (0, 0));
        System.Console.WriteLine($"Endposition: {endPosition} -> Produkt: {endPosition.Item1 * endPosition.Item2}");
        Console.WriteLine();

        Console.WriteLine("--- Aufgabe 1: Wo steht das Tauchboot am Ende? ---");
        Tuple<string,int>[] anweisungen = CommandReader("02-dive-input.txt");
        Console.WriteLine($"Anweisungen: {anweisungen.Length}");
        endPosition = CalculatePositionFromXY(anweisungen, new Tuple<int, int> (0, 0));
        System.Console.WriteLine($"Endposition: {endPosition} -> Produkt: {endPosition.Item1 * endPosition.Item2}");
    }
}

namespace karlgeorg.adventskal;

public class LanternFish {
    public static int Main(string[] args) {

        Console.WriteLine("--- Minimal-Beispiel: 1 Fisch ---");
        ModelPopulation("3", 5, logLevel: 3);
        Console.WriteLine();

        Console.WriteLine("--- Beispiel ---");
        ModelPopulation("3,4,3,1,2", 18, logLevel: 2);
        ModelPopulation("3,4,3,1,2", 80, logLevel: 1);
        Console.WriteLine();

        Console.WriteLine("--- Aufgabe 1: In 80 Tagen durch das Meer ---");
        string eingabeDateiName = (args.Length > 0 ) ? args[0] : "06-lanternfish-input.txt";
        string schwarmdata = File.ReadAllText(eingabeDateiName).Trim();
        Console.WriteLine($"Start population from file {eingabeDateiName}: {schwarmdata}");
        Console.WriteLine($"End population:  {ModelPopulation(schwarmdata, 80, logLevel: 1)}");
        Console.WriteLine();

        return 0;
    }

    public static List<byte> ParsePopulation(string volkszählung) =>
        volkszählung.Split(',', StringSplitOptions.TrimEntries|StringSplitOptions.RemoveEmptyEntries).Select(s => byte.Parse(s)).ToList();

    public static int ModelPopulation(string startPop, int numGenerations, int logLevel = 1) {
        List<byte> currpop = ParsePopulation(startPop);
        if (logLevel >= 2)
            Console.WriteLine($"Initial state: {currpop.Count}, [{string.Join(", ", currpop)}]");
        else if (logLevel >= 1)
            Console.WriteLine($"Start population: {currpop.Count}");
        for (int g = 0; g < numGenerations; g++) {
            if (logLevel >= 3)
                Console.WriteLine($"After {g,4} {(g != 1 ? "days:" : "day: ")} {currpop.Count}, [{string.Join(",",currpop)}]");
            List<byte> nextpop = GetNextPopulationStep(currpop);
            currpop = nextpop;
        }
        if (logLevel >= 2)
            Console.WriteLine($"End population in generation {numGenerations}: {currpop.Count}, [{string.Join(",", currpop)}]");
        else if (logLevel >= 1)
            Console.WriteLine($"End population in generation {numGenerations}: {currpop.Count}");
        return currpop.Count;
    }

    public static List<byte> GetNextPopulationStep(List<byte> currentPopulation) {
        List<byte> newpopulation = new List<byte>();
        foreach (byte timer in currentPopulation) {
            bool shouldSpawn;
            newpopulation.Add(GetNextTimerValueAndDecideWhetherToSpawn(timer, out shouldSpawn));
            if (shouldSpawn)  newpopulation.Add(8);
        }
        return newpopulation;
    }

    public static byte GetNextTimerValueAndDecideWhetherToSpawn(byte timer, out bool spawn) {
        if (timer > 0) {
            spawn = false;
            return (byte)(timer - 1);
        } else {
            spawn = true;
            return 6;
        }
    }
}
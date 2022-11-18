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

        Console.WriteLine("--- Minimal-Beispiel in Altersgruppen-Implementierung ---");
        long sumpop = ModelPopulationInAgeGroups("3", 5, logLevel: 3);
        Console.WriteLine();

        Console.WriteLine("--- Beispiel in Altersgruppen-Implementierung ---");
        Console.WriteLine($"Start population:  3,4,3,1,2");
        sumpop = ModelPopulationInAgeGroups("3,4,3,1,2", 18, logLevel: 3);
        Console.WriteLine($"End population: {sumpop}");
        sumpop = ModelPopulationInAgeGroups("3,4,3,1,2", 80, logLevel: 1);
        Console.WriteLine($"End population: {sumpop}");
        Console.WriteLine();

        Console.WriteLine("--- Aufgabe 2: In 256 Tagen durch das Meer ---");
        Console.WriteLine($"End population:  {ModelPopulationInAgeGroups(schwarmdata, 256, logLevel: 2)}");

        return 0;
    }

    public static List<byte> ParsePopulation(string volkszählung) =>
        volkszählung.Split(',', StringSplitOptions.TrimEntries|StringSplitOptions.RemoveEmptyEntries).Select(s => byte.Parse(s)).ToList();

    public static long sumArray (long[] arr) => arr.Aggregate((sum, el) => (sum + el));

    public static int ModelPopulation(string startPop, int numGenerations, int logLevel = 1) {
        List<byte> currpop = ParsePopulation(startPop);
        if (logLevel >= 2)
            Console.WriteLine($"Initial state: {currpop.Count}, [{string.Join(", ", currpop)}]");
        else if (logLevel >= 1)
            Console.WriteLine($"Initial state: {currpop.Count}");
        for (int g = 0; g < numGenerations; g++) {
            if (logLevel >= 3)
                Console.WriteLine($"After {g,4} {(g != 1 ? "days:" : "day: ")} {currpop.Count}, [{string.Join(",",currpop)}]");
            else if (logLevel >= 2)
                Console.WriteLine($"After {g,4} {(g != 1 ? "days:" : "day: ")} {currpop.Count}");
            List<byte> nextpop = GetNextPopulationStep(currpop);
            currpop = nextpop;
        }
        if (logLevel >= 2)
            Console.WriteLine($"End population in generation {numGenerations}: {currpop.Count}, [{string.Join(",", currpop)}]");
        else if (logLevel >= 1)
            Console.WriteLine($"End population in generation {numGenerations}: {currpop.Count}");
        return currpop.Count;
    }

    public static long ModelPopulationInAgeGroups(string startPop, int numGenerations, int logLevel) {
        // Set up age groups
        List<byte> startpop = ParsePopulation(startPop);
        long[] numberInAgeGroup = GetAgeGroups(startpop);
        if (logLevel >= 2)
            Console.WriteLine($"Start population:  {sumArray (numberInAgeGroup)},  Members per age group 0...8: [ {string.Join(", ", numberInAgeGroup)} ]");
        else if (logLevel >= 1)
            Console.WriteLine($"Start population:  {sumArray (numberInAgeGroup)}");
        // Simulate generations
        for (int g = 0; g < numGenerations; g++) {
            if (logLevel >= 3)
                Console.WriteLine($"After {g,4} {(g != 1 ? "days:" : "day: ")}  {sumArray (numberInAgeGroup)},  Members per age group 0...8: [ {string.Join(", ", numberInAgeGroup)} ]");
            else if (logLevel >= 2)
                Console.WriteLine($"After {g,4} {(g != 1 ? "days:" : "day: ")}  {sumArray (numberInAgeGroup)}");
            long[] newAgeGroups = GetNextPopulationStepInAgeGroups(numberInAgeGroup);
            numberInAgeGroup = newAgeGroups;
        }
        if (logLevel >= 2)
            Console.WriteLine($"End population in generation {numGenerations}:  {sumArray (numberInAgeGroup)},  Members per age group 0...8: [ {string.Join(", ", numberInAgeGroup)} ]");
        else if (logLevel >= 1)
            Console.WriteLine($"End population in generation {numGenerations}:  {sumArray (numberInAgeGroup)}");
        return sumArray(numberInAgeGroup);
    }

    public static long[] GetAgeGroups(List<byte> population) {
        long[] ageGroups = new long[9];
        for (int ag = 0; ag < 9; ag++) {
            ageGroups[ag] = population.Count(b => b == ag);
        }
        return ageGroups;
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

    public static long[] GetNextPopulationStepInAgeGroups(long[] currentAgeGroups) {
        long[] newAgeGroups = new long[9];
        for (int ag = 0; ag < 8; ag++) {
            newAgeGroups[ag] = currentAgeGroups[ag + 1];
        }
        // Fish with timer value 0 spawn a fish with timer vaue 8 and are reset to 6
        newAgeGroups[8] = currentAgeGroups[0];
        newAgeGroups[6] += currentAgeGroups[0];
        return newAgeGroups;
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
// Advent of Code 2015, Day 21 RPG Simulator 20XX
// https://adventofcode.com/2015/day/21

package karlgeorg.adventskal;

import java.util.List;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class RpgSimulator {

  public final static String inputfilename = "21-rpg-simulator-20xx-input.txt";

  public final static MagicItem[] Weapons = new MagicItem[] {
    // A weapon is obligatory, there is no "BareHands" weapon
    new MagicItem ("Dagger",      8, 4, 0),
    new MagicItem ("Shortsword", 10, 5, 0),
    new MagicItem ("Warhammer",  25, 6, 0),
    new MagicItem ("Longsword",  40, 7, 0),
    new MagicItem ("Greataxe",   74, 8, 0)
  };

  public final static MagicItem[] Armors = new MagicItem[] {
    // include "no armor" so it can be treated in the same way as real armor
    new MagicItem ("No Armor",     0, 0,  0),
    new MagicItem ("Leather",     13, 0,  1),
    new MagicItem ("Chainmail",   31, 0,  2),
    new MagicItem ("Splintmail",  53, 0,  3),
    new MagicItem ("Bandedmail",  75, 0,  4),
    new MagicItem ("Platemail",  102, 0,  5),
  };

  public final static MagicItem[] Rings = new MagicItem[] {
    // include two "no ring"s so they can be treated in the same way
    new MagicItem ("No Ring 1",    0, 0, 0),
    new MagicItem ("No Ring 2",    0, 0, 0),
    new MagicItem ("Damage +1 ",  25, 1, 0),
    new MagicItem ("Damage +2 ",  50, 2, 0),
    new MagicItem ("Damage +3 ", 100, 3, 0),
    new MagicItem ("Defense +1",  20, 0, 1),
    new MagicItem ("Defense +2",  40, 0, 2),
    new MagicItem ("Defense +3",  80, 0, 3)
  };

  /** An item such as a weapon or armor with certain properties */
  static class MagicItem {
    public String title;
    public int cost;
    public int damage;
    public int armor;

    public MagicItem (String itemtitle, int costs, int damage, int armor) {
      title = itemtitle;
      cost = costs;
      this.damage = damage;
      this.armor = armor;
    }

    @Override
    public String toString() {
      return String.format ("%-10s  cost %3d   dam %2d  arm %2d",
        title, cost, damage, armor);
    }
  }

  /** Attributes of an actor (player, enemy) */
  static class ActorStats {
    public String name;
    public int hitpoints;
    public int armor;
    public int damage;

    public ActorStats (String newname, int hp, int arm, int dam) {
      this.name = newname;
      this.hitpoints = hp;
      this.armor = arm;
      this.damage = dam;
    }
  }


  /** How many rounds an actor can last */
  public static int GetRoundsStanding (int hitpoints, int armor,
      int opponentdamage) {
    int hplost = opponentdamage - armor > 0 ? opponentdamage - armor : 1;
    return (hitpoints - 1) / hplost;
  }

  /** Who will win a fight (and how close).
   * @return &gt; 0  Actor 1 wins,  &lt; 0  actor 2 wins,
   *              = 0  both go down in the same round;
   *              the magnitude shows how many more rounds the winner
   *              could have done. */
  public static int GetFightWinner (ActorStats actor1, ActorStats actor2) {
    return GetRoundsStanding (actor1.hitpoints, actor1.armor, actor2.damage)
      - GetRoundsStanding (actor2.hitpoints, actor2.armor, actor1.damage);
  }

  /** Find the cheapest/most expensive buy that still lets actor 1 win/lose
   *  the fight.
   *  @param shouldwin  Whether to calculate the least amount of gold
   *                    to win (true) or the most amount to lose (false) */
  public static int GetBestBuy (ActorStats actor1, ActorStats actor2,
      boolean shouldwin) {
    int totalcost = shouldwin ? 9999 : 0;
    // Choose all combinations of items (weapon, armor, left and right ring)
    ActorStats actor1try = new ActorStats ("Player (Try)", actor1.hitpoints,
      actor1.armor, actor1.damage);
    for (int w = 0; w < Weapons.length; w++) {
      for (int a = 0; a < Armors.length; a++) {
        for (int l = 0; l < Rings.length - 1; l++) {
          for (int r = l + 1; r < Rings.length; r++) {
            actor1try.armor = actor1.armor + Weapons[w].armor +
              Armors[a].armor + Rings[l].armor + Rings[r].armor;
            actor1try.damage = actor1.damage + Weapons[w].damage +
              Armors[a].damage + Rings[l].damage + Rings[r].damage;
            int winner = GetFightWinner (actor1try, actor2);
            // Player wins if they can stand at least as many rounds
            // as the enemy
            if (((winner >= 0) && shouldwin) || ((winner < 0) && !shouldwin)) {
              int cost = Weapons[w].cost + Armors[a].cost +
                      Rings[l].cost + Rings[r].cost;
              if (((cost < totalcost) && shouldwin) ||
                  ((cost > totalcost) && !shouldwin)) {
                String weapon = Weapons[w].title;
                String armor = Armors[a].title;
                String ring1 = Rings[l].title;
                String ring2 = Rings[r].title;
                totalcost = cost;
                int playerrounds = GetRoundsStanding (actor1try.hitpoints,
                  actor1try.armor, actor2.damage);
                int enemyrounds = GetRoundsStanding (actor2.hitpoints,
                  actor2.armor, actor1try.damage);
                System.out.println (
                  (shouldwin ? "+ Winning" : "- Losing") + " equipment: " +
                  weapon + ", " +
                  armor + ", " + ring1 + ", " + ring2 +
                  " -> costs " + totalcost + ",  player " + playerrounds +
                  " rounds, enemy " + enemyrounds + " rounds");
              }
            }
          }
        }
      }
    }
    return totalcost;
  }

  /** Read attributes for an actor (hit points, armor, damage) from a file */
  public static void ReadActorStatsFromFile (String filename,
      ActorStats stats) throws IOException {
    List<String> inputlines;
    inputlines = Files.readAllLines (Paths.get (filename));
    for (String line : inputlines) {
      if (line.startsWith ("Hit Points: "))
        stats.hitpoints = Integer.parseInt (line.substring (12));
      if (line.startsWith ("Damage: "))
        stats.damage = Integer.parseInt (line.substring (8));
      if (line.startsWith ("Armor: "))
        stats.armor = Integer.parseInt (line.substring (7));
    }
  }


  public static void main (String[]args) {
    System.out.println ("--- Shop offers ---");
    System.out.println ("Weapons: " + Weapons.length);
    for (int i = 0; i < Weapons.length; i++)
      System.out.println (Weapons[i]);
    System.out.println ("Armor: " + Armors.length);
    for (int i = 0; i < Armors.length; i++)
      System.out.println (Armors[i]);
    System.out.println ("Rings: " + Rings.length);
    for (int i = 0; i < Rings.length; i++)
      System.out.println (Rings[i]);
    // Player:  100 HP, 0 DAM, 0 ARM;  Boss: 100 HP, 7 DAM, 2 ARM
    System.out.println ();
    System.out.println ("--- Examples ---");
    ActorStats traineestats = new ActorStats ("Trainee Player", 8, 5, 5);
    ActorStats enemystats = new ActorStats ("Enemy", 12, 2, 7);
    System.out.println ("- Player (8 HP, 5 ARM) can resist 7 DAM for " +
      GetRoundsStanding (8, 5, 7) + " rounds.");
    System.out.println ("- Boss (12 HP, 2 ARM) can resist 5 DAM for " +
      GetRoundsStanding (12, 2, 5) + " rounds.");
    System.out.println ("-> Player - Boss: " +
      GetFightWinner (traineestats, enemystats));
    System.out.println ();

    System.out.println ("--- Part 1: Cheapest win ---");
    ActorStats playerstats = new ActorStats ("Player", 100, 0, 0);
    ActorStats bossstats = new ActorStats ("Boss", 100, 1, 1);
    try {
      ReadActorStatsFromFile (inputfilename, bossstats);
    }
    catch (IOException ioex) {
      System.err.println ("Failed to read file \"" + inputfilename + "\": "+
        ioex);
      System.exit (1);
    }
    System.out.printf ("Player Stats: %d HP, armor: %d, damage: %d\n",
      playerstats.hitpoints, playerstats.armor, playerstats.damage);
    System.out.printf ("Boss Stats: %d HP, armor: %d, damage: %d\n",
      bossstats.hitpoints, bossstats.armor, bossstats.damage);
    int best = GetBestBuy (playerstats, bossstats, true);
    System.out.println ("* Cheapest amount of gold that still wins: " + best);
    System.out.println ();

    System.out.println ("--- Part 2: Most expensive loss ---");
    int worst = GetBestBuy (playerstats, bossstats, false);
    System.out.println ("* Most amount of gold that still loses: " + worst);
  }
}

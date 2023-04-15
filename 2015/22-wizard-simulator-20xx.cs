// Advent of Code 2015, Day 22 Wizard Simulator 20XX
// https://adventofcode.com/2015/day/22

namespace karlgeorg.adventskal;


public class WizardSimulator {

  public static readonly int MagicMissileManaCost = 53;
  public static readonly int DrainManaCost = 73;
  public static readonly int ShieldManaCost = 113;
  public static readonly int PoisonManaCost = 173;
  public static readonly int RechargeManaCost = 229;
  public static readonly int MagicMissileDamage = 4;
  public static readonly int DrainDamage = 2;
  public static readonly int DrainHeal = 2;
  public static readonly int ShieldArmorBoost = 7;
  public static readonly uint ShieldTimer = 6;
  public static readonly int PoisonDamage = 3;
  public static readonly uint PoisonTimer = 6;
  public static readonly int RechargeManaRegenerate = 101;
  public static readonly uint RechargeTimer = 5;

  /// <summary>An actor, player or boss</summary>
  public class WizardStats {
    public int hitpoints;
    public int mana;
    public int armor;
    public int damage;
    public uint shieldtime;
    public uint poisontime;
    public uint rechargetime;
    public WizardStats (int hp, int mn, int arm = 0, int dam = 0,
        uint shtime = 0, uint poistime = 0, uint rechtime = 0) {
      hitpoints = hp;  mana = mn;  armor = arm;  damage = dam;
      shieldtime = shtime;  poisontime = poistime;  rechargetime = rechtime;
    }
    public WizardStats (WizardStats other) {
      this.hitpoints = other.hitpoints;  this.mana = other.mana;
      this.armor = other.armor;  this.damage = other.damage;
      this.shieldtime = other.shieldtime;  this.poisontime = other.poisontime;
      this.rechargetime = other.rechargetime;
    }
    public override string ToString () {
      return string.Format ("{0} HP,  {1} Mana,  {2} Armor,  {3} Damage",
          hitpoints, mana, armor, damage) +
        (shieldtime > 0 ? string.Format ("  shield time {0}", shieldtime) : "") +
        (poisontime > 0 ? string.Format ("  poison time {0}", poisontime) : "") +
        (rechargetime > 0 ? string.Format ("  recharge time {0}", rechargetime) : "");
    }
  }

  /// <summary>Spell properties</summary>
  public struct Spell {
    public string name;
    public PlayerTurnType func;
    public int manacost;
    public Spell (string spellname, PlayerTurnType spellfunc, int mana) {
      name = spellname;  func = spellfunc;  manacost = mana;
    }
  }
  // Possible spells
  public static readonly Spell MagicMissileSpell =
    new Spell ("Magic Missile", PlayerCastMagicMissile, MagicMissileManaCost);
  public static readonly Spell DrainSpell =
    new Spell ("Drain", PlayerCastDrain, DrainManaCost);
  public static readonly Spell ShieldSpell =
    new Spell ("Shield", PlayerCastShield, ShieldManaCost);
  public static readonly Spell PoisonSpell =
    new Spell ("Poison", PlayerCastPoison, PoisonManaCost);
  public static readonly Spell RechargeSpell =
    new Spell ("Recharge", PlayerCastRecharge, RechargeManaCost);
  /// <summary>All available spells</summary>
  public static Spell[] SpellBook = new Spell[] {
    MagicMissileSpell, DrainSpell, ShieldSpell, PoisonSpell, RechargeSpell
  };

  /// <summary>A spell to cast, it will modify one or both actors if successful
  /// </summary>
  /// \return Whether the spell could be cast, according to mana needs and
  ///         depending on whether the effect is not already active
  public delegate bool PlayerTurnType (ref WizardStats player,
    ref WizardStats boss);

  public static bool PlayerCastMagicMissile (ref WizardStats player,
      ref WizardStats boss) {
    if (player.mana < MagicMissileManaCost)  return false;
    player.mana -= MagicMissileManaCost;
    boss.hitpoints -= MagicMissileDamage;
    return true;
  }

  public static bool PlayerCastDrain (ref WizardStats player,
      ref WizardStats boss) {
    if (player.mana < DrainManaCost)  return false;
    player.mana -= DrainManaCost;
    boss.hitpoints -= DrainDamage;
    player.hitpoints += DrainHeal;
    return true;
  }

  public static bool PlayerCastShield (ref WizardStats player,
      ref WizardStats boss) {
    if (player.mana < ShieldManaCost)  return false;
    if (player.shieldtime > 0)  return false;
    player.mana -= ShieldManaCost;
    player.shieldtime = ShieldTimer;
    return true;
  }

  public static bool PlayerCastPoison (ref WizardStats player,
      ref WizardStats boss) {
    if (player.mana < PoisonManaCost)  return false;
    if (player.poisontime > 0)  return false;
    player.mana -= PoisonManaCost;
    player.poisontime = PoisonTimer;
    return true;
  }

  public static bool PlayerCastRecharge (ref WizardStats player,
      ref WizardStats boss) {
    if (player.mana < RechargeManaCost)  return false;
    if (player.rechargetime > 0)  return false;
    player.mana -= RechargeManaCost;
    player.rechargetime = RechargeTimer;
    return true;
  }

  /// <summary>A non-magical attack by the boss, the player will be modified
  /// </summary>
  /// <returns>Always true because the attack always succeeds</returns>
  public static bool BossAttack (ref WizardStats boss,
      ref WizardStats player) {
    int damage = boss.damage;
    if (player.shieldtime > 0)
      damage = (damage > ShieldArmorBoost) ? damage - ShieldArmorBoost : 1;
    player.hitpoints -= damage;
    return true;
  }

  /// <summary>Play a boss turn, including effects and attack</summary>
  public static void PlayBossTurn (ref WizardStats boss, ref WizardStats player) {
    // Effects first
    ApplyEffects (ref player, ref boss);
    if (player.hitpoints <= 0) {
      Console.WriteLine ("--! player lost (boss env)");
      return;
    }
    if (boss.hitpoints <= 0) {
      // Console.WriteLine ("--! Player win (environment killed boss before their turn)");
      return;
    }
    // Attack
    BossAttack (ref boss, ref player);
  }

  /// <summary>Apply effects of currently active spells</summary>
  public static void ApplyEffects (ref WizardStats player,
      ref WizardStats boss) {
    if (player.shieldtime > 0)  player.shieldtime --;
    if (player.poisontime > 0) {
      player.poisontime--;
      boss.hitpoints -= PoisonDamage;
    }
    if (player.rechargetime > 0) {
      player.rechargetime--;
      player.mana += RechargeManaRegenerate;
    }
  }

  /// <summary>Play out a certain "double turn",
  /// i.e. player action followed by boss action</summary>
  /// <returns>1 if the player won, -1 if the boss won,
  ///          0 if the fight is still on</returns>
  public static int PlayScriptedDoubleTurn (PlayerTurnType spell,
      ref WizardStats player, ref WizardStats boss) {
    // Player turn first
    ApplyEffects (ref player, ref boss);
    if (player.hitpoints <= 0)  return -1;
    if (boss.hitpoints <= 0)  return 1;
    // Cast the chosen spell, if not possible the player loses
    if (!spell (ref player, ref boss))  return -1;
    if (boss.hitpoints <= 0)  return 1;
    // Boss turn
    ApplyEffects (ref player, ref boss);
    if (player.hitpoints <= 0)  return -1;
    if (boss.hitpoints <= 0)  return 1;
    // Attack
    BossAttack (ref boss, ref player);
    if (player.hitpoints <= 0)  return -1;
    // Undecided
    return 0;
  }

  /// <summary>Follow a fixed sequence of spells and simulate the fight</summary>
  public static void PlayScriptedBattle (Spell[] playerstrategy,
      ref WizardStats player, ref WizardStats boss) {
    foreach (Spell spell in playerstrategy) {
      Console.WriteLine ("+ Casting spell {0}", spell.name);
      int res = PlayScriptedDoubleTurn (spell.func, ref player, ref boss);
      Console.WriteLine (
        "  -> {4}   Player: {0} HP, {1} Mana,  Boss: {2} HP, {3} Mana",
        player.hitpoints, player.mana, boss.hitpoints, boss.mana,
        res > 0 ? "[WIN  ]" : (res < 0 ? "[LOSS ]" : "[FIGHT]"));
    }
  }

  /// <summary>Recursively play rounds to find the least mana to win</summary>
  /// depth and spell are used to track the strategy found.
  /// <returns>Mana spent for the strategy needing least mana,
  ///          negative: player cannot win along this path</returns>
  public static int TryDoubleTurn (WizardStats player, WizardStats boss,
      bool harddifficulty = false, int depth = 0, Spell[]? spellsused = null) {
    // Try limiting depth if the search does not finish otherwise
    // if (depth > 20)  return -1;
    // Console.WriteLine ($"[{depth}] (entry) " + "Player: " + player + ",   Boss: " + boss);
    // Additional environment effect from difficulty setting: lose 1 hit point
    if (harddifficulty) {
      if (--player.hitpoints <= 0) {
        // Console.WriteLine (
        //   "[{0}] player lost (environment/difficulty effects)", depth);
        return -1;   // player loses
      }
    }
    // Apply effects from spells
    ApplyEffects (ref player, ref boss);
    if (boss.hitpoints <= 0) {
      // Console.WriteLine ("-! Player win (environment killed boss before player turn)");
      return 0;   // player wins without casting any spell this turn
    }
    // Choose a spell to cast
    int manabest = int.MaxValue;   // initialise to "no way to win"
    spellsused ??= Array.Empty<Spell> ();   // Start with an empty list by default
    foreach (Spell spell in SpellBook) {
      // Player turn first
      WizardStats newplayer = new WizardStats (player);
      WizardStats newboss = new WizardStats (boss);
      if (spell.func (ref newplayer, ref newboss)) {
        // Console.WriteLine ("[{0}] ~Player casts {1}", depth, spell.name);
        // Console.WriteLine ("[{0}] ~Player: {1},  Boss: {2}", depth, newplayer, newboss);
        if (newboss.hitpoints > 0)  PlayBossTurn (ref newboss, ref newplayer);
        if (newplayer.hitpoints <= 0) continue;   // player loses, try a different choice
        if (newboss.hitpoints <= 0) {   // player wins
          // Console.WriteLine ("--! Player win, spells used: " + string.Join (", ",
          //     (spellsused ?? Array.Empty<Spell>()) .Append (spell) .Select (s => s.name)));
          if (spell.manacost < manabest)  manabest = spell.manacost;
          continue;   // no need to do recursive calls
        }
        // Both actors are still alive, recursively try the next spell
        int mananeed = TryDoubleTurn (newplayer, newboss, harddifficulty, depth + 1,
          (spellsused ?? Array.Empty<Spell>()) .Append (spell) .ToArray() );
        if (mananeed >= 0) {
          // Successful strategy, mana need = recursive need plus current spell
          mananeed += spell.manacost;
          if (mananeed < manabest)  manabest = mananeed;
        }
      }
    }
    // Return mana spent
    // Console.WriteLine ("[{0}] done, best mana: {1}", depth, manabest);
    return manabest < int.MaxValue ? manabest : -1;
  }


  /// <summary>Read actor attributes from file</summary>
  public static void ReadWizardStatsFromFile (string filename, ref WizardStats boss) {
    foreach (string line in File.ReadAllLines (filename)) {
      if (line.StartsWith ("Hit Points: "))
        boss.hitpoints = int.Parse (line.Substring (12));
      if (line.StartsWith ("Damage: "))
        boss.damage = int.Parse (line.Substring (8));
    }
  }


  public static readonly WizardStats ExamplePlayer =
    new WizardStats (hp: 10, mn: 250);
  public static readonly WizardStats Example1Boss =
    new WizardStats (hp: 13, mn: 250, arm: 0, dam: 8);
  public static readonly Spell[] Example1Strategy = new Spell[] {
    PoisonSpell, MagicMissileSpell };

  public static readonly WizardStats Example2Boss =
    new WizardStats (hp: 14, mn: 250, arm: 0, dam: 8);
  public static readonly Spell[] Example2Strategy = new Spell[] {
    RechargeSpell, ShieldSpell, DrainSpell, PoisonSpell, MagicMissileSpell };

  public static readonly WizardStats StartPlayer = new WizardStats (hp: 50, mn: 500);

  public static void Main () {
    Console.WriteLine ("--- Example 1 ---");
    WizardStats player = new WizardStats (ExamplePlayer);
    WizardStats boss = new WizardStats (Example1Boss);
    PlayScriptedBattle (Example1Strategy, ref player, ref boss);
    Console.WriteLine ();

    Console.WriteLine ("--- Example 2 ---");
    player = new WizardStats (ExamplePlayer);
    boss = new WizardStats (Example2Boss);
    Spell[] Example2Strategy = new Spell[] {
      RechargeSpell, ShieldSpell, DrainSpell, PoisonSpell, MagicMissileSpell };
    PlayScriptedBattle (Example2Strategy, ref player, ref boss);
    Console.WriteLine ();

    Console.WriteLine ("--- Example 1 Search least mana to win ---");
    player = new WizardStats (ExamplePlayer);
    boss = new WizardStats (Example1Boss);
    int manaspent = TryDoubleTurn (player, boss);
    Console.WriteLine ("* Spent Mana: {0}", manaspent);
    Console.WriteLine ();

    Console.WriteLine ("--- Example 2 Search least mana to win ---");
    player = new WizardStats (ExamplePlayer);
    boss = new WizardStats (Example2Boss);
    manaspent = TryDoubleTurn (player, boss);
    Console.WriteLine ("* Spent Mana: {0}", manaspent);
    Console.WriteLine ();

    Console.WriteLine ("--- Part 1: Least mana to win ---");
    WizardStats startboss = new WizardStats (100, 250, 0, 10);
    ReadWizardStatsFromFile ("22-wizard-simulator-20xx-input.txt", ref startboss);
    Console.WriteLine ("Player:  {0}", StartPlayer);
    Console.WriteLine ("Boss:    {0}", startboss);
    manaspent = TryDoubleTurn (StartPlayer, startboss, false, 0);
    Console.WriteLine ("* Spent Mana: {0}", manaspent);
    Console.WriteLine ();

    Console.WriteLine ("--- Part 1: Least mana to win on difficulty hard ---");
    Console.WriteLine ("Player:  {0}", StartPlayer);
    Console.WriteLine ("Boss:    {0}", startboss);
    manaspent = TryDoubleTurn (StartPlayer, startboss, true, 0);
    Console.WriteLine ("* Spent Mana: {0}", manaspent);
  }
}

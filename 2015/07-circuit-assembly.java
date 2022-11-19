// Advent of Code 2015, Day 7 Some Assembly Required
// https://adventofcode.com/2015/day/7

package karlgeorg.adventskal;

import java.util.Map;
import java.util.HashMap;
import java.util.List;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.io.IOException;

public class CircuitAssembly {

  final static String[] ExampleInstructions = new String[] {
    "123 -> x",
    "456 -> y",
    "x AND y -> d",
    "x OR y -> e",
    "x LSHIFT 2 -> f",
    "y RSHIFT 2 -> g",
    "NOT x -> h",
    "NOT y -> i"
  };

  /** Run a single instruction and return the new wire state */ 
  public static Map<String, Short> RunInstruction (
      Map<String, Short> currentState, Instruction instruction) {
    short value1 = 0, value2 = 0, resultvalue;
    Map<String, Short> newState = currentState;
    if (instruction.gatetype == "AND" || instruction.gatetype == "OR") {
      // Get two values from appropriate wires
      value1 = currentState.get(instruction.inputWire1);
      value2 = currentState.get(instruction.inputWire2);
    }
    else if (instruction.gatetype == "NOT" || instruction.gatetype == "COPY" ||
        instruction.gatetype == "LSHIFT" || instruction.gatetype == "RSHIFT" ||
        instruction.gatetype == "ANDY" || instruction.gatetype == "ORY") {
      // Get one value from the first wire
      value1 = currentState.get(instruction.inputWire1);
    }
    else if (instruction.gatetype == "ANDX" || instruction.gatetype == "ORX") {
      // Get one value from the second wire
      value2 = currentState.get(instruction.inputWire2);
    }
    else if (instruction.gatetype == "SET") {
      // Do not need any value from a wire
    }
    else {
      System.err.println ("RunInstruction: Unsupported gate type: " +
        instruction.gatetype);
    }
    // Let the gate calculate the output value and set the correct wire
    resultvalue = instruction.Apply (value1, value2);
    newState.put(instruction.outputWire, resultvalue);
    return newState;
  }

  /** Run the instructions until all wires are connected
   *  and return the resulting wire state */ 
  public static Map<String, Short> RunCircuit (Instruction[] instructions,
      boolean verbose) {
    Map<String, Short> valuesOnWires = new HashMap<String, Short>();
    int activeWires = 0;
    for (int step = 0; step < instructions.length; step++) {
      if (verbose)  System.out.println ("* Iteration step " + step);
      // Go through all instructions to find any connected ones
      for (int i = 0; i < instructions.length; i++) {
        Instruction instr = instructions[i];
        if (valuesOnWires.containsKey (instr.outputWire)) {
          // if (verbose)  System.out.println (
          //   "- Output already connected: " + instr.outputWire);
          continue;
        }
        // Find out whether inputs are available
        boolean need1 = false, need2 = false;
        if (instr.gatetype == "AND" || instr.gatetype == "OR") {
          // Need two wires
          need1 = true;
          need2 = true;
        }
        else if (instr.gatetype == "LSHIFT" || instr.gatetype == "RSHIFT" ||
            instr.gatetype == "NOT" || instr.gatetype == "COPY" ||
            instr.gatetype == "ANDY" || instr.gatetype == "ORY") {
          need1 = true;
        }
        else if (instr.gatetype == "ANDX" || instr.gatetype == "ORX") {
          need2 = true;
        }
        // if the necessary input values are already known use them to calculate
        // the new output value
        if ((!need1 || valuesOnWires.containsKey (instr.inputWire1)) &&
            (!need2 || valuesOnWires.containsKey (instr.inputWire2))) {
          // TODO combine getting the value with above tests whether they are present
          //      and get rid of RunInstruction() function completely?
          // a = GetWireValue (instructions, instr.inputWire1, verbose, recurDepth + 1);
          // b = GetWireValue (instructions, instr.inputWire2, verbose, recurDepth + 1);
          valuesOnWires = RunInstruction (valuesOnWires, instructions[i]);
          if (verbose)  System.out.println ("  - Connected new output: " +
            instr + " = " +
            Short.toUnsignedInt (valuesOnWires.get (instr.outputWire)));
        }
      } // end for (instructions)
      if (verbose)  System.out.println ("  -> active wires: previous: " +
        activeWires + ", " + "now: " + valuesOnWires.size());
      if (valuesOnWires.size() == activeWires) {
        if (verbose)
          System.out.println ("Cannot connect any more outputs, stopping");
        break;
      }
      // Remember active wires for next iteration
      activeWires = valuesOnWires.size();
    } // end for (iteration step)
    return valuesOnWires;
  }


  /** Parse a single instruction
   * @return the parsed instruction or null when parsing failed */
  public Instruction ParseInstruction (String instruction) {
    String[] parts = instruction.split ("->");
    // Get the input from the left part
    if (parts.length != 2) {
      System.err.println ("Failed to split instruction \"" +
        instruction + "\"at ->, expected 2 parts, but got " +
        Integer.toString(parts.length));
      return null;
    }
    Instruction instr = new Instruction ();
    String[] operands = new String[0];
    // Decide the gate type
    if (parts[0].contains ("AND")) {
      operands = parts[0].split("AND");
      if (Character.isLetter (operands[0].trim().charAt(0))) {
        if (Character.isLetter (operands[1].trim().charAt(0))) {
          instr.gatetype = "AND";
        } else if (Character.isDigit (operands[1].trim().charAt(0))) {
          instr.gatetype = "ANDY";
        } else {
          System.out.println ("Unsupported wiring AND (" +
            operands[0].trim() + ", " + operands[1].trim() + ")");
        }
      }
      else if (Character.isDigit (operands[0].trim().charAt(0))) {
        if (Character.isLetter (operands[1].trim().charAt(0))) {
          instr.gatetype = "ANDX";
        } else if (Character.isDigit (operands[1].trim().charAt(0))) {
          instr.gatetype = "ANDN";
        } else {
          System.out.println ("Unsupported wiring AND (" +
            operands[0].trim() + ", " + operands[1].trim() + ")");
        }
      }
      else {
        System.out.println ("Unsupported wiring AND (" +
          operands[0].trim() + ", " + operands[1].trim() + ")");
      }
    }
    else if (parts[0].contains ("OR")) {
      operands = parts[0].split("OR");
      if (Character.isLetter (operands[0].trim().charAt(0))) {
        if (Character.isLetter (operands[1].trim().charAt(0))) {
          instr.gatetype = "OR";
        } else if (Character.isDigit (operands[1].trim().charAt(0))) {
          instr.gatetype = "ORY";
        } else {
          System.out.println ("Unsupported wiring OR (" +
            operands[0].trim() + ", " + operands[1].trim() + ")");
        }
      }
      else if (Character.isDigit (operands[0].trim().charAt(0))) {
        if (Character.isLetter (operands[1].trim().charAt(0))) {
          instr.gatetype = "ORX";
        } else if (Character.isDigit (operands[1].trim().charAt(0))) {
          instr.gatetype = "ORN";
        } else {
          System.out.println ("Unsupported wiring OR (" +
            operands[0].trim() + ", " + operands[1].trim() + ")");
        }
      }
      else {
        System.out.println ("Unsupported wiring OR (" +
          operands[0].trim() + ", " + operands[1].trim() + ")");
      }
    }
    else if (parts[0].contains ("LSHIFT")) {
      operands = parts[0].split("LSHIFT");
      instr.gatetype = "LSHIFT";
    }
    else if (parts[0].contains ("RSHIFT")) {
      operands = parts[0].split("RSHIFT");
      instr.gatetype = "RSHIFT";
    }
    else if (parts[0].contains ("NOT")) {
      operands = parts[0].split("NOT");
      instr.gatetype = "NOT";
    }
    else {
      if (Character.isLetter (parts[0].trim().charAt(0))) {
        operands = parts[0].split("no-present");
        instr.gatetype = "COPY";
      }
      else if (Character.isDigit (parts[0].trim().charAt(0))) {
        operands = parts[0].split("no-present");
        instr.gatetype = "SET";
      }
    }
    // Read operands: wiire identifiers or numbers
    if (instr.gatetype == "AND" || instr.gatetype == "OR") {
      // - Use two wire identifiers, one before and one after the gate type
      instr.inputWire1 = operands[0].trim();
      instr.inputWire2 = operands[1].trim();
    }
    else if (instr.gatetype == "LSHIFT" || instr.gatetype == "RSHIFT" ||
      instr.gatetype == "ANDY" || instr.gatetype == "ORY") {
      // - Use one wire identifier before the gate type and a number after it
      instr.inputWire1 = operands[0].trim();
      instr.operand = (short) Integer.parseInt (operands[1].trim());
    }
    else if (instr.gatetype == "ANDX" || instr.gatetype == "ORX") {
      // - Use one number before and one wire identifier after the gate type
      instr.operand = (short) Integer.parseInt (operands[0].trim());
      instr.inputWire2 = operands[1].trim();
    }
    else if (instr.gatetype == "NOT") {
      // - Use one wire identifier after the gate type
      instr.inputWire1 = operands[1].trim();
    }
    else if (instr.gatetype == "SET") {
      // - Use a single integer as operand
      instr.operand = (short) Integer.parseInt (parts[0].trim());
    }
    else if (instr.gatetype == "COPY") {
      // - Use one wire identifier to copy from
      instr.inputWire1 = operands[0].trim();
    }
    else {
      System.out.println ("Unsupported Gate: " + instr.gatetype);
    }
    // Set output wire from the right part
    instr.outputWire = parts[1].trim();
    // Return the assembled instruction
    return instr;
  }

  /** Parse an array of instructions */
  public Instruction[] ParseInstructions (String[] instructions) {
    Instruction[] instarray = new Instruction[instructions.length];
    for (int i = 0; i < instructions.length; i++) {
      instarray[i] = ParseInstruction (instructions[i]);
    }
    return instarray;
  }

  public static void printwires (Map<String, Short> wires) {
    for (String k: wires.keySet())
      System.out.println (k + ": " + Short.toUnsignedInt(wires.get(k)));
  }


  public static void main (String[] args) {

    System.out.println ("--- Beispiel ---");
    CircuitAssembly ca = new CircuitAssembly ();
    Instruction[] instrarr = ca.ParseInstructions (ExampleInstructions);
    Map<String, Short> wirevalues = RunCircuit (instrarr, true);
    System.out.println ("Wire values after running the circuit:");
    printwires (wirevalues);
    System.out.println ();

    System.out.println ("--- Aufgabe 1 ---");
    String filename = "07-circuit-assembly-input.txt";
    List<String> manual = null;
    try {
      manual = Files.readAllLines (Paths.get (filename));
    }
    catch (IOException ioex) {
      System.err.println ("Failed to read file \"" + filename + "\": " + ioex);
      System.exit (1);
    }
    instrarr = ca.ParseInstructions (manual.toArray(new String[0]));
    System.out.println ("Read file \"" + filename +
      "\", number of instructions: " + instrarr.length);
    wirevalues = RunCircuit (instrarr, false);
    System.out.println ("Wire values after running the circuit:");
    printwires (wirevalues);
    short ergebnis = wirevalues.get ("a");
    System.out.println ("*** Ergebnis:  Wire \"a\" has value: " +
      ergebnis + " ***");
  }


  /** Data type to describe one instruction, the "wiring of the gate" */
  class Instruction {
    public String gatetype;
    public String inputWire1;
    public String inputWire2;
    public String outputWire;
    public Short operand;
    public Instruction () {
      gatetype = "NOP";
      inputWire1 = "-";
      inputWire2 = "-";
      outputWire = "-";
      operand = 0;
    }

    /*
    protected void ReadTwoInputWiresFromString (String gatetype, String input) {
      String[] operands = input.split (gatetype.trim());
      inputWire1 = operands[0].trim().charAt(0);
      inputWire2 = operands[1].trim().charAt(0);
    }
    */

    public short Apply (short op1, short op2) {
      switch (gatetype) {
        case "AND":  return (short)(op1 & op2);
        case "ANDX":  return (short)(operand & op2);
        case "ANDY":  return (short)(op1 & operand);
        case "OR":   return (short)(op1 | op2);
        case "ORX":   return (short)(operand | op2);
        case "ORY":   return (short)(op1 | operand);
        case "LSHIFT":  return (short)(op1 << operand);
        case "RSHIFT":  return (short)(op1 >> operand);
        case "NOT":  return (short)~op1;
        case "SET":  return operand;
        case "COPY":  return op1;
        default:
          System.err.println ("Instruction.Apply: Unsupported gate type: " +
            gatetype);
      }
      return 0;
    }

    @Override
    public String toString() {
      return super.toString() + "  " + gatetype + " (" +
        inputWire1 + ", " + inputWire2 + ", " + operand + " -> " +
        outputWire + ")";
    }
  }
}

// Advent of Code 2015, Day 23 Opening the Turing Lock
// https://adventofcode.com/2015/day/23

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>


// Forward declaration needed by the InstructionFunc typedef
struct CpuState_st;

/// \brief Function type for a CPU instruction that modifies the CPU state
typedef void InstructionFunc (struct CpuState_st *cpustate, signed int operand);

/// \brief Type for a CPU instruction in a program
typedef struct Instruction_st {
  //Opcode        op;
  InstructionFunc *opfunc;   ///< Pointer to the function handling the instruction
  signed int    arg;         ///< Argument, may or may not be used by the instruction
} Instruction;

typedef struct CpuState_st {
  unsigned int  a;
  unsigned int  b;
  /// Instruction pointer = index of next instruction to execute
  unsigned int  ip;
  /// Number of instructions
  unsigned int  numinstr;
  /// Instructions ("code segment"?)
  Instruction *instructions;
} CpuState;


// Arithmetic instructions
void HalfA (CpuState *cpustate, signed int) { cpustate->a /= 2; cpustate->ip++; }
void HalfB (CpuState *cpustate, signed int) { cpustate->b /= 2; cpustate->ip++; }
void TripleA (CpuState *cpustate, signed int) { cpustate->a *= 3; cpustate->ip++; }
void TripleB (CpuState *cpustate, signed int) { cpustate->b *= 3; cpustate->ip++; }
void IncrementA (CpuState *cpustate, signed int) { cpustate->a++; cpustate->ip++; }
void IncrementB (CpuState *cpustate, signed int) { cpustate->b++; cpustate->ip++; }

// Jumps
void Jump (CpuState *cpustate, signed int offset) { cpustate->ip += offset; }
void JumpIfEvenA (CpuState *cpustate, signed int offset) {
  cpustate->ip += ((cpustate->a & 0x1) == 0) ? offset : 1; }
void JumpIfEvenB (CpuState *cpustate, signed int offset) {
  cpustate->ip += ((cpustate->b & 0x1) == 0) ? offset : 1; }
void JumpIfOneA (CpuState *cpustate, signed int offset) {
  cpustate->ip += (cpustate->a == 1) ? offset : 1; }
void JumpIfOneB (CpuState *cpustate, signed int offset) {
  cpustate->ip += (cpustate->b == 1) ? offset : 1; }


/// \brief Return the instruction name for an instruction (for listings)
const char *InstructionText (const Instruction *inst) {
  if (inst->opfunc == IncrementA)  return "inc a";
  if (inst->opfunc == IncrementB)  return "inc b";
  if (inst->opfunc == HalfA)  return "hlf a";
  if (inst->opfunc == HalfB)  return "hlf b";
  if (inst->opfunc == TripleA)  return "tpl a";
  if (inst->opfunc == TripleA)  return "tpl b";
  if (inst->opfunc == Jump)  return "jmp";
  if (inst->opfunc == JumpIfEvenA)  return "jie a,";
  if (inst->opfunc == JumpIfEvenB)  return "jie b,";
  if (inst->opfunc == JumpIfOneA)  return "jio a,";
  if (inst->opfunc == JumpIfOneA)  return "jio b,";
  return "unk";
}


/// \brief Parse a text line to get the appropriate handler function and argument
Instruction ParseInstruction (const char *line) {
  Instruction instr;  instr.opfunc = NULL;  instr.arg = 0;
  if      (strcmp (line, "hlf a") == 0)  instr.opfunc = HalfA;
  else if (strcmp (line, "hlf b") == 0 )  instr.opfunc = HalfB;
  else if (strcmp (line, "tpl a") == 0 )  instr.opfunc = TripleA;
  else if (strcmp (line, "tpl b") == 0 )  instr.opfunc = TripleB;
  else if (strcmp (line, "inc a") == 0 )  instr.opfunc = IncrementA;
  else if (strcmp (line, "inc b") == 0 )  instr.opfunc = IncrementB;
  else if (strncmp (line, "jmp ", 4) == 0 ) {
    instr.opfunc = Jump;  instr.arg = strtol (&line[4], NULL, 0); }
  else if (strncmp (line, "jie a, ", 7) == 0 ) {
    instr.opfunc = JumpIfEvenA;  instr.arg = strtol (&line[7], NULL, 0); }
  else if (strncmp (line, "jie b, ", 7) == 0 ) {
    instr.opfunc = JumpIfEvenB;  instr.arg = strtol (&line[7], NULL, 0); }
  else if (strncmp (line, "jio a, ", 7) == 0 ) {
    instr.opfunc = JumpIfOneA;  instr.arg = strtol (&line[7], NULL, 0); }
  else if (strncmp (line, "jio b, ", 7) == 0 ) {
    instr.opfunc = JumpIfOneB;  instr.arg = strtol (&line[7], NULL, 0); }
  else  fprintf (stderr, "ERROR: Invalid operation: %s\n", line);
  return instr;
}


void RunProgramme (CpuState *cpustate, bool trace) {
  unsigned int cpucycles = 0;   // Count how many instructions are executed
  printf ("Running programme with %u instructions\n", cpustate->numinstr);
  while (cpustate->ip < cpustate->numinstr) {
    Instruction *instr = &cpustate->instructions[cpustate->ip];
    if (trace)  printf ("[cycle %5d]  ip = %3d   %-8s %3d   a = %5d, b = %5d\n",
      cpucycles, cpustate->ip,
      InstructionText (instr), instr->arg, cpustate->a, cpustate->b);
    instr->opfunc (cpustate, instr->arg);
    cpucycles++;
    // printf ("[%04d]  ip = %04d,  a = %5d,  b = %05d\n",
    //   cpucycles++, cpustate->ip, cpustate->a, cpustate->b);
  }
  printf ("Programme done, executed %u instructions\n", cpucycles);
}


/// \brief Read instructions from an input file
/// \param filename  Input filename to read
/// \param maxinstructions  Maximum number of instructions to read
/// \param Pointer to memory for maxinstructions instructions
/// \return Number of instructions read
//
unsigned int ReadInstructionsFromFile (const char *filename,
    unsigned int maxinstructions, Instruction *instructions) {
  FILE *inputfile = fopen (filename, "rt");
  if (inputfile == NULL)  return 0;
  int numlines = 0;
  while (feof (inputfile) == 0 && numlines < maxinstructions) {
    size_t linelen = 0;
    char *s = NULL;   // getline will allocate memory
    ssize_t nread = getline (&s, &linelen, inputfile);   // includes \n
    if (nread > 0) {
      char *c = strchr (s, '\0');
      while (c > s && isspace (*(c-1)))  *(--c) = '\0';   // strip trailing space
      instructions[numlines++] = ParseInstruction (s);
    }
    if (s != NULL)  free (s);
    if (nread <= 0 || feof (inputfile) != 0)  break;   // end of file or error
  }
  fclose (inputfile);
  return numlines;
}


/// \brief Output a programme listing
void PrintProgramme (unsigned int numinstructions, const Instruction *instructions) {
  for (int i = 0; i < numinstructions; i++) {
    printf ("%3d   %p  %-4s  %5d\n", i, instructions[i].opfunc,
      InstructionText (&instructions[i]), instructions[i].arg);
  }
}


int main (int argc, char *argv[]) {
  Instruction example[4] = {
    { IncrementA, 0 }, { JumpIfOneA, 2 }, { TripleA, 0 }, { IncrementA, 0 }
  };
  unsigned int examplesize = sizeof(example) / sizeof(example[0]);
  printf ("--- Example ---\n");
  PrintProgramme (examplesize, example);
  CpuState examplestate = { 0, 0, 0, examplesize, example };
  RunProgramme (&examplestate, true);
  printf ("End state:  ip = %4u,  a = %5u,  b = %5u\n",
    examplestate.ip, examplestate.a, examplestate.b);
  printf ("\n");

  printf ("--- Part 1 ---\n");
  const unsigned int maxcodesize = 1024;
  Instruction programme[maxcodesize];
  const char *inputfilename = "23-opening-the-turing-lock-input.txt";
  unsigned int programmesize =
    ReadInstructionsFromFile (inputfilename, maxcodesize, programme);
  printf ("File: %s, instructions read: %u\n", inputfilename, programmesize);
  // PrintProgramme (programmesize, programme);
  CpuState cpustate = { 0, 0, 0, programmesize, programme };
  RunProgramme (&cpustate, false);
  printf ("End state:  ip = %4u,  a = %5u,  b = %5u\n",
    cpustate.ip, cpustate.a, cpustate.b);
}

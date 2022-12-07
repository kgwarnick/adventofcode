HELPER_OBJ := fileread.o strsplit.o freadlns.o

PUZZLE_OBJ := 01-calorie-counting.o 02-rock-paper-scissors.o \
  03-rucksack-reorganization.o 04-camp-cleanup.o \
  05-supply-stacks.o 06-tuning-trouble.o \
  07-no-space-left-on-device.o

# Extension for binaries, empty for Linux binaries
BINEXT :=

PUZZLE_BIN := $(PUZZLE_OBJ:.o=)

all : $(PUZZLE_BIN)

clean :
	$(RM) $(HELPER_OBJ) $(PUZZLE_OBJ)
	$(RM) $(PUZZLE_BIN)

CFLAGS   = -g
CXXFLAGS = -g
LINK_CXX = $(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

01-calorie-counting$(BINEXT) : %$(BINEXT) : %.o strsplit.o fileread.o
	$(LINK_CXX)

02-rock-paper-scissors$(BINEXT) : %$(BINEXT) : %.o

03-rucksack-reorganization : %$(BINEXT) : %.o fileread.o
	$(LINK_CXX)

04-camp-cleanup$(BINEXT) : %$(BINEXT) : %.o freadlns.o

05-supply-stacks$(BINEXT) : %$(BINEXT) : %.o fileread.o
	$(LINK_CXX)

06-tuning-trouble$(BINEXT) : %$(BINEXT) : %.o

07-no-space-left-on-device$(BINEXT) : %$(BINEXT) : %.o fileread.o
	$(LINK_CXX)

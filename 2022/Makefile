HELPER_OBJ := fileread.o strsplit.o

PUZZLE_OBJ := 01-calorie-counting.o

# Extension for binaries, empty for Linux binaries
BINEXT :=

PUZZLE_BIN := $(PUZZLE_OBJ:.o=)

all : $(PUZZLE_BIN)

clean :
	$(RM) $(HELPER_OBJ) $(PUZZLE_OBJ)
	$(RM) $(PUZZLE_BIN)

CXXFLAGS += -g
LINK_CXX = $(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

01-calorie-counting$(BINEXT) : %$(BINEXT) : %.o strsplit.o fileread.o
	$(LINK_CXX)

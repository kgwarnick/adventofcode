# Extension for object files, typically .o on Linux, .obj on Windows
OBJEXT = .o
# Extension for binaries, empty for Linux binaries
BINEXT =

HELPER_OBJ = fileread$(OBJEXT) strsplit$(OBJEXT) freadlns$(OBJEXT) trimendl$(OBJEXT)

PUZZLE_OBJ = 01-calorie-counting.o 02-rock-paper-scissors.o \
  03-rucksack-reorganization.o 04-camp-cleanup.o \
  05-supply-stacks.o 06-tuning-trouble.o \
  07-no-space-left-on-device.o 08-treetop-tree-house.o \
  09-rope-bridge$(OBJEXT) 10-cathode-ray-tube$(OBJEXT) \
  11-monkey-in-the-middle$(OBJEXT) 12-hill-climbing-algorithm$(OBJEXT) \
  13-distress-lists$(OBJEXT) 14-regolith-reservoir$(OBJEXT) \
  15-beacon-exclusion-zone$(OBJEXT) 16-proboscidea-volcanium$(OBJEXT) \
  17-pyroclastic-flow$(OBJEXT) 18-boiling-boulders$(OBJEXT) \
  19-not-enough-minerals$(OBJEXT) 20-grove-positioning-system$(OBJEXT) \
  21-monkey-math$(OBJEXT) 22-monkey-map$(OBJEXT) \
  23-unstable-diffusion$(OBJEXT) 24-blizzard-basin$(OBJEXT) \
  25-full-of-hot-air$(OBJEXT)

PUZZLE_BIN = $(PUZZLE_OBJ:$(OBJEXT)=$(BINEXT))

all : $(PUZZLE_BIN)

clean :
	$(RM) $(HELPER_OBJ) $(PUZZLE_OBJ)
	$(RM) $(PUZZLE_BIN)

CFLAGS   = -g -Wall -Wextra
CXXFLAGS = -g -Wall -Wextra
LINK_CXX = $(CXX) $(CXXFLAGS) $(LDFLAGS) $^ -o $@

VALGRIND = valgrind
VALGRIND_FLAGS = --error-exitcode=16 --leak-check=full --track-origins=yes
LEAKCHECKS = $(addprefix leakcheck-,$(PUZZLE_OBJ:$(OBJEXT)=))

leakcheck-% : %$(BINEXT)
	$(VALGRIND) $(VALGRIND_FLAGS) ./$< > /dev/null

leakcheck : $(LEAKCHECKS)

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

08-treetop-tree-house$(BINEXT) : %$(BINEXT) : %.o freadlns.o

09-rope-bridge$(BINEXT) : %$(BINEXT) : %$(OBJEXT) fileread$(OBJEXT)
	$(LINK_CXX)

10-cathode-ray-tube$(BINEXT): %$(BINEXT) : %$(OBJEXT) freadlns$(OBJEXT)

11-monkey-in-the-middle$(BINEXT) : %$(BINEXT) : %$(OBJEXT) fileread$(OBJEXT)
	$(LINK_CXX)

12-hill-climbing-algorithm$(BINEXT) : %$(BINEXT) : %$(OBJEXT) freadlns$(OBJEXT) trimendl$(OBJEXT)

13-distress-lists$(BINEXT) : %$(BINEXT) : %$(OBJEXT) fileread$(OBJEXT)
	$(LINK_CXX)

14-regolith-reservoir$(BINEXT) : %$(BINEXT) : %$(OBJEXT) freadlns$(OBJEXT)

15-beacon-exclusion-zone$(BINEXT) : %$(BINEXT) : %$(OBJEXT) strsplit$(OBJEXT) fileread$(OBJEXT)
	$(LINK_CXX)

16-proboscidea-volcanium$(BINEXT) : %$(BINEXT) : %$(OBJEXT) freadlns$(OBJEXT)

17-pyroclastic-flow$(BINEXT) : %$(BINEXT) : %$(OBJEXT) fileread$(OBJEXT)
	$(LINK_CXX)

18-boiling-boulders$(BINEXT) : %$(BINEXT) : %$(OBJEXT) freadlns$(OBJEXT)

19-not-enough-minerals$(BINEXT) : %$(BINEXT) : %$(OBJEXT) fileread$(OBJEXT)
	$(LINK_CXX)

20-grove-positioning-system$(BINEXT) : %$(BINEXT) : %$(OBJEXT) freadlns$(OBJEXT)

21-monkey-math$(BINEXT) : %$(BINEXT) : %$(OBJEXT) fileread$(OBJEXT)
	$(LINK_CXX)

22-monkey-map$(BINEXT) : %$(BINEXT) : %$(OBJEXT) freadlns$(OBJEXT)

23-unstable-diffusion$(BINEXT) : %$(BINEXT) : %$(OBJEXT) fileread$(OBJEXT)
	$(LINK_CXX)

24-blizzard-basin$(BINEXT) : %$(BINEXT) : %$(OBJEXT) freadlns$(OBJEXT) trimendl$(OBJEXT)

25-full-of-hot-air$(BINEXT) : %$(BINEXT) : %$(OBJEXT) fileread$(OBJEXT)
	$(LINK_CXX)

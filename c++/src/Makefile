PR_DIR = ..
BIN_DIR = $(PR_DIR)/bin
SRC_DIR = $(PR_DIR)/src
ALG_DIR = $(SRC_DIR)/algorithms
COR_DIR = $(SRC_DIR)/core
NUM_DIR = $(COR_DIR)/numeric

OPT = -O0
FLAGS = $(OPT)

LINKERS = -lgmp

NUMERIC			= $(BIN_DIR)/integer.o \
				  $(BIN_DIR)/rational.o
ALGORITHMS		= $(BIN_DIR)/nested_sums.o
DATA_STRUCTURES = $(BIN_DIR)/sparse_pascal_triangle.o \
				  $(BIN_DIR)/dense_pascal_triangle.o

OBJECTS = $(BIN_DIR)/tests.o \
		  $(NUMERIC) \
		  $(NESTED_SUMS) \
		  $(DATA_STRUCTURES)

tests: $(OBJECTS)
	g++ $(FLAGS) -o tests $(OBJECTS) $(LINKERS)

include Makefile.rules

clean:
	rm -f $(OBJECTS)

distclean:
	make clean
	rm -f tests

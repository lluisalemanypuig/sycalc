PR_DIR = ..
BIN_DIR = $(PR_DIR)/bin
SRC_DIR = $(PR_DIR)/src
ALG_DIR = $(SRC_DIR)/algorithms
COR_DIR = $(SRC_DIR)/core
MSC_DIR = $(SRC_DIR)/misc
NUM_DIR = $(COR_DIR)/numeric

OPT = -O0
FLAGS = $(OPT)

LINKERS = -lgmp

# -- CORE

NUMERIC			= $(BIN_DIR)/integer.o \
				  $(BIN_DIR)/rational.o \
				  $(BIN_DIR)/gmp_utils.o

DATA_STRUCTURES = $(BIN_DIR)/sparse_pascal_triangle.o \
				  $(BIN_DIR)/dense_pascal_triangle.o

SYMBOLIC		= $(BIN_DIR)/monomial.o \
				  $(BIN_DIR)/polynomial.o

# -- ALGORITHMS

ALGORITHMS		= $(BIN_DIR)/nested_sums.o

# EVERYTHING
OBJECTS 		= $(BIN_DIR)/tests.o \
				  $(NUMERIC) \
				  $(DATA_STRUCTURES) \
				  $(SYMBOLIC) \
				  $(ALGORITHMS)

tests: $(OBJECTS)
	g++ $(FLAGS) -o tests $(OBJECTS) $(LINKERS)

include Makefile.rules

clean:
	rm -f $(OBJECTS)

distclean:
	make clean
	rm -f tests

PR_DIR = ..
BIN_DIR = $(PR_DIR)/bin
SRC_DIR = $(PR_DIR)/src
ALG_DIR = $(SRC_DIR)/algorithms
COR_DIR = $(SRC_DIR)/core
MSC_DIR = $(SRC_DIR)/misc
TST_DIR = $(SRC_DIR)/tests
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

ALGORITHMS		= $(BIN_DIR)/nested_sums.o $(BIN_DIR)/power_sums.o

# -- TESTS

TESTS			= $(BIN_DIR)/triangle_tests.o \
				  $(BIN_DIR)/integer_tests.o \
				  $(BIN_DIR)/rational_tests.o \
				  $(BIN_DIR)/monomial_tests.o \
				  $(BIN_DIR)/polynomial_tests.o \
				  $(BIN_DIR)/algorithms_tests.o

# EVERYTHING
OBJECTS 		= $(BIN_DIR)/main.o \
				  $(NUMERIC) \
				  $(DATA_STRUCTURES) \
				  $(SYMBOLIC) \
				  $(ALGORITHMS) \
				  $(TESTS)

main: $(OBJECTS)
	g++ $(FLAGS) -o main $(OBJECTS) $(LINKERS)

include Makefile.rules

clean:
	rm -f $(OBJECTS)

distclean:
	make clean
	rm -f main

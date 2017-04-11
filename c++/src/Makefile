PR_DIR = ..
BIN_DIR = $(PR_DIR)/bin
SRC_DIR = $(PR_DIR)/src
ALG_DIR = $(SRC_DIR)/algorithms
COR_DIR = $(SRC_DIR)/core
NUM_DIR = $(COR_DIR)/numeric

OPT = -O0
FLAGS = $(OPT)

LINKERS = -lgmp

OBJECTS = $(BIN_DIR)/main.o \
		  $(BIN_DIR)/integer.o $(BIN_DIR)/rational.o \
		  $(BIN_DIR)/nested_sums.o \
		  $(BIN_DIR)/sparse_pascal_triangle.o \
		  $(BIN_DIR)/dense_pascal_triangle.o

main: $(OBJECTS)
	g++ $(FLAGS) -o main $(OBJECTS) $(LINKERS)

include Makefile.rules

clean:
	rm -f $(OBJECTS)

distclean:
	make clean
	rm -f main

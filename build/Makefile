# DIRECTORIES

PROJ_DIR = ..
SRC_DIR = $(PROJ_DIR)/src
BIN_DIR = $(PROJ_DIR)/bin
ALG_DIR = $(SRC_DIR)/algorithms

# COMPILATION FLAGS

OPT = -O3
FLAGS = $(OPT) --goal=main --stand_alone=true

# FILES FOR COMPILING

EXE_FILES = $(SRC_DIR)/sycalc.pl $(SRC_DIR)/debug_sycalc.pl

NUMBER = $(SRC_DIR)/core/number/natural.pl $(SRC_DIR)/core/number/integer.pl $(SRC_DIR)/core/number/fraction.pl \
		 $(SRC_DIR)/core/number/rational.pl $(SRC_DIR)/core/number/irrational.pl $(SRC_DIR)/core/number/real.pl \
		 $(SRC_DIR)/core/number/arithmetic_evaluation.pl \
		 $(SRC_DIR)/core/number.pl

LISTS = $(SRC_DIR)/core/list/list.pl \
		$(SRC_DIR)/core/list.pl

POLYNOMIALS	= $(SRC_DIR)/core/monomial.pl $(SRC_DIR)/core/monomial_evaluation.pl \
			  $(SRC_DIR)/core/polynomial.pl $(SRC_DIR)/core/polynomial_evaluation.pl \
			  $(SRC_DIR)/core/polynomials.pl

CORE = $(LISTS) \
	   $(NUMBER) \
	   $(POLYNOMIALS) \
	   $(SRC_DIR)/core.pl

ALGORITHMS = $(ALG_DIR)/power_sums.pl

ALL_FILES = $(CORE) $(ALGORITHMS)

# BUILD BIN DIRECTORY

.PHONY: bindirectory
bindirectory: $(BIN_DIR)
$(BIN_DIR):
	mkdir -p $(BIN_DIR)

# COMPILATION RULES

all: release debug
release: $(BIN_DIR)/sycalc
debug: $(BIN_DIR)/debug_sycalc

$(BIN_DIR)/sycalc: $(ALL_FILES) $(SRC_DIR)/sycalc.pl
	swipl $(FLAGS) -o $(BIN_DIR)/sycalc -c $(SRC_DIR)/sycalc.pl

$(BIN_DIR)/debug_sycalc: $(ALL_FILES) $(SRC_DIR)/debug_sycalc.pl
	swipl $(FLAGS) -o $(BIN_DIR)/debug_sycalc -c $(SRC_DIR)/debug_sycalc.pl

clean:
	rm -f $(BIN_DIR)/sycalc
	rm -f $(BIN_DIR)/debug_sycalc
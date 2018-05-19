# DIRECTORIES

PROJ_DIR = ..
SRC_DIR = $(PROJ_DIR)/src
DEB_DIR = $(PROJ_DIR)/debug
BIN_DIR = $(PROJ_DIR)/bin
ALG_DIR = $(SRC_DIR)/algorithms

# COMPILATION FLAGS

OPT = -O3
FLAGS = $(OPT) --goal=main --stand_alone=true

# FILES FOR COMPILING

NUMBER =												\
	$(SRC_DIR)/core/number/natural.pl					\
	$(SRC_DIR)/core/number/integer.pl					\
	$(SRC_DIR)/core/number/fraction.pl					\
	$(SRC_DIR)/core/number/rational.pl					\
	$(SRC_DIR)/core/number/irrational.pl				\
	$(SRC_DIR)/core/number/real.pl						\
	$(SRC_DIR)/core/number/arithmetic_evaluation.pl		\
	$(SRC_DIR)/core/number.pl

LISTS =								\
	$(SRC_DIR)/core/list/list.pl 	\
	$(SRC_DIR)/core/list.pl

POLYNOMIALS	= 																\
	$(SRC_DIR)/core/monomial.pl $(SRC_DIR)/core/monomial_evaluation.pl		\
	$(SRC_DIR)/core/polynomial.pl $(SRC_DIR)/core/polynomial_evaluation.pl	\
	$(SRC_DIR)/core/polynomials.pl

CORE =					\
	$(SRC_DIR)/core.pl	\
	$(LISTS)			\
	$(NUMBER)			\
	$(POLYNOMIALS)

ALGORITHMS = 									\
	$(SRC_DIR)/algorithms.pl					\
	$(ALG_DIR)/power_sums.pl					\
	$(ALG_DIR)/poly_expr_eval.pl

ALL_FILES = $(CORE) $(ALGORITHMS)

# RULES

.PHONY: bindirectory
bindirectory: $(BIN_DIR)
$(BIN_DIR):
	mkdir -p $(BIN_DIR)

all: release debug

include Makefile.release
include Makefile.debug

clean:
	rm -f $(BIN_DIR)/sycalc
	rm -f $(BIN_DIR)/debug_sycalc

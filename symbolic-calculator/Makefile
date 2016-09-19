
EXE_FILES = sycalc.pl debug_sycalc.pl

ALL_FILES = lists.pl integer_algorithms.pl numbers.pl arithmetic_evaluation.pl \
			polynomials.pl polynomial_evaluation.pl \
			monomials.pl monomial_evaluation.pl

OBJECTS = lists.o integer_algorithms.o numbers.o arithmetic_evaluation.o \
		  polynomials.o polynomial_evaluation.o \
		  monomials.o monomial_evaluation.o

OPT = -O7
FLAGS = $(OPT)

all: sycalc debug_sycalc

sycalc: $(ALL_FILES) sycalc.pl
	swipl --goal=main --stand_alone=true $(FLAGS) -o sycalc -c sycalc.pl

debug_sycalc: $(ALL_FILES) debug_sycalc.pl
	swipl --goal=main --stand_alone=true $(FLAGS) -o debug_sycalc -c debug_sycalc.pl

clean:
	rm -f sycalc
	rm -f debug_sycalc

EXE_FILES = sycalc.pl debug_sycalc.pl

ALL_FILES = lists.pl integer_algorithms.pl numbers.pl arithmetic_evaluation.pl \
			polynomials.pl polynomial_evaluation.pl \
			monomials.pl monomial_evaluation.pl

OBJECTS = lists.o integer_algorithms.o numbers.o arithmetic_evaluation.o \
		  polynomials.o polynomial_evaluation.o \
		  monomials.o monomial_evaluation.o

ALL_FILES = lists.pl \
			number/natural.pl number/integer.pl number/fraction.pl number/irrational.pl \
			number/rational.pl number/real.pl number.pl \
			arithmetic_evaluation.pl \
			monomial.pl monomial_evaluation.pl \
			polynomial.pl polynomial_evaluation.pl

NUMBER = number/natural.pl number/integer.pl number/fraction.pl \
		 number/rational.pl number/irrational.pl \
		 number/real.pl \
		 number.pl

LISTS = lists/lists.pl \
		lists.pl

ALL_FILES = $(LISTS) \
			$(NUMBER) \
			arithmetic_evaluation.pl \
			monomial.pl monomial_evaluation.pl \
			polynomial.pl polynomial_evaluation.pl

OPT = -O0
FLAGS = $(OPT)

all: release debug
release: sycalc
debug: debug_sycalc

sycalc: $(ALL_FILES) sycalc.pl
	swipl --goal=main --stand_alone=true $(FLAGS) -o sycalc -c sycalc.pl

debug_sycalc: $(ALL_FILES) debug_sycalc.pl
	swipl --goal=main --stand_alone=true $(FLAGS) -o debug_sycalc -c debug_sycalc.pl

clean:
	rm -f sycalc
	rm -f debug_sycalc

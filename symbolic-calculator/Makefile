EXE_FILES = sycalc.pl debug_sycalc.pl

NUMBER = core/number/natural.pl core/number/integer.pl core/number/fraction.pl \
		 core/number/rational.pl core/number/irrational.pl core/number/real.pl \
		 core/number/arithmetic_evaluation.pl \
		 core/number.pl

LISTS = core/list/list.pl \
		core/list.pl

POLYNOMIALS	= core/monomial.pl core/monomial_evaluation.pl \
			  core/polynomial.pl core/polynomial_evaluation.pl \
			  core/polynomials.pl

CORE = $(LISTS) \
	   $(NUMBER) \
	   $(POLYNOMIALS) \
	   core.pl

ALL_FILES = $(CORE) \
			power_sums.pl

OPT = -O7
FLAGS = $(OPT)

release: sycalc
debug: debug_sycalc

sycalc: $(ALL_FILES) sycalc.pl
	swipl --goal=main --stand_alone=true $(FLAGS) -o sycalc -c sycalc.pl

debug_sycalc: $(ALL_FILES) debug_sycalc.pl
	swipl --goal=main --stand_alone=true $(FLAGS) -o debug_sycalc -c debug_sycalc.pl

clean:
	rm -f sycalc
	rm -f debug_sycalc

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
FLAGS = $(OPT) --goal=main --stand_alone=true

debug: isycalc idebug_sycalc

isycalc: $(ALL_FILES) sycalc.pl
	swipl $(FLAGS) -o isycalc -c sycalc.pl

idebug_sycalc: $(ALL_FILES) debug_sycalc.pl
	swipl $(FLAGS) -o idebug_sycalc -c debug_sycalc.pl

clean:
	rm -f isycalc idebug_sycalc

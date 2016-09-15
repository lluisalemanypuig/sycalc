
ALL_FILES = main.pl load.pl \
			lists.pl numerical_algorithms.pl numbers.pl arithmetic_evaluation.pl \
			polynomials.pl polynomial_evaluation.pl \
			monomials.pl monomial_evaluation.pl \
			debug.pl

OBJECTS = main.o load.o \
		  lists.o numerical_algorithms.o numbers.o arithmetic_evaluation.o \
		  polynomials.o polynomial_evaluation.o \
		  monomials.o monomial_evaluation.o \
		  debug.o

OPT = -O0
FLAGS = $(OPT)

TARGET = sycalc
all: $(TARGET)

$(TARGET): $(ALL_FILES)
	swipl --goal=main --stand_alone=true $(FLAGS) -o $(TARGET) -c load.pl

clean:
	rm -f $(TARGET)
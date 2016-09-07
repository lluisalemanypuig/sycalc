
ALL_FILES = main.pl load.pl \
			lists.pl numerical_algorithms.pl numbers.pl arithmetic_evaluation.pl \
			polynomials.pl monomials.pl \
			debug.pl


OPT = -O0
FLAGS = $(OPT)

TARGET = sycalc
all: $(TARGET)

$(TARGET): $(ALL_FILES)
	swipl --goal=main --stand_alone=true $(FLAGS) -o $(TARGET) -c load.pl

clean:
	rm -f $(TARGET)
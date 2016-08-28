
ALL_FILES = main.pl load.pl \
			lists.pl numeric.pl \
			polynomials.pl monomials.pl

OPT = -O0
FLAGS = $(OPT)

TARGET = sycalc
all: $(TARGET)

$(TARGET): $(ALL_FILES)
	swipl --goal=main --stand_alone=true $(FLAGS) -o $(TARGET) -c load.pl

clean:
	rm -f $(TARGET)
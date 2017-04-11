/// C includes
#include <gmp.h>

/// C++ includes
#include <iostream>
using namespace std;

/// Custom includes
#include "core/numeric/rational.hpp"
#include "core/numeric/integer.hpp"

#include "core/dense_pascal_triangle.hpp"
#include "core/sparse_pascal_triangle.hpp"
#include "algorithms.hpp"

size_t partial_sum(size_t a, size_t b, size_t k) {
	size_t s = 0;
	if (k == 1) {
		for (size_t i = a; i <= b; ++i) s += i;
		return s;
	}
	
	for (size_t i = a; i <= b; ++i) {
		s += partial_sum(a, i, k - 1);
	}
	return s;
}

void integer_test() {
	integer k = "12345678900123456789001234567890012345678900";
	cout << k << endl;
	
	integer two(2);
	cout << (two^2) << endl;
}

void triangle_test() {
	sparse_pascal_triangle ptri;
	ptri.init(10);
	cout << ptri << endl;
	ptri.fill();
	cout << ptri << endl;
	
	ptri.fill_rows(15);
	ptri.fill();
	cout << ptri << endl;
	
	cout << ptri.get_binomial(15, 3) << endl;
	cout << ptri.get_binomial(16, 3) << endl;
	cout << ptri << endl;
	
	cout << ptri.get_binomial(200, 3) << endl;
	cout << ptri << endl;
}

void rational_test() {
	rational r1("1/3");
	rational r2("2/3");
	
	cout << r1 + r2 + "28/28" << endl;
}

int main(int argc, char *argv[]) {
	integer_test();
	rational_test();
}


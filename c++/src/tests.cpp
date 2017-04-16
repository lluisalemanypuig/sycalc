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
#include "core/monomial.hpp"

#include "algorithms.hpp"

using namespace sycalc;
using namespace core;
using namespace numeric;

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

// from bool to string
string fbts(bool v) {
	return (v ? "Yes" : "No");
}

void integer_test() {
	cout << "Integer tests" << endl;
	
	integer k = "789456";
	cout << "    789456 == " << k << "? " << fbts(k == "789456") << endl;
	
	integer two(2);
	cout << "    2^2 = " << (two^2) << endl;
	cout << "    2^2 = " << (two^two) << endl;
	
	integer three(3), five(5);
	cout << "    3 + 5= " << three + five << endl;
	
	integer copy_three = three, copy_five = five;
	cout << "    3 + 5= " << copy_three + copy_five << endl;
	
	integer r = three + five;
	cout << "    3 + 5= " << r << endl;
	
	cout << "    2 < 5 ? " << fbts(integer(2) < integer(5)) << " - from int initialization" << endl;
	cout << "    2 < 5 ? " << fbts(integer("2") < integer("5")) << " - from string initialization" << endl;
	
	cout << "    2 + 5 = " << integer(2) + "5" << " - from integer + string op" << endl;
	
	integer one = five;
	one -= 4;
	cout << "    one == 1 ? " << fbts(one == 1) << endl;
	cout << "    one == 1 ? " << fbts(one == "1") << endl;
	
	cout << endl;
}

void rational_test() {
	cout << "Rational tests" << endl;
	
	rational r1("1/3");
	rational r2("2/3");
	
	cout << "    1/3 + 2/3 == 1 ? " << fbts((r1 + r2) == rational(1)) << endl;
	
	cout << "    (1/3)^4 == 1/81 ? " << fbts((rational("1/3")^4) == rational("1/81")) << endl;
	
	r1 ^= 4;
	cout << "    (1/3)^4 ^= " << r1 << endl;
	cout << "    (1/3)^4^4 = " << (r1^4) << endl;
	
	rational one_eigth("1/8");
	integer four(4);
	cout << "    (1/8)^4 = " << (one_eigth^four) << endl;
	
	one_eigth ^= four;
	cout << "    (1/8)^4 = " << (one_eigth) << endl;
	
	cout << "    rational 4/1: " << rational("4/1") << endl;
	cout << "    rational from integer 4: " << rational(integer(4)) << endl;
	
	cout << endl;
}

void monomial_test() {
	monomial m1(3, "x", 2);
	monomial m2(5, "x", 2);
	
	cout << m1 << " + " << m2 << " = " << m1 + m2 << endl;
	cout << m1 << " - " << m2 << " = " << m1 - m2 << endl;
	
	monomial m12(3, "x", 2);
	monomial m22(5, "x", 3);
	
	cout << m12 << " + " << m22 << " = " << m12 + m22 << endl;
	cout << m12 << " - " << m22 << " = " << m12 - m22 << endl;
	
	monomial m3(3, "x", 2);
	monomial m4(5, "x", 5);
	cout << m3 << " * " << m4 << " = " << m3 * m4 << endl;
}

void triangle_test() {
	cout << "Triangles tests" << endl;
	
	cout << "-- Dense Pascal Triangle" << endl;
	
	dense_pascal_triangle dptri;
	dptri.init(10);
	cout << "After init(10)" << endl;
	cout << dptri << endl;
	
	dptri.fill();
	cout << "After fill()" << endl;
	cout << dptri << endl;
	
	cout << "After fill_rows(15)" << endl;
	dptri.fill_rows(15);
	cout << dptri << endl;
	
	cout << dptri.get_binomial(15, 3) << endl;
	cout << dptri.get_binomial(16, 3) << endl;
	cout << dptri.get_binomial(16, 9) << endl;
	cout << dptri.get_binomial(16, 14) << endl;
	
	cout << "After computing some particular values of the triangle:" << endl;
	cout << dptri << endl;
	
	cout << "-- Sparse Pascal Triangle" << endl;
	
	sparse_pascal_triangle ptri;
	ptri.init(10);
	cout << "After init(10)" << endl;
	cout << ptri << endl;
	
	ptri.fill();
	cout << "After fill()" << endl;
	cout << ptri << endl;
	
	cout << "After fill_rows(15)" << endl;
	ptri.fill_rows(15);
	cout << ptri << endl;
	
	cout << ptri.get_binomial(15, 3) << endl;
	cout << ptri.get_binomial(16, 3) << endl;
	cout << ptri.get_binomial(16, 9) << endl;
	cout << ptri.get_binomial(16, 14) << endl;
	
	cout << "After computing some particular values of the triangle:" << endl;
	cout << ptri << endl;
	
	cout << endl;
}

int main(int argc, char *argv[]) {
	triangle_test();
}


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
#include "core/polynomial.hpp"
#include "core/monomial.hpp"

#include "algorithms.hpp"

using namespace sycalc;
using namespace core;
using namespace numeric;

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
	
	cout << "    5/2 = " << integer(5)/2 << endl;
	
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
	cout << "Monomial tests" << endl;
	
	monomial m1(3, 2);
	monomial m2(5, 2);
	
	cout << m1 << " + " << m2 << " = " << m1 + m2 << endl;
	cout << m1 << " - " << m2 << " = " << m1 - m2 << endl;
	
	monomial m12(3, 3);
	monomial m22("7/2", 3);
	
	cout << m12 << " + " << m22 << " = " << m12 + m22 << endl;
	cout << m12 << " - " << m22 << " = " << m12 - m22 << endl;
	
	monomial m3(3, 2);
	monomial m4(5, 5);
	cout << m3 << " * " << m4 << " = " << m3 * m4 << endl;
	
	cout << "- (3*l^234) = " << -monomial(3, "234") << endl;
	cout << endl;
	
	cout << "Pushing into vector" << endl;
	vector<monomial> v;
	v.push_back(m1);
	v.push_back(m2);
	v.push_back(monomial(1, 2));
	v.push_back(monomial("1/3", 2));
	
	cout << "(3*x^2)^5 = " << (monomial(3, 2)^5) << endl;
	cout << "((1/4)*x^3)^2 = " << (monomial("1/4", 3)^2) << endl;
	
	cout << endl;
}

void polynomial_test() {
	cout << "Polynomial tests" << endl;
	
	polynomial p1;
	p1 += monomial(-3, 4);
	p1 += monomial(-3, 2);
	p1 += monomial(-3, 1);
	p1 += monomial(-3, 3);
	
	cout << "p1(x)= " << p1 << endl;
	cout << "p1(3)= " << p1.evaluate(3) << endl;
	cout << endl;
	
	polynomial p2;
	p2 += monomial( 2, 0);
	p2 += monomial(-1, 4);
	p2 += monomial( 4, 2);
	p2 += monomial("-25/3", 1);
	p2 += monomial( "1/4", 25);
	cout << "p2(x)= " << p2 << endl;
	cout << "p2(3)= " << p2.evaluate(3) << endl;
	cout << endl;
	
	cout << "(p1 + p2)(x) = " << p1 + p2 << endl;
	cout << "(p1 + p2)(3) = " << (p1 + p2).evaluate(3) << endl;
	
	cout << "(p1 - p2)(x) = " << p1 - p2 << endl;
	cout << "(p1 - p2)(3) = " << (p1 - p2).evaluate(3) << endl;
	
	cout << "(p1 * p2)(x) = " << p1*p2 << endl;
	cout << "(p1 * p2)(3) = " << (p1*p2).evaluate(3) << endl;
	
	polynomial x_plus_one;
	x_plus_one += monomial(1, 0);
	x_plus_one += monomial(1, 1);
	cout << "(x + 1)^2 = " << (x_plus_one^2) << endl;
	cout << "(x + 1)^3 = " << (x_plus_one^3) << endl;
	cout << "(x + 1)^4 = " << (x_plus_one^4) << endl;
	
	polynomial x_minus_one;
	x_minus_one += monomial(-1, 0);
	x_minus_one += monomial(1, 1);
	cout << "(x - 1)^2 = " << (x_minus_one^2) << endl;
	cout << "(x - 1)^3 = " << (x_minus_one^3) << endl;
	cout << "(x - 1)^4 = " << (x_minus_one^4) << endl;
	cout << "(x - 1)^5 = " << (x_minus_one^5) << endl;
	
	cout << (x_minus_one^5).get_monomial_coefficient(5) << endl;
	cout << (x_minus_one^5).get_monomial_coefficient(4) << endl;
	cout << (x_minus_one^5).get_monomial_coefficient(3) << endl;
	cout << (x_minus_one^5).get_monomial_coefficient(2) << endl;
	cout << (x_minus_one^5).get_monomial_coefficient(1) << endl;
	cout << (x_minus_one^5).get_monomial_coefficient(0) << endl;
	
	cout << endl;
}

void algorithms_test() {
	cout << "Algorithms tests" << endl;
	
	cout << "-- Nested sums (a -> b)" << endl;
	cout << "(4, 7, 4)= " << algorithms::nested_sums(4, 7, 4) << endl;
	cout << "(3, 7, 5)= " << algorithms::nested_sums(3, 7, 5) << endl;
	cout << "(2, 7, 6)= " << algorithms::nested_sums(2, 7, 6) << endl;
	
	/*
	for (size_t i = 30; i <= 33; ++i) {
		cout << "(1, 7, " << i << ")= " << algorithms::nested_sums(1, 7, i) << endl;
	}
	
	cout << "-- Nested sums (1 -> n)" << endl;
	for (size_t i = 30; i <= 33; ++i) {
		cout << "(1, 7, " << i << ")= " << algorithms::nested_sums(7, i) << endl;
	}
	*/
	
	cout << "-- Power sums polynomials" << endl;
	for (size_t p = 1; p <= 100; ++p) {
		polynomial poly;
		algorithms::power_sums(p, poly);
		cout << "p=" << p << " -> " << poly << endl;
	}
	
	cout << endl;
}

int main(int argc, char *argv[]) {
	polynomial_test();
	algorithms_test();
}


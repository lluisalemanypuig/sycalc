#include "tests.hpp"

namespace sycalc {
namespace tests {

	void polynomial_tests() {
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

}
}


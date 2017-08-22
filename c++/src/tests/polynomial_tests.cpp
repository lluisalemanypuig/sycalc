#include "tests.hpp"

namespace sycalc {
namespace tests {

	void polynomial_tests() {
		cout << "Polynomial tests" << endl;
		
		polynomial omega;
		omega += monomial(-3, 4);
		omega += monomial("3/4", 2);
		omega += monomial("-1/2", 1);
		omega += monomial(2, 3);
		omega += monomial("24/33", 0);
		omega += monomial(-5, 5);
		cout << "    omega = " << omega << endl;
		cout << "    coefficient(omega, 0)= " << omega.get_monomial_coefficient(0) << endl;
		cout << "    coefficient(omega, 1)= " << omega.get_monomial_coefficient(1) << endl;
		cout << "    coefficient(omega, 2)= " << omega.get_monomial_coefficient(2) << endl;
		cout << "    coefficient(omega, 3)= " << omega.get_monomial_coefficient(3) << endl;
		cout << "    coefficient(omega, 4)= " << omega.get_monomial_coefficient(4) << endl;
		cout << "    coefficient(omega, 5)= " << omega.get_monomial_coefficient(5) << endl;
		
		polynomial p1;
		p1 += monomial(-3, 4);
		p1 += monomial(-3, 2);
		p1 += monomial(-3, 1);
		p1 += monomial(-3, 3);
		
		cout << "    p1(x)= " << p1 << endl;
		cout << "    p1(3)= " << p1.evaluate(3) << endl;
		cout << endl;
		
		polynomial p2;
		p2 += monomial( 2, 0);
		p2 += monomial(-1, 4);
		p2 += monomial( 4, 2);
		p2 += monomial("-25/3", 1);
		p2 += monomial( "1/4", 25);
		cout << "    p2(x)= " << p2 << endl;
		cout << "    p2(3)= " << p2.evaluate(3) << endl;
		cout << endl;
		
		cout << "    (p1 + p2)(x) = " << p1 + p2 << endl;
		cout << "    (p1 + p2)(3) = " << (p1 + p2).evaluate(3) << endl;
		
		cout << "    (p1 - p2)(x) = " << p1 - p2 << endl;
		cout << "    (p1 - p2)(3) = " << (p1 - p2).evaluate(3) << endl;
		
		cout << "    (p1 * p2)(x) = " << p1*p2 << endl;
		cout << "    (p1 * p2)(3) = " << (p1*p2).evaluate(3) << endl;
		
		polynomial x_plus_one;
		x_plus_one += monomial(1, 0);
		x_plus_one += monomial(1, 1);
		cout << "    (x + 1)^2 = " << (x_plus_one^2) << endl;
		cout << "    (x + 1)^3 = " << (x_plus_one^3) << endl;
		cout << "    (x + 1)^4 = " << (x_plus_one^4) << endl;
		
		polynomial x_minus_one;
		x_minus_one += monomial(-1, 0);
		x_minus_one += monomial(1, 1);
		cout << "    (x - 1)^2 = " << (x_minus_one^2) << endl;
		cout << "    (x - 1)^3 = " << (x_minus_one^3) << endl;
		cout << "    (x - 1)^4 = " << (x_minus_one^4) << endl;
		cout << "    (x - 1)^5 = " << (x_minus_one^5) << endl;
		
		cout << "    coefficient((x - 1)^5, 5)= " << (x_minus_one^5).get_monomial_coefficient(5) << endl;
		cout << "    coefficient((x - 1)^5, 4)= " << (x_minus_one^5).get_monomial_coefficient(4) << endl;
		cout << "    coefficient((x - 1)^5, 3)= " << (x_minus_one^5).get_monomial_coefficient(3) << endl;
		cout << "    coefficient((x - 1)^5, 2)= " << (x_minus_one^5).get_monomial_coefficient(2) << endl;
		cout << "    coefficient((x - 1)^5, 1)= " << (x_minus_one^5).get_monomial_coefficient(1) << endl;
		cout << "    coefficient((x - 1)^5, 0)= " << (x_minus_one^5).get_monomial_coefficient(0) << endl;
		
		cout << endl;
		
		polynomial large_poly("x");
		large_poly += monomial(1, -2, "x");
		large_poly += monomial(-2, 2, "x");
		large_poly += monomial(3, -3, "x");
		large_poly += monomial(-6, 4, "x");		// large_poly = 3x^(-3) + x^(-1) - 2x^2 - 6x^4
		
		cout << "          p(x)= " << large_poly << endl;
		cout << "         p(x)'= " << large_poly.derivative() << endl;
		cout << "     int(p(x))= " << large_poly.integral() << endl;
		cout << "    int(p(x))'= " << large_poly.integral().derivative() << endl;
	}

}
}


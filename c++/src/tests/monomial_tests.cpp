#include "tests.hpp"

namespace sycalc {
namespace tests {

	void monomial_tests() {
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

}
}


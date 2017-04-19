#include "tests.hpp"

namespace sycalc {
namespace tests {
	
	inline
	string fbts(bool v) {
		return (v ? "Yes" : "No");
	}
	
	void rational_tests() {
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
		
		rational one_fourth("1/4");
		cout << "    (1/4)/(1/8) = " << one_fourth/rational("1/8") << endl;
		cout << "    (1/4)/(8/1) = " << one_fourth/rational("8/1") << endl;
		cout << "    (1/4)/8 = " << one_fourth/8 << endl;
		
		cout << endl;
	}

}
}


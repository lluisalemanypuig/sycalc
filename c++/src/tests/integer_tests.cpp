#include "tests.hpp"

namespace sycalc {
namespace tests {
	
	inline
	string fbts(bool v) {
		return (v ? "Yes" : "No");
	}
	
	void integer_tests() {
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

}
}


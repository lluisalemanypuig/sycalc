#include "tests.hpp"

namespace sycalc {
namespace tests {

	void algorithms_tests() {
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
		for (size_t p = 1; p <= 20; ++p) {
			polynomial poly;
			algorithms::power_sums(p, poly);
			cout << "p=" << p << " -> " << poly << endl;
		}
		
		cout << endl;
	}

}
}


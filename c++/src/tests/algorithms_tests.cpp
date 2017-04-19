#include "tests.hpp"

namespace sycalc {
namespace tests {

	void algorithms_tests() {
		cout << "Algorithms tests" << endl;
		
		cout << "-- Nested sums (a -> b)" << endl;
		cout << "    (4, 7, 4)= " << algorithms::nested_sums(4, 7, 4) << endl;
		cout << "    (3, 7, 5)= " << algorithms::nested_sums(3, 7, 5) << endl;
		cout << "    (2, 7, 6)= " << algorithms::nested_sums(2, 7, 6) << endl;
		
		for (size_t i = 10; i <= 13; ++i) {
			cout << "    (1, 7, " << i << ")= " << algorithms::nested_sums(1, 7, i) << endl;
		}
		cout << endl;
		
		cout << "-- Nested sums (1 -> n)" << endl;
		for (size_t i = 10; i <= 13; ++i) {
			cout << "    (1, 7, " << i << ")= " << algorithms::nested_sums(7, i) << endl;
		}
		cout << endl;
		
		cout << "-- Power sums polynomials" << endl;
		const size_t P1 = 1;
		const size_t P2 = 100;
		vector<polynomial> polys(P2 - P1 + 1);
		
		timing begin, end;
		
		begin = now();
		algorithms::power_sums(P2, polys);
		end = now();
		cout << "    Time to calculate polynomials: " << elapsed_time(begin, end) << " s" << endl;
		cout << endl;
		
		for (size_t p = P1; p <= P1 + 3; ++p) {
			for (size_t n = 50; n <= 100; ++n) {
				rational poly_eval = polys[p - P1].evaluate(n);
				integer lin_sum = algorithms::linear_power_sums(1, n, p);
				
				if (poly_eval != lin_sum) {
					cout << "    " << p << " -> p= " << polys[p - P1] << endl;
					cout << "    " << p << " -> p(" << n << ")= " << poly_eval << endl;
					cout << "    " << p << " ->     l(" << n << ")= " << lin_sum << endl;
				}
			}
		}
		
		cout << endl;
	}

}
}


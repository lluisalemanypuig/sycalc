/// C includes
#include <gmp.h>

/// C++ includes
#include <iostream>
using namespace std;

/// Custom includes
#include "algorithms.hpp"
#include "polynomial.hpp"
#include "tests.hpp"
#include "timing.hpp"

using namespace sycalc;
using namespace mtime;
using namespace algorithms;
using namespace core;

void some_tests(int argc, char *argv[]) {
	//sycalc::tests::polynomial_tests();
	//sycalc::tests::algorithms_tests();
	
	timing gbegin = now();
	const size_t s = 100;
	for (size_t d = s; d <= s + 20; ++d) {
		cout << "d -> " << d << endl;
		
		polynomial p;
		const size_t inf_lim = 2500001;
		const size_t sup_lim = 2500010;
		
		timing begin = now();
		algorithms::power_sums(d, p);
		/*cout << "    p(n)= " << p << endl;
		for (size_t n = inf_lim; n <= sup_lim; ++n) {
			cout << "        p(" << n << ")= " << p.evaluate(n) << endl;
		}*/
		timing end = now();
		cout << "        * Polynomial computation and evaluation: " << elapsed_time(begin, end) << " s" << endl;
		
		/*
		begin = now();
		cout << "        ---" << endl;
		for (size_t n = inf_lim; n <= sup_lim; ++n) {
			cout << "        L(" << n << ")= " << algorithms::linear_power_sums(1, n, d) << endl;
		}
		end = now();
		cout << "        * Linear computation: " << elapsed_time(begin, end) << " s" << endl;
		
		cout << endl;
		*/
	}
	timing gend = now();
	cout << "    * Total time: " << elapsed_time(gbegin, gend) << " s" << endl;
}

void nice_demo() {
	int D = 1;
	cout << "Enter a power: ";
	
	while (cin >> D and D > 0) {
		
		polynomial p;
		timing begin = now();
		algorithms::power_sums(D, p);
		timing end = now();

		cout << "The formula is: " << p << endl;
		cout << "    Computed in " << elapsed_time(begin, end) << " s" << endl;
		
		size_t n;
		cout << "sum from 1 to:"; cin >> n;
		
		rational poly_res, linear_res;
		
		begin = now();
		poly_res = p.evaluate(n);
		end = now();
		cout << "polynomial: p(" << n << ")= " << poly_res << endl;
		cout << "    in " << elapsed_time(begin, end) << endl;
		
		begin = now();
		linear_res = algorithms::linear_power_sums(1, n, D);
		end = now();
		cout << "linear sum(" << n << ")= " << linear_res << endl;
		cout << "    in " << elapsed_time(begin, end) << endl;
		
		cout << endl;
		cout << "Are results equal? " << (poly_res == linear_res ? "Yes" : "No") << endl;
		
		cout << "Enter a power: ";
	}
}

int main() {
	polynomial base1;
	base1 += monomial(1, 1, "i"); 	// base1 = i
	base1 += monomial(1, 0, "i"); 	// base1 = i + 1
	base1 = base1*base1*base1;		// base1 = (i + 1)^3
	base1 *= monomial(1, 1, "i");	// base1 = i*(i + 1)^3
	cout << "base1: " << base1 << endl;
	
	polynomial p;
	algorithms::polynomial_sum(base1, p);
	cout << "p : " << p << endl;
	
}


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

int main(int argc, char *argv[]) {
	//sycalc::tests::algorithms_tests();
	
	for (size_t d = 10; d <= 20; ++d) {
		cout << "d -> " << d << endl;
		
		polynomial p;
		const size_t inf_lim = 2500001;
		const size_t sup_lim = 2500010;
		
		timing begin = now();
		algorithms::power_sums(d, p);
		cout << "    p(n)= " << p << endl;
		for (size_t n = inf_lim; n <= sup_lim; ++n) {
			cout << "        p(" << n << ")= " << p.evaluate(n) << endl;
		}
		timing end = now();
		cout << "        * Polynomial computation and evaluation: " << elapsed_time(begin, end) << " s" << endl;
		
		begin = now();
		cout << "        ---" << endl;
		for (size_t n = inf_lim; n <= sup_lim; ++n) {
			cout << "        L(" << n << ")= " << algorithms::linear_power_sums(1, n, d) << endl;
		}
		end = now();
		cout << "        * Linear computation: " << elapsed_time(begin, end) << " s" << endl;
		
		cout << endl;
	}
}


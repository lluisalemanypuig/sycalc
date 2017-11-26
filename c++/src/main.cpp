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

void add_rationals() {
	rational r1, r2;
	while (cin >> r1 >> r2) {
		cout << r1 << " + " << r2 << " = " << r1 + r2 << endl;
		cout << r1 << " - " << r2 << " = " << r1 - r2 << endl;
	}
}

void find_contradiction() {
	cout << "Enter power: ";
	int d;
	cin >> d;
	
	polynomial pD;
	algorithms::power_sums(d, pD);
	
	cout << "Enter delta (e.g. 17/63): ";
	rational delta;
	cin >> delta;
	
	rational x0("-1/2");
	cout << "p_" << d << "(x0 + delta) = p_" << d << "(" << x0 << " + " << delta << ")= p_" << d << "(" << x0 + delta << ")= ";
	cout << pD.evaluate(x0 + delta) << endl;
	
	cout << "p_" << d << "(x0 - delta) = p_" << d << "(" << x0 << " - " << delta << ")= p_" << d << "(" << x0 - delta << ")= ";
	cout << pD.evaluate(x0 - delta) << endl;
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
		cout << "    Was computed in " << elapsed_time(begin, end) << " seconds" << endl;
		cout << "    Has size: " << p.bytes() << " bytes" << endl;
		
		size_t n;
		cout << "sum from 1 to: "; cin >> n;
		
		rational poly_res, linear_res;
		
		begin = now();
		poly_res = p.evaluate(n);
		end = now();
		cout << "polynomial: p(" << n << ")= " << poly_res << endl;
		cout << "    computed in " << elapsed_time(begin, end) << " seconds" << endl;
		
		begin = now();
		linear_res = algorithms::linear_power_sums(1, n, D);
		end = now();
		cout << "linear sum(" << n << ")= " << linear_res << endl;
		cout << "    computed in " << elapsed_time(begin, end) << " seconds" << endl;
		cout << "Are results equal? " << (poly_res == linear_res ? "Yes" : "No") << endl;
		
		cout << "Enter a power: ";
	}
}

int main() {
	/*
	const size_t p1 = 10;
	const size_t p2 = 20;
	
	vector<polynomial> polys1(p1);
	algorithms::power_sums(p1, polys1);
	
	for (size_t i = 0; i < p1; ++i) {
		cout << "power: " << i + 1 << " -> " << polys1[i] << endl;
	}
	
	cout << "*********************" << endl;
	
	vector<polynomial> polys2(p2);
	algorithms::power_sums(p2, polys2);
	
	vector<polynomial> polys3(p2);
	for (size_t i = 0; i < p1; ++i) polys3[i] = polys1[i];
	algorithms::power_sums(p1 + 1, p2, polys3);
	
	for (size_t i = 0; i < p2; ++i) {
		cout << "power (2): " << i + 1 << " -> " << polys2[i] << endl;
		cout << "power (3): " << i + 1 << " -> " << polys3[i] << endl;
		cout << "-----" << endl;
	}
	*/
	
	//tests::polynomial_tests();
	nice_demo();
	
	//find_contradiction();
	//add_rationals();
}


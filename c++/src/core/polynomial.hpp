#pragma once

/// C++ includes
#include <vector>
#include <string>
using namespace std;

/// Custom includes
#include "monomial.hpp"
#include "rational.hpp"
#include "integer.hpp"
#include "inout.hpp"

namespace sycalc {
namespace core {

class polynomial {
	private:
		vector<monomial> ms;
		
		// Adds a monomial to the list of monomials using the insertion sort algorithm.
		void add_monomial(const monomial& m);
		
		// Inserts a monomial at the specified position
		void add_monomial(const monomial& m, size_t idx);
	
	public:
		polynomial();
		polynomial(const polynomial& p);
		~polynomial();
		
		/* OPERATORS */
		
		polynomial& operator= (const monomial& m);
		polynomial& operator= (const polynomial& p);
		
		bool operator== (const polynomial& p) const;
		bool operator!= (const polynomial& p) const;
		
		polynomial operator+ (const monomial& m) const;
		polynomial operator+ (const polynomial& p) const;
		
		polynomial& operator+= (const monomial& m);
		polynomial& operator+= (const polynomial& p);
		
		polynomial operator- (const monomial& m) const;
		polynomial operator- (const polynomial& p) const;
		
		polynomial& operator-= (const monomial& m);
		polynomial& operator-= (const polynomial& p);
		
		polynomial operator* (const monomial& m) const;
		polynomial operator* (const polynomial& p) const;
		
		polynomial& operator*= (const monomial& m);
		polynomial& operator*= (const polynomial& p);
		
		inline friend
		ostream& operator<< (ostream& os, const polynomial& p) {
			os << p.ms[0];
			for (size_t m = 1; m < p.ms.size(); ++m) {
				if (p.ms[m].get_coefficient().get_sign() == 1) {
					os << " + " << p.ms[m];
				}
				else {
					os << " " << p.ms[m];
				}
			}
			return os;
		}
		
};

static inline
void swap(polynomial& a, polynomial& b) {
	polynomial copy = a;
	a = b;
	b = copy;
}

}
}


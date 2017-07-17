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
		string var;
		
		// Adds a monomial to the list of monomials using the insertion sort algorithm.
		void add_monomial(const monomial& m);
		
		// Inserts a monomial at the specified position
		void add_monomial(const monomial& m, size_t idx);
	
	public:
		polynomial(const string& var_name = "x");
		polynomial(const polynomial& p);
		~polynomial();
		
		rational evaluate(const rational& r) const;
		
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
		
		polynomial operator* (const integer& i) const;
		polynomial operator* (const rational& r) const;
		polynomial operator* (const monomial& m) const;
		polynomial operator* (const polynomial& p) const;
		
		polynomial& operator*= (const integer& i);
		polynomial& operator*= (const rational& r);
		polynomial& operator*= (const monomial& m);
		polynomial& operator*= (const polynomial& p);
		
		polynomial operator^ (const integer& i) const;
		polynomial& operator^= (const integer& i);
		
		const monomial& operator[] (size_t i) const;
		monomial& operator[] (size_t i);
		
		inline friend
		ostream& operator<< (ostream& os, const polynomial& p) {
			if (p.ms.size() > 0) {
				os << p.ms.back();
				if (p.ms.size() > 1) {
					for (size_t m = p.ms.size() - 2; m > 0; --m) {
						
						if (p.ms[m].get_coefficient() != 0) {
							if (p.ms[m].get_coefficient().get_sign() == 1) {
								os << " + " << p.ms[m];
							}
							else {
								os << " - " << -p.ms[m];
							}
						}
					}
					if (p.ms[0].get_coefficient() != 0) {
						if (p.ms[0].get_coefficient().get_sign() == 1) {
							os << " + " << p.ms[0];
						}
						else {
							os << " - " << -p.ms[0];
						}
					}
				}
			}
			return os;
		}
		
		/* SETTERS */
		
		void set_var_name(const string& var_name);
		
		/* GETTERS */
		
		integer get_degree() const;
		rational get_monomial_coefficient(const integer& d) const;
		
		size_t size() const;
		const string& get_var_name() const;
};

static inline
void swap(polynomial& a, polynomial& b) {
	polynomial copy = a;
	a = b;
	b = copy;
}

}
}


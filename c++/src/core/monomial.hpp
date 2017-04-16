#pragma once

/// C++ includes
#include <string>
using namespace std;

/// Custom includes
#include "rational.hpp"
#include "integer.hpp"
#include "inout.hpp"

namespace sycalc {
namespace core {

using namespace numeric;

class monomial {
	private:
		rational c;
		integer e;
		string var;
	
	public:
		monomial();
		monomial(const rational& coef, const integer& exp, const string& var_name = "l");
		monomial(const monomial& m);
		~monomial();
		
		rational evaluate(const rational& r) const;
		rational evaluate(const integer& i) const;
		
		/* OPERATORS */
		
		monomial& operator= (const monomial& m);
		
		// All these operators assume equal variables
		bool operator== (const monomial& m) const;
		bool operator!= (const monomial& m) const;
		
		monomial operator+ (const monomial& m) const;
		monomial& operator+= (const monomial& m);
		
		monomial operator- () const;
		monomial operator- (const monomial& m) const;
		monomial& operator-= (const monomial& m);
		
		monomial operator* (const monomial& m) const;
		monomial& operator*= (const monomial& m);
		
		inline friend
		ostream& operator<< (ostream& os, const monomial& m) {
			if (m.e == 0) {
				if (m.c != 0) {
					os << m.c;
				}
			}
			else if (m.c != 0) {
				if (m.c != 1) {
					os << m.c << "*";
				}
				os << m.var;
				
				if (m.e != 1) {
					os << "^" << m.e;
				}
			}
			return os;
		}
		
		/* SETTERS */
		
		void set_var_name(const string& n);
		void set_coefficient(const rational& r);
		void set_exponent(const integer& i);
		
		/* GETTERS */
		
		const rational& get_coefficient() const;
		const string& get_var_name() const;
		const integer& get_exponent() const;
		
		string get_raw_string() const;
};

static inline
void swap(monomial& a, monomial& b) {
	monomial copy = a;
	a = b;
	b = copy;
}

}
}


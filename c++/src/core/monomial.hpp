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
		monomial(const rational& coef, const integer& exp, const string& var_name = "x");
		~monomial();
		
		rational eval(const rational& r) const;
		rational eval(const integer& i) const;
		
		/* OPERATORS */
		
		monomial& operator= (const monomial& m);
		
		// All these operators assume equal variables
		bool operator== (const monomial& m) const;
		bool operator!= (const monomial& m) const;
		
		monomial operator+ (const monomial& m) const;
		monomial operator- (const monomial& m) const;
		monomial operator* (const monomial& m) const;
		
		inline friend
		ostream& operator<< (ostream& os, const monomial& m) {
			if (m.e == 0) {
				os << "1";
			}
			else {
				if (m.c != 1) {
					os << m.c;
				}
				os << m.var;
				
				if (m.e != 1) {
					os << "^" << m.e;
				}
			}
			return os;
		}
		
		/* GETTERS */
		
		const string& get_var_name() const;
		
};

}
}


#include "monomial.hpp"

namespace sycalc {
namespace core {

/// NON-CLASS PRIVATE

#define EQUAL_VARS(m1, m2)						\
	if (m1->get_var_name() != m2.get_var_name())

/// PRIVATE

/// PUBLIC

monomial::monomial() {
	var = "anon";
}

monomial::monomial(const rational& coef, const integer& exp, const string& var_name) {
	c = coef;
	var = var_name;
	e = exp;
}

monomial::~monomial() { }

rational monomial::eval(const rational& r) const {
	return c*(r^e);
}

rational monomial::eval(const integer& i) const {
	return c*(i^e);
}

/* OPERATORS */

monomial& monomial::operator= (const monomial& m) {
	c = m.c;
	var = m.var;
	e = m.e;
	return *this;
}

bool monomial::operator== (const monomial& m) const {
	return (c == m.c and e == m.e);
}

bool monomial::operator!= (const monomial& m) const {
	return not (*this == m);
}

monomial monomial::operator+ (const monomial& m) const {
	if (e != m.e) {
		STD_ERR << "Error: cannot operate '" << *this << " + " << m << "'." << endl;
		return monomial(0, 1, "anon");
	}
	
	return monomial(c + m.c, e, var);
}

monomial monomial::operator- (const monomial& m) const {
	if (e != m.e) {
		STD_ERR << "Error: cannot operate '" << *this << " - " << m << "'." << endl;
		return monomial(0, 1, "anon");
	}
	
	return monomial(c - m.c, e, var);
}

monomial monomial::operator* (const monomial& m) const {
	return monomial(c*m.c, e + m.e, var);
}

/* GETTERS */

const string& monomial::get_var_name() const {
	return var;
}

}
}


#include "monomial.hpp"

namespace sycalc {
namespace core {

/// NON-CLASS PRIVATE

#define __SYCALC_CORE_MONOMIAL_ERROR_MSG(m_This, m_Other)				\
	"(monomial) Error: cannot operate '"	+	\
	m_This->get_raw_string()				+	\
	" + "									+ 	\
	m_Other.get_raw_string()				+	\
	"'."
	
/// PRIVATE

/// PUBLIC

monomial::monomial() {
	var = "x";
}

monomial::monomial(const rational& coef, const integer& exp, const string& var_name) {
	c = coef;
	var = var_name;
	e = exp;
}

monomial::monomial(const monomial& m) {
	*this = m;
}

monomial::~monomial() { }

rational monomial::evaluate(const rational& r) const {
	return c*(r^e);
}

rational monomial::evaluate(const integer& i) const {
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

monomial monomial::operator+ (const monomial& m) const	{ monomial r(*this); r += m; return r; }

monomial& monomial::operator+= (const monomial& m) {
	if (e != m.e) {
		DISPLAY_ERR(__SYCALC_CORE_MONOMIAL_ERROR_MSG(this, m));
		return *this;
	}
	
	c += m.c;
	return *this;
}

monomial monomial::operator- () const {
	return monomial(-c, e, var);
}

monomial monomial::operator- (const monomial& m) const {
	monomial r(*this);
	r -= m;
	return r;
}

monomial& monomial::operator-= (const monomial& m) {
	if (e != m.e) {
		DISPLAY_ERR(__SYCALC_CORE_MONOMIAL_ERROR_MSG(this, m));
		return *this;
	}
	
	c -= m.c;
	return *this;
}

monomial monomial::operator* (const integer& i) const		{ monomial r(*this); r *= i; return r; }
monomial monomial::operator* (const rational& s) const		{ monomial r(*this); r *= s; return r; }
monomial monomial::operator* (const monomial& m) const	{ monomial r(*this); r *= m; return r; }

monomial& monomial::operator*= (const integer& i) {
	c *= i;
	return *this;
}

monomial& monomial::operator*= (const rational& s) {
	c *= s;
	return *this;
}

monomial& monomial::operator*= (const monomial& m) {
	c *= m.c;
	e += m.e;
	return *this;
}

monomial monomial::operator^ (unsigned int i) const	{ monomial c = *this; c ^= i; return c; }
monomial monomial::operator^ (const integer& i) const	{ monomial c = *this; c ^= i; return c; }

monomial& monomial::operator^= (unsigned int i) {
	c ^= i;
	e ^= i;
	return *this;
}

monomial& monomial::operator^= (const integer& i) {
	c ^= i;
	e ^= i;
	return *this;
}

/* SETTERS */

void monomial::set_var_name(const string& n) {
	var = n;
}

void monomial::set_coefficient(const rational& r) {
	c = r;
}

void monomial::set_exponent(const integer& i) {
	e = i;
}

/* GETTERS */

const rational& monomial::get_coefficient() const {
	return c;
}

const string& monomial::get_var_name() const {
	return var;
}

const integer& monomial::get_exponent() const {
	return e;
}

string monomial::get_raw_string() const {
	return c.to_string() + "*" + var + "^" + e.to_string();
}

}
}


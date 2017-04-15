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

monomial::monomial(const rational& coef, const string& var_name, const integer& exp) {
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
		return monomial(0, "anon", 1);
	}
	
	return monomial(c + m.c, var, e);
}

monomial& monomial::operator+= (const monomial& m) {
	if (e != m.e) {
		STD_ERR << "Error: cannot operate '" << *this << " + " << m << "'." << endl;
		return *this;
	}
	
	c += m.c;
	return *this;
}

monomial monomial::operator- (const monomial& m) const {
	if (e != m.e) {
		STD_ERR << "Error: cannot operate '" << *this << " - " << m << "'." << endl;
		return monomial(0, "anon", 1);
	}
	
	return monomial(c - m.c, var, e);
}

monomial& monomial::operator-= (const monomial& m) {
	if (e != m.e) {
		STD_ERR << "Error: cannot operate '" << *this << " - " << m << "'." << endl;
		return *this;
	}
	
	c -= m.c;
	return *this;
}

monomial monomial::operator* (const monomial& m) const {
	return monomial(c*m.c, var, e + m.e);
}

monomial& monomial::operator*= (const monomial& m) {
	c *= m.c;
	e += m.e;
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

const string& monomial::get_var_name() const {
	return var;
}

const rational& monomial::get_coefficient() const {
	return c;
}

const integer& monomial::get_exponent() const {
	return e;
}

string monomial::get_raw_string() const {
	return c.to_string() + "*" + var + "^" + e.to_string();
}

string monomial::get_pretty_string() const {
	string p = "";
	if (e == 0) {
		p += "1";
	}
	else if (c != 0) {
		if (c != 1) {
			p += c.to_string() + "*";
		}
		p += var;
		
		if (e != 1) {
			p += "^" + e.to_string();
		}
	}
	else {
		p += "0";
	}
	return p;
}

}
}


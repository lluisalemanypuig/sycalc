#include "polynomial.hpp"

namespace sycalc {
namespace core {

/// PRIVATE

void polynomial::add_monomial(const monomial& m) {
	ms.push_back(m);
	
	size_t idx = ms.size() - 1;
	while (idx > 0 and ms[idx - 1].get_exponent() > ms[idx].get_exponent()) {
		swap(ms[idx - 1], ms[idx]);
		--idx;
	}
}

void polynomial::add_monomial(const monomial& m, size_t idx) {
	ms.insert(ms.begin() + idx, m);
}

/// PUBLIC

polynomial::polynomial(const string& var_name) {
	var = var_name;
}

polynomial::polynomial(const polynomial& p) {
	*this = p;
}

polynomial::~polynomial() { }

rational polynomial::evaluate(const rational& r) const {
	rational e = 0;
	for (size_t i = 0; i < ms.size(); ++i) {
		e += ms[i].evaluate(r);
	}
	return e;
}

/* OPERATORS */

polynomial& polynomial::operator= (const monomial& m) {
	ms = vector<monomial>();
	ms.push_back(m);
	var = m.get_var_name();
	return *this;
}

polynomial& polynomial::operator= (const polynomial& p) {
	ms = p.ms;
	return *this;
}

bool polynomial::operator== (const polynomial& p) const {
	bool eq = true;
	
	size_t m = 0;
	while (m < ms.size() and eq) {
		if (ms[m] != p.ms[m]) {
			eq = false;
		}
		++m;
	}
	
	return eq;
}

bool polynomial::operator!= (const polynomial& p) const {
	return not (*this == p);
}

polynomial polynomial::operator+ (const monomial& m) const		{ polynomial r(*this); r += m; return r; }
polynomial polynomial::operator+ (const polynomial& p) const	{ polynomial r(*this); r += p; return r; }

polynomial& polynomial::operator+= (const monomial& m) {
	if (ms.size() == 0) {
		ms.push_back(m);
	}
	else {
		size_t idx = 0;
		while (idx < ms.size() and ms[idx].get_exponent() < m.get_exponent()) {
			++idx;
		}
		
		if (idx < ms.size() and ms[idx].get_exponent() == m.get_exponent()) {
			ms[idx] += m;
		}
		else {
			add_monomial(m, idx);
		}
	}
	
	return *this;
}

polynomial& polynomial::operator+= (const polynomial& p) {
	for (size_t i = 0; i < p.ms.size(); ++i) {
		*this += p.ms[i];
	}
	return *this;
}

polynomial polynomial::operator- (const monomial& m) const		{ polynomial r(*this); r -= m; return r; }
polynomial polynomial::operator- (const polynomial& p) const	{ polynomial r(*this); r -= p; return r; }

polynomial& polynomial::operator-= (const monomial& m) {
	size_t idx = 0;
	while (idx < ms.size() and ms[idx].get_exponent() < m.get_exponent()) {
		++idx;
	}
	
	if (ms[idx].get_exponent() == m.get_exponent()) {
		ms[idx] -= m;
	}
	else {
		add_monomial(-m, idx);
	}
	
	return *this;
}

polynomial& polynomial::operator-= (const polynomial& p) {
	for (size_t i = 0; i < p.ms.size(); ++i) {
		*this -= p.ms[i];
	}
	return *this;
}

polynomial polynomial::operator* (const integer& i) const		{ polynomial r(*this); r *= i; return r; }
polynomial polynomial::operator* (const rational& s) const		{ polynomial r(*this); r *= s; return r; }
polynomial polynomial::operator* (const monomial& m) const		{ polynomial r(*this); r *= m; return r; }
polynomial polynomial::operator* (const polynomial& p) const	{ polynomial r(*this); r *= p; return r; }

polynomial& polynomial::operator*= (const integer& i) {
	for (size_t j = 0; j < ms.size(); ++j) {
		ms[j] *= i;
	}
	return *this;
}

polynomial& polynomial::operator*= (const rational& r) {
	for (size_t i = 0; i < ms.size(); ++i) {
		ms[i] *= r;
	}
	return *this;
}

polynomial& polynomial::operator*= (const monomial& m) {
	for (size_t i = 0; i < ms.size(); ++i) {
		ms[i] *= m;
	}
	return *this;
}

polynomial& polynomial::operator*= (const polynomial& p) {
	vector<polynomial> prod(p.ms.size(), *this);
	for (size_t i = 0; i < p.ms.size(); ++i) {
		prod[i] *= p.ms[i];
	}
	
	for (size_t i = 1; i < prod.size(); ++i) {
		prod[0] += prod[i];
	}
	
	*this = prod[0];
	return *this;
}

polynomial polynomial::operator^ (const integer& i) const {
	polynomial c = *this;
	c ^= i;
	return c;
}

polynomial& polynomial::operator^= (const integer& i) {
	if (i == 1) {
		// do nothing
	}
	else if (i == 2) {
		*this = (*this)*(*this);
	}
	else if (i%2 == 0) {
		integer h = i/2;
		*this ^= h;						// q = p^(i/2)
		*this = (*this)*(*this);		// q = (p^(i/2))^2 = p^i
	}
	else {
		integer e = i - 1;
		*this = ((*this)^e)*(*this);	// q = (p^(i - 1))*p
	}
	
	return *this;
}

monomial polynomial::operator[] (size_t i) const	{ return ms[i]; }
monomial& polynomial::operator[] (size_t i)			{ return ms[i]; }

/* GETTERS */

integer polynomial::get_degree() const {
	integer d;
	if (ms.size() == 0) {
		d = 0;
	}
	else {
		d = ms.back().get_exponent();
	}
	return d;
}

rational polynomial::get_monomial_coefficient(const integer& d) const {
	rational r = 0;
	bool found = false;
	size_t i = 0;
	while (i < ms.size() and not found) {
		if (ms[i].get_exponent() == d) {
			found = true;
			r = ms[i].get_coefficient();
		}
		++i;
	}
	return r;
}

size_t polynomial::size() const {
	return ms.size();
}

const string& polynomial::get_var_name() const {
	return var;
}

}
}


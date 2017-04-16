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

polynomial::polynomial() { }
polynomial::polynomial(const polynomial& p) {
	*this = p;
}

polynomial::~polynomial() { }

/* OPERATORS */

polynomial& polynomial::operator= (const monomial& m) {
	ms = vector<monomial>();
	ms.push_back(m);
}

polynomial& polynomial::operator= (const polynomial& p) {
	ms = p.ms;
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

polynomial polynomial::operator+ (const monomial& m) const {
	polynomial r = *this;
	r += m;
	return r;
}

polynomial polynomial::operator+ (const polynomial& p) const {
	polynomial r = *this;
	r += p;
	return r;
}

polynomial& polynomial::operator+= (const monomial& m) {
	size_t idx = 0;
	while (idx < ms.size() and ms[idx].get_exponent() < m.get_exponent()) {
		++idx;
	}
	
	if (ms[idx].get_exponent() == m.get_exponent()) {
		ms[idx] += m;
	}
	else {
		add_monomial(m, idx);
	}
	
	return *this;
}

polynomial& polynomial::operator+= (const polynomial& p) {
	for (size_t i = 0; i < p.ms.size(); ++i) {
		*this += p.ms[i];
	}
	return *this;
}

polynomial polynomial::operator- (const monomial& m) const {
	polynomial r = *this;
	r -= m;
	return r;
}

polynomial polynomial::operator- (const polynomial& p) const {
	polynomial r = *this;
	r -= p;
	return r;
}

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

polynomial polynomial::operator* (const monomial& m) const {
	polynomial r = *this;
	r *= m;
	return r;
}

polynomial polynomial::operator* (const polynomial& p) const {
	polynomial r = *this;
	r *= p;
	return r;
}

polynomial& polynomial::operator*= (const monomial& m) {
	for (size_t i = 0; i < ms.size(); ++i) {
		ms[i] += m;
	}
	return *this;
}

polynomial& polynomial::operator*= (const polynomial& p) {
	vector<polynomial> prod(p.ms.size(), *this);
	for (size_t i = 0; i < p.ms.size(); ++i) {
		prod[i] *= p.ms[i];
	}
	
	for (size_t i = 1; i < p.ms.size(); ++i) {
		prod[0] += prod[i];
	}
	
	*this = prod[0];
	return *this;
}

}
}


#include "polynomial.hpp"

namespace sycalc {
namespace core {

/// PRIVATE

inline
bool find_mon_degree(const vector<monomial>& ms, const integer& d, size_t& idx) {
	int l = 0;
	int r = ms.size() - 1;
	
	while (l < r) {
		int m = (l + r)/2;
		if (ms[m].get_exponent() < d) {
			l = m + 1;
		}
		else {
			r = m;
		}
	}
	
	bool found = false;
	int m = (l + r)/2;
	if (ms[m].get_exponent() == d) {
		idx = m;
		found = true;
	}
	
	return found;
}

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

polynomial polynomial::operator+ (const monomial& m) const	{ polynomial r(*this); r += m; return r; }
polynomial polynomial::operator+ (const polynomial& p) const	{ polynomial r(*this); r += p; return r; }

polynomial& polynomial::operator+= (const monomial& m) {
	if (ms.size() == 0) {
		ms.push_back(m);
	}
	else {
		size_t idx;
		bool found = find_mon_degree(ms, m.get_exponent(), idx);
		
		if (found) {
			ms[idx] += m;
		}
		else {
			add_monomial(m);
		}
	}
	
	return *this;
}

polynomial& polynomial::operator+= (const polynomial& p) {
	if (ms.size() == 0) {
		ms = p.ms;
	}
	else {
		size_t it, ip;
		it = ip = 0;
		
		while (it < ms.size() and ip < p.ms.size()) {
			const integer& ms_deg = ms[it].get_exponent();
			const integer& pms_deg = p.ms[ip].get_exponent();
			
			if (ms_deg == pms_deg) {
				ms[it] += p.ms[ip];
				++it;
				++ip;
			}
			else if (ms_deg < pms_deg) {
				++it;
			}
			else {
				ms.insert(ms.begin() + it, p.ms[ip]);
				++it;
				++ip;
			}
		}
		
		ms.insert(ms.end(), p.ms.begin() + ip, p.ms.end());
	}
	
	return *this;
}

polynomial polynomial::operator- (const monomial& m) const	{ polynomial r(*this); r -= m; return r; }
polynomial polynomial::operator- (const polynomial& p) const	{ polynomial r(*this); r -= p; return r; }

polynomial& polynomial::operator-= (const monomial& m) {
	if (ms.size() == 0) {
		ms.push_back(-m);
	}
	else {
		size_t idx;
		bool found = find_mon_degree(ms, m.get_exponent(), idx);
		
		if (found) {
			ms[idx] -= m;
		}
		else {
			add_monomial(-m);
		}
	}
	
	return *this;
}

polynomial& polynomial::operator-= (const polynomial& p) {
	if (ms.size() == 0) {
		ms = p.ms;
	}
	else {
		size_t it, ip;
		it = ip = 0;
		
		while (it < ms.size() and ip < p.ms.size()) {
			const integer& ms_deg = ms[it].get_exponent();
			const integer& pms_deg = p.ms[ip].get_exponent();
			
			if (ms_deg == pms_deg) {
				ms[it] -= p.ms[ip];
				++it;
				++ip;
			}
			else if (ms_deg < pms_deg) {
				++it;
			}
			else {
				ms.insert(ms.begin() + it, -p.ms[ip]);
				++it;
				++ip;
			}
		}
		
		for (; ip < p.ms.size(); ++ip) {
			ms.push_back(-p.ms[ip]);
		}
	}
	
	return *this;
}

polynomial polynomial::operator* (const integer& i) const		{ polynomial r(*this); r *= i; return r; }
polynomial polynomial::operator* (const rational& s) const	{ polynomial r(*this); r *= s; return r; }
polynomial polynomial::operator* (const monomial& m) const	{ polynomial r(*this); r *= m; return r; }
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
	polynomial res;
	for (size_t i = 0; i < p.ms.size(); ++i) {
		res += (*this)*p.ms[i];
	}
	
	*this = res;
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
		*this *= (*this);		// q = (p^(i/2))^2 = p^i
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
	integer d = 0;
	if (ms.size() > 0) {
		d = ms.back().get_exponent();
	}
	return d;
}

rational polynomial::get_monomial_coefficient(const integer& d) const {
	size_t idx;
	bool found = find_mon_degree(ms, d, idx);
	
	rational r = 0;
	if (found) {
		r = ms[idx].get_coefficient();
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


#include "rational.hpp"

namespace sycalc {
namespace core {
namespace numeric {

/// NON-CLASS PRIVATE

inline
void mpz_divide_mpq(mpq_t& num, const mpz_t& c) {
	mpz_t b;
	mpz_init(b);
	
	mpq_get_den(b, num);	// num = a/b
	
	mpz_mul(b, b, c);
	
	mpq_set_den(num, b);
	mpq_canonicalize(num);
	
	mpz_clear(b);
}

inline
void mpq_divide_mpq(mpq_t& num, const mpq_t& den) {
	mpz_t a, b, c, d;
	mpz_inits(a, b, c, d, NULL);
	
	mpq_get_num(a, num);	// num = a/b
	mpq_get_den(b, num);
	mpq_get_num(c, den);	// den = c/d
	mpq_get_den(d, den);
	
	mpz_mul(a, a, d);
	mpz_mul(b, b, c);
	
	mpq_set_num(num, a);
	mpq_set_den(num, b);
	mpq_canonicalize(num);
	
	mpz_clears(a, b, c, d, NULL);
}

inline
void operate_power(mpq_t& res, unsigned int p) {
	mpz_t num, den;
	mpz_inits(num, den, NULL);
	
	mpq_get_num(num, res);
	mpq_get_den(den, res);
	
	mpz_pow_ui(num, num, p);
	mpz_pow_ui(den, den, p);
	
	mpq_set_num(res, num);
	mpq_set_den(res, den);
	
	mpq_canonicalize(res);
	
	mpz_clears(num, den, NULL);
}

inline
void operate_power(mpq_t& res, const mpz_t& p) {
	mpz_t num, den;
	mpz_inits(num, den, NULL);
	
	mpq_get_num(num, res);
	mpq_get_den(den, res);
	
	gmp_utils::mpz_pow_mpz(num, num, p);
	gmp_utils::mpz_pow_mpz(den, den, p);
	
	mpq_set_num(res, num);
	mpq_set_den(res, den);
	
	mpq_canonicalize(res);
	
	mpz_clears(num, den, NULL);
}

/// PRIVATE

/// PUBLIC

rational::rational() {
	initialized = false;
}

rational::rational(int n, unsigned int d) {
	initialized = false;
	init_si(n, d);
}

rational::rational(const char *s, int base) {
	initialized = false;
	init(s, base);
}

rational::rational(const string& s, int base) {
	initialized = false;
	init(s, base);
}

rational::rational(const integer& n, const integer& d) {
	initialized = false;
	init(n, d);
}

rational::rational(const rational& r, const integer& i) {
	initialized = false;
	init(r, i);
}

rational::rational(const integer& i, const rational& r) {
	initialized = false;
	init(i, r);
}

rational::rational(const rational& r1, const rational& r2) {
	initialized = false;
	init(r1, r2);
}

rational::rational(const rational& r) {
	initialized = false;
	*this = r;
}

rational::~rational() {
	clear();
}

/* ALLOC AND DEALLOC */

void rational::init() {
	if (!is_initialized()) {
		mpq_init(val);
		initialized = true;
	}
}

void rational::init_si(int n, unsigned int d) {
	init();
	set_si(n, d);
}

void rational::init_ui(unsigned int n, unsigned int d) {
	init();
	set_ui(n, d);
}

void rational::init(const char *s, int base) {
	init();
	set(s, base);
}

void rational::init(const string& s, int base) {
	init();
	set(s, base);
}

void rational::init(const integer& n, const integer& d) {
	if (n.is_initialized() and d.is_initialized()) {
		init();
		set(n, d);
	}
}

void rational::init(const rational& r, const integer& i) {
	if (r.is_initialized() and i.is_initialized()) {
		init();
		set(r/i);
	}
}

void rational::init(const integer& i, const rational& r) {
	if (i.is_initialized() and r.is_initialized()) {
		init();
		set(r*i);
	}
}

void rational::init(const rational& r1, const rational& r2) {
	if (r1.is_initialized() and r2.is_initialized()) {
		init();
		set(r1/r2);
	}
}

void rational::init(const rational& r) {
	if (r.is_initialized()) {
		init();
		set(r);
	}
}

void rational::clear() {
	if (is_initialized()) {
		mpq_clear(val);
		initialized = false;
	}
}

/* SET VALUE */

void rational::set_si(int n, unsigned int d) 			{ mpq_set_si(val, n, d); }
void rational::set_ui(unsigned int n, unsigned int d) 	{ mpq_set_si(val, n, d); }
void rational::set(const char *s, int base) 			{ mpq_set_str(val, s, base); }
void rational::set(const string& s, int base)			{ mpq_set_str(val, s.c_str(), base); }
void rational::set(const integer& n, const integer& d)	{ set(from_ints_to_rat(n, d)); }
void rational::set(const rational& r) 					{ mpq_set(val, r.val); }

void rational::invert() {
	mpq_inv(val, val);
}

/* OPERATORS */

rational& rational::operator= (int i) {
	if (is_initialized()) set_si(i);
	else init_si(i);
	return *this;
}

rational& rational::operator= (const char *s) {
	if (is_initialized()) set(s, 10);
	else init(s, 10);
	return *this;
}

rational& rational::operator= (const string& s) {
	if (is_initialized()) set(s, 10);
	else init(s, 10);
	return *this;
}

rational& rational::operator= (const integer& i) {
	init(from_ints_to_rat(i, 1));
}

rational& rational::operator= (const rational& r) {
	if (r.is_initialized()) {
		if (is_initialized()) set(r);
		else init(r);
	}
	return *this;
}

bool rational::operator== (int i) const					{ rational r(i); 	return mpq_cmp(val, r.val) == 0; }
bool rational::operator== (const char *s) const			{ rational r(s); 	return mpq_cmp(val, r.val) == 0; }
bool rational::operator== (const string& s) const		{ rational r(s); 	return mpq_cmp(val, r.val) == 0; }
bool rational::operator== (const integer& i) const		{ rational r(i); 	return mpq_cmp(val, r.val) == 0; }
bool rational::operator== (const rational& r) const		{					return mpq_cmp(val, r.val) == 0; }

bool rational::operator!= (int i) const					{ return not (*this == i); }
bool rational::operator!= (const char *s) const			{ return not (*this == s); }
bool rational::operator!= (const string& s) const		{ return not (*this == s); }
bool rational::operator!= (const integer& i) const		{ return not (*this == i); }
bool rational::operator!= (const rational& r) const		{ return not (*this == r); }

bool rational::operator< (int i) const					{ rational r(i); 	return mpq_cmp(val, r.val) < 0; }
bool rational::operator< (const char *s) const			{ rational r(s); 	return mpq_cmp(val, r.val) < 0; }
bool rational::operator< (const string& s) const		{ rational r(s); 	return mpq_cmp(val, r.val) < 0; }
bool rational::operator< (const integer& i) const		{ rational r(i); 	return mpq_cmp(val, r.val) < 0; }
bool rational::operator< (const rational& r) const		{					return mpq_cmp(val, r.val) < 0; }

bool rational::operator<= (int i) const					{ rational r(i); 	return mpq_cmp(val, r.val) <= 0; }
bool rational::operator<= (const char *s) const			{ rational r(s); 	return mpq_cmp(val, r.val) <= 0; }
bool rational::operator<= (const string& s) const		{ rational r(s); 	return mpq_cmp(val, r.val) <= 0; }
bool rational::operator<= (const integer& i) const		{ rational r(i); 	return mpq_cmp(val, r.val) <= 0; }
bool rational::operator<= (const rational& r) const		{ 					return mpq_cmp(val, r.val) <= 0; }

bool rational::operator> (int i) const					{ rational r(i); 	return mpq_cmp(val, r.val) > 0; }
bool rational::operator> (const char *s) const			{ rational r(s); 	return mpq_cmp(val, r.val) > 0; }
bool rational::operator> (const string& s) const		{ rational r(s); 	return mpq_cmp(val, r.val) > 0; }
bool rational::operator> (const integer& i) const		{ rational r(i); 	return mpq_cmp(val, r.val) > 0; }
bool rational::operator> (const rational& r) const		{ 					return mpq_cmp(val, r.val) > 0; }

bool rational::operator>= (int i) const					{ rational r(i); 	return mpq_cmp(val, r.val) >= 0; }
bool rational::operator>= (const char *s) const			{ rational r(s); 	return mpq_cmp(val, r.val) >= 0; }
bool rational::operator>= (const string& s) const		{ rational r(s); 	return mpq_cmp(val, r.val) >= 0; }
bool rational::operator>= (const integer& i) const		{ rational r(i); 	return mpq_cmp(val, r.val) >= 0; }
bool rational::operator>= (const rational& r) const		{					return mpq_cmp(val, r.val) >= 0; }

rational rational::operator+ (int i) const				{ rational r(*this); r += i; return r; }
rational rational::operator+ (const char *s) const		{ rational r(*this); r += s; return r; }
rational rational::operator+ (const string& s) const	{ rational r(*this); r += s; return r; }
rational rational::operator+ (const integer& i) const	{ rational r(*this); r += i; return r; }
rational rational::operator+ (const rational& r) const	{ rational k(*this); k += r; return k; }

rational& rational::operator+= (int i)					{ rational r(i); 	mpq_add(val, val, r.val); return *this; }
rational& rational::operator+= (const char *s)			{ rational r(s); 	mpq_add(val, val, r.val); return *this; }
rational& rational::operator+= (const string& s)		{ rational r(s); 	mpq_add(val, val, r.val); return *this; }
rational& rational::operator+= (const integer& i)		{ rational r(i); 	mpq_add(val, val, r.val); return *this; }
rational& rational::operator+= (const rational& r)		{					mpq_add(val, val, r.val); return *this; }

rational rational::operator- () const 					{ rational r(*this); 		mpq_neg(r.val, r.val); return r; }
rational rational::operator- (int i) const				{ rational r(*this); r -= i; return r; }
rational rational::operator- (const char *s) const		{ rational r(*this); r -= s; return r; }
rational rational::operator- (const string& s) const	{ rational r(*this); r -= s; return r; }
rational rational::operator- (const integer& i) const	{ rational r(*this); r -= i; return r; }
rational rational::operator- (const rational& r) const	{ rational k(*this); k -= r; return r; }

rational& rational::operator- ()						{					mpq_neg(val, val); return *this; }
rational& rational::operator-= (int i)					{ rational r(i);	mpq_sub(val, val, r.val); return *this; }
rational& rational::operator-= (const char *s)			{ rational r(s);	mpq_sub(val, val, r.val); return *this; }
rational& rational::operator-= (const string& s)		{ rational r(s);	mpq_sub(val, val, r.val); return *this; }
rational& rational::operator-= (const integer& i)		{ rational r(i);	mpq_sub(val, val, r.val); return *this; }
rational& rational::operator-= (const rational& r)		{					mpq_sub(val, val, r.val); return *this; }

rational rational::operator* (int i) const				{ rational r(*this); r *= i; return r; }
rational rational::operator* (const char *s) const		{ rational r(*this); r *= s; return r; }
rational rational::operator* (const string& s) const	{ rational r(*this); r *= s; return r; }
rational rational::operator* (const integer& i) const	{ rational r(*this); r *= i; return r; }
rational rational::operator* (const rational& r) const	{ rational k(*this); k *= r; return k; }

rational& rational::operator*= (int i)					{ rational r(i);	mpq_mul(val, val, r.val); return *this; }
rational& rational::operator*= (const char *s)			{ rational r(s);	mpq_mul(val, val, r.val); return *this; }
rational& rational::operator*= (const string& s)		{ rational r(s);	mpq_mul(val, val, r.val); return *this; }
rational& rational::operator*= (const integer& i)		{ rational r(i);	mpq_mul(val, val, r.val); return *this; }
rational& rational::operator*= (const rational& r)		{					mpq_mul(val, val, r.val); return *this; }

rational rational::operator/ (int i) const				{ rational r(*this); r /= i; return r; }
rational rational::operator/ (const char *s) const		{ rational r(*this); r /= s; return r; }
rational rational::operator/ (const string& s) const	{ rational r(*this); r /= s; return r; }
rational rational::operator/ (const integer& i) const	{ rational r(*this); r /= i; return r; }
rational rational::operator/ (const rational& r) const	{ rational k(*this); k /= r; return k; }

rational& rational::operator/= (int I)					{ integer i(I);		mpz_divide_mpq(val, i.get_raw_value()); return *this; }
rational& rational::operator/= (const char *s) 			{ rational r(s);	mpq_divide_mpq(val, r.val);				return *this; }
rational& rational::operator/= (const string& s) 		{ rational r(s);	mpq_divide_mpq(val, r.val);				return *this; }
rational& rational::operator/= (const integer& i)	 	{ 					mpz_divide_mpq(val, i.get_raw_value()); return *this; }
rational& rational::operator/= (const rational& r)		{					mpq_divide_mpq(val, r.val);				return *this; }

rational rational::operator^ (unsigned int p) const {
	rational r(*this);
	operate_power(r.val, p);
	return r;
}

rational rational::operator^ (const integer& p) const {
	rational r(*this);
	operate_power(r.val, p.get_raw_value());
	return r;
}

rational& rational::operator^= (unsigned int p) {
	operate_power(val, p);
	return *this;
}

rational& rational::operator^= (const integer& p) {
	operate_power(val, p.get_raw_value());
	return *this;
}

/* GETTERS */

bool rational::is_initialized() const {
	return initialized;
}

int rational::get_sign() const {
	return mpq_sgn(val);
}

/* CONVERTERS */

string rational::to_string() const {
	string k;
	to_string(k);
	return k;
}

void rational::to_string(string& s) const {
	if (!is_initialized()) {
		s = "uninitialized";
		return;
	}
	
	char *buf = NULL;
	buf = mpq_get_str(buf, 10, val);
	s = string(buf);
	free(buf);
}

}
}
}


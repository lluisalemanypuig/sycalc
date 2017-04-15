#include "integer.hpp"

namespace sycalc {
namespace core {
namespace numeric {

/// NON-CLASS PRIVATE

/// PUBLIC

integer::integer() {
	initialized = false;
}

integer::integer(int v) {
	initialized = false;
	init_si(v);
}

integer::integer(const char *s, int base) {
	initialized = false;
	init(s, base);
}

integer::integer(const string& s, int base) {
	initialized = false;
	init(s, base);
}

integer::integer(const integer& v) {
	initialized = false;
	init(v);
}

integer::~integer() {
	clear();
}

/* ALLOC AND DEALLOC */

void integer::init() {
	if (!is_initialized()) {
		mpz_init(val);
		initialized = true;
	}
}

void integer::init_si(int v) {
	init();
	set_si(v);
}

void integer::init_ui(unsigned int v) {
	init();
	set_ui(v);
}

void integer::init(const char *s, int base) {
	init();
	set(s, base);
}

void integer::init(const string& s, int base) {
	init();
	set(s, base);
}

void integer::init(const integer& v) {
	if (v.is_initialized()) {
		init();
		set(v);
	}
}

void integer::clear() {
	if (is_initialized()) {
		mpz_clear(val);
		initialized = false;
	}
}

/* SET VALUE */

void integer::set_si(int v)					{ mpz_set_si(val, v); }
void integer::set_ui(unsigned int v)		{ mpz_set_ui(val, v); }
void integer::set(const char *s, int b)		{ mpz_set_str(val, s, b); }
void integer::set(const string& s, int b)	{ mpz_set_str(val, s.c_str(), b); }
void integer::set(const integer& v)			{ mpz_set(val, v.val); }

/* OPERATORS */

integer& integer::operator= (int v) {
	if (is_initialized()) set_si(v);
	else init(v);
	return *this;
}

integer& integer::operator= (const char *s) {
	if (is_initialized()) set(s);
	else init(s);
	return *this;
}

integer& integer::operator= (const string& s) {
	if (is_initialized()) set(s);
	else init(s);
	return *this;
}

integer& integer::operator= (const integer& v) {
	if (v.is_initialized()) {
		if (is_initialized()) set(v);
		else init(v);
	}
	return *this;
}

bool integer::operator== (int v) const 				{ 				return mpz_cmp_si(val, v) == 0; }
bool integer::operator== (const char *s) const		{ integer k(s);	return mpz_cmp(val, k.val) == 0; }
bool integer::operator== (const string& s) const	{ integer k(s);	return mpz_cmp(val, k.val) == 0; }
bool integer::operator== (const integer& v) const 	{ 				return mpz_cmp(val, v.val) == 0; }

bool integer::operator!= (int v) const				{ return not (*this == v); }
bool integer::operator!= (const char *s) const		{ return not (*this == s); }
bool integer::operator!= (const string& s) const	{ return not (*this == s); }
bool integer::operator!= (const integer& v) const	{ return not (*this == v); }

bool integer::operator< (int v) const 				{ 				return mpz_cmp_si(val, v) < 0; }
bool integer::operator< (const char *s) const		{ integer k(s);	return mpz_cmp(val, k.val) < 0; }
bool integer::operator< (const string& s) const		{ integer k(s); return mpz_cmp(val, k.val) < 0; }
bool integer::operator< (const integer& v) const 	{ 				return mpz_cmp(val, v.val) < 0; }

bool integer::operator<= (int v) const 				{ 				return mpz_cmp_si(val, v) <= 0; }
bool integer::operator<= (const char *s) const		{ integer k(s); return mpz_cmp(val, k.val) <= 0; }
bool integer::operator<= (const string& s) const	{ integer k(s); return mpz_cmp(val, k.val) <= 0; }
bool integer::operator<= (const integer& v) const 	{ 				return mpz_cmp(val, v.val) <= 0; }

bool integer::operator> (int v) const 				{ 				return mpz_cmp_si(val, v) > 0; }
bool integer::operator> (const char *s) const		{ integer k(s); return mpz_cmp(val, k.val) > 0; }
bool integer::operator> (const string& s) const		{ integer k(s); return mpz_cmp(val, k.val) > 0; }
bool integer::operator> (const integer& v) const 	{ 				return mpz_cmp(val, v.val) > 0; }

bool integer::operator>= (int v) const 				{ 				return mpz_cmp_si(val, v) >= 0; }
bool integer::operator>= (const char *s) const		{ integer k(s); return mpz_cmp(val, k.val) >= 0; }
bool integer::operator>= (const string& s) const	{ integer k(s); return mpz_cmp(val, k.val) >= 0; }
bool integer::operator>= (const integer& v) const 	{ 				return mpz_cmp(val, v.val) >= 0; }

integer integer::operator+ (unsigned int v) const	{ integer a(*this); 		mpz_add_ui(a.val, a.val, v); return a; }
integer integer::operator+ (const char *s) const	{ integer a(*this), k(s); 	mpz_add(a.val, a.val, k.val); return a; }
integer integer::operator+ (const string& s) const	{ integer a(*this), k(s); 	mpz_add(a.val, a.val, k.val); return a; }
integer integer::operator+ (const integer& v) const	{ integer a(*this); 		mpz_add(a.val, a.val, v.val); return a; }

integer& integer::operator+= (unsigned int v) 	{ 				mpz_add_ui(val, val, v); return *this; }
integer& integer::operator+= (const char *s)	{ integer k(s); mpz_add(val, val, k.val); return *this; }
integer& integer::operator+= (const string& s)	{ integer k(s); mpz_add(val, val, k.val); return *this; }
integer& integer::operator+= (const integer& v)	{ 				mpz_add(val, val, v.val); return *this; }

integer integer::operator- () const					{ integer a(*this);			mpz_neg(a.val, a.val); return a; }
integer integer::operator- (unsigned int v) const	{ integer a(*this); 		mpz_sub_ui(a.val, a.val, v); return a; }
integer integer::operator- (const char *s) const	{ integer a(*this), k(s); 	mpz_sub(a.val, a.val, k.val); return a; }
integer integer::operator- (const string& s) const	{ integer a(*this), k(s); 	mpz_sub(a.val, a.val, k.val); return a; }
integer integer::operator- (const integer& v) const	{ integer a(*this); 		mpz_sub(a.val, a.val, v.val); return a; }

integer& integer::operator- ()					{ 				mpz_neg(val, val); return *this; }
integer& integer::operator-= (unsigned int v)	{ 				mpz_sub_ui(val, val, v); return *this; }
integer& integer::operator-= (const char *s)	{ integer k(s);	mpz_sub(val, val, k.val); return *this; }
integer& integer::operator-= (const string& s)	{ integer k(s); mpz_sub(val, val, k.val); return *this; }
integer& integer::operator-= (const integer& v)	{ 				mpz_sub(val, val, v.val); return *this; }

integer integer::operator* (int v) const			{ integer a(*this);			mpz_mul_ui(a.val, a.val, v); return a; }
integer integer::operator* (const char *s) const	{ integer a(*this), k(s);	mpz_mul(a.val, a.val, k.val); return a; }
integer integer::operator* (const string& s) const	{ integer a(*this), k(s); 	mpz_mul(a.val, a.val, k.val); return a; }
integer integer::operator* (const integer& v) const	{ integer a(*this);			mpz_mul(a.val, a.val, v.val); return a; }

integer& integer::operator*= (int v)				{ 				mpz_mul_ui(val, val, v); return *this; }
integer& integer::operator*= (const char *s)		{ integer k(s); mpz_mul(val, val, k.val); return *this; }
integer& integer::operator*= (const string& s)		{ integer k(s);	mpz_mul(val, val, k.val); return *this; }
integer& integer::operator*= (const integer& v)		{ 				mpz_mul(val, val, v.val); return *this; }

integer integer::operator^ (unsigned int v)	 const	{ integer a(*this); mpz_pow_ui(a.val, a.val, v); return a; }
integer integer::operator^ (const integer& i) const {
	integer r;
	r.init();
	gmp_utils::mpz_pow_mpz(r.val, val, i.val);
	return r;
}

integer& integer::operator^= (unsigned int i) {
	mpz_pow_ui(val, val, i); return *this;
}

integer& integer::operator^= (const integer& i) {
	gmp_utils::mpz_pow_mpz(val, val, i.val);
	return *this;
}

/* GETTERS */

bool integer::is_initialized() const {
	return initialized;
}

int integer::get_sign() const {
	return mpz_sgn(val);
}

const mpz_t& integer::get_raw_value() const	{
	return val;
}

/* CONVERTERS */

string integer::to_string() const {
	string k;
	to_string(k);
	return k;
}

void integer::to_string(string& s) const {
	if (!is_initialized()) {
		s = "uninitialized";
		return;
	}
	
	char *buf = NULL;
	buf = mpz_get_str(buf, 10, val);
	s = string(buf);
	free(buf);
}

}
}
}


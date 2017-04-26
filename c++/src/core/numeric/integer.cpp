#include "integer.hpp"

namespace sycalc {
namespace core {
namespace numeric {

/// NON-CLASS PRIVATE

/// PUBLIC

integer::integer() {
	initialized = false;
}

integer::integer(int i) {
	initialized = false;
	init_si(i);
}

integer::integer(const char *s, int base) {
	initialized = false;
	init(s, base);
}

integer::integer(const string& s, int base) {
	initialized = false;
	init(s, base);
}

integer::integer(const integer& i) {
	initialized = false;
	*this = i;
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

void integer::init_si(int i) {
	init();
	set_si(i);
}

void integer::init_ui(unsigned int i) {
	init();
	set_ui(i);
}

void integer::init(const char *s, int base) {
	init();
	set(s, base);
}

void integer::init(const string& s, int base) {
	init();
	set(s, base);
}

void integer::init(const integer& i) {
	if (i.is_initialized()) {
		init();
		set(i);
	}
}

void integer::clear() {
	if (is_initialized()) {
		mpz_clear(val);
		initialized = false;
	}
}

/* SET VALUE */

void integer::set_si(int i)				{ mpz_set_si(val, i); }
void integer::set_ui(unsigned int i)		{ mpz_set_ui(val, i); }
void integer::set(const char *s, int b)	{ mpz_set_str(val, s, b); }
void integer::set(const string& s, int b)	{ mpz_set_str(val, s.c_str(), b); }
void integer::set(const integer& i)		{ mpz_set(val, i.val); }

/* OPERATORS */

integer& integer::operator= (int i) {
	if (is_initialized()) set_si(i);
	else init(i);
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

integer& integer::operator= (const integer& i) {
	if (i.is_initialized()) {
		if (is_initialized()) set(i);
		else init(i);
	}
	return *this;
}

bool integer::operator== (int i) const 				{ 				return mpz_cmp_si(val, i) == 0; }
bool integer::operator== (const char *s) const		{ integer k(s);	return mpz_cmp(val, k.val) == 0; }
bool integer::operator== (const string& s) const		{ integer k(s);	return mpz_cmp(val, k.val) == 0; }
bool integer::operator== (const integer& i) const 	{ 				return mpz_cmp(val, i.val) == 0; }

bool integer::operator!= (int i) const				{ return not (*this == i); }
bool integer::operator!= (const char *s) const		{ return not (*this == s); }
bool integer::operator!= (const string& s) const		{ return not (*this == s); }
bool integer::operator!= (const integer& i) const	{ return not (*this == i); }

bool integer::operator< (int i) const 				{ 				return mpz_cmp_si(val, i)  < 0; }
bool integer::operator< (const char *s) const		{ integer k(s);	return mpz_cmp(val, k.val) < 0; }
bool integer::operator< (const string& s) const		{ integer k(s); return mpz_cmp(val, k.val) < 0; }
bool integer::operator< (const integer& i) const 	{ 				return mpz_cmp(val, i.val) < 0; }

bool integer::operator<= (int i) const 				{ 				return mpz_cmp_si(val, i)  <= 0; }
bool integer::operator<= (const char *s) const		{ integer k(s); return mpz_cmp(val, k.val) <= 0; }
bool integer::operator<= (const string& s) const		{ integer k(s); return mpz_cmp(val, k.val) <= 0; }
bool integer::operator<= (const integer& i) const 	{ 				return mpz_cmp(val, i.val) <= 0; }

bool integer::operator> (int i) const 				{ 				return mpz_cmp_si(val, i)  > 0; }
bool integer::operator> (const char *s) const		{ integer k(s); return mpz_cmp(val, k.val) > 0; }
bool integer::operator> (const string& s) const		{ integer k(s); return mpz_cmp(val, k.val) > 0; }
bool integer::operator> (const integer& i) const 	{ 				return mpz_cmp(val, i.val) > 0; }

bool integer::operator>= (int i) const 				{ 				return mpz_cmp_si(val, i)  >= 0; }
bool integer::operator>= (const char *s) const		{ integer k(s); return mpz_cmp(val, k.val) >= 0; }
bool integer::operator>= (const string& s) const		{ integer k(s); return mpz_cmp(val, k.val) >= 0; }
bool integer::operator>= (const integer& i) const 	{ 				return mpz_cmp(val, i.val) >= 0; }

integer integer::operator+ (unsigned int i) const	{ integer a(*this); a += i;					return a; }
integer integer::operator+ (const char *s) const		{ integer a(*this); a += s;					return a; }
integer integer::operator+ (const string& s) const	{ integer a(*this); a += s;					return a; }
integer integer::operator+ (const integer& i) const	{ integer a(*this); a += i;					return a; }

integer& integer::operator+= (unsigned int i) 		{ 				mpz_add_ui(val, val, i);	return *this; }
integer& integer::operator+= (const char *s)			{ integer k(s); mpz_add(val, val, k.val);	return *this; }
integer& integer::operator+= (const string& s)		{ integer k(s); mpz_add(val, val, k.val);	return *this; }
integer& integer::operator+= (const integer& i)		{ 				mpz_add(val, val, i.val);	return *this; }

integer integer::operator- () const					{ integer a(*this);	mpz_neg(a.val, a.val);	return a; }
integer integer::operator- (unsigned int i) const	{ integer a(*this); a -= i;					return a; }
integer integer::operator- (const char *s) const		{ integer a(*this); a -= s;					return a; }
integer integer::operator- (const string& s) const	{ integer a(*this); a -= s;					return a; }
integer integer::operator- (const integer& i) const	{ integer a(*this); a -= i;					return a; }

integer& integer::operator- ()							{ 				mpz_neg(val, val);			return *this; }
integer& integer::operator-= (unsigned int i)			{ 				mpz_sub_ui(val, val, i);	return *this; }
integer& integer::operator-= (const char *s)			{ integer k(s);	mpz_sub(val, val, k.val);	return *this; }
integer& integer::operator-= (const string& s)		{ integer k(s); mpz_sub(val, val, k.val);	return *this; }
integer& integer::operator-= (const integer& i)		{ 				mpz_sub(val, val, i.val);	return *this; }

integer integer::operator* (int i) const				{ integer a(*this); a *= i;					return a; }
integer integer::operator* (const char *s) const		{ integer a(*this); a *= s;					return a; }
integer integer::operator* (const string& s) const	{ integer a(*this); a *= s;					return a; }
integer integer::operator* (const integer& i) const	{ integer a(*this); a *= i;					return a; }

integer& integer::operator*= (int i)					{ 				mpz_mul_ui(val, val, i);	return *this; }
integer& integer::operator*= (const char *s)			{ integer k(s); mpz_mul(val, val, k.val);	return *this; }
integer& integer::operator*= (const string& s)		{ integer k(s);	mpz_mul(val, val, k.val);	return *this; }
integer& integer::operator*= (const integer& i)		{ 				mpz_mul(val, val, i.val);	return *this; }

integer integer::operator/ (int i) const				{ integer a(*this); a /= i;					return a; }
integer integer::operator/ (const char *s) const		{ integer a(*this); a /= s;					return a; }
integer integer::operator/ (const string& s) const	{ integer a(*this); a /= s;					return a; }
integer integer::operator/ (const integer& i) const	{ integer a(*this); a /= i;					return a; }

integer& integer::operator/= (int i)					{ 				mpz_div_ui(val, val, i);	return *this; }
integer& integer::operator/= (const char *s)			{ integer k(s); mpz_div(val, val, k.val);	return *this; }
integer& integer::operator/= (const string& s)		{ integer k(s); mpz_div(val, val, k.val);	return *this; }
integer& integer::operator/= (const integer& i)		{ 				mpz_div(val, val, i.val);	return *this; }

integer integer::operator^ (unsigned int i)	 const	{ integer r(*this); r ^= i;					return r; }
integer integer::operator^ (const integer& i) const 	{ integer r(*this); r ^= i;					return r; }

integer& integer::operator^= (unsigned int i) {
	mpz_pow_ui(val, val, i);
	return *this;
}

integer& integer::operator^= (const integer& i) {
	gmp_utils::mpz_pow_mpz(val, val, i.val);
	return *this;
}

integer integer::operator% (const integer& i) const {
	integer r;
	r.init();
	mpz_mod(r.val, val, i.val);
	return r;
}

unsigned int integer::operator% (unsigned int i) const {
	mpz_t r;
	mpz_init(r);
	unsigned int m = mpz_mod_ui(r, val, i);
	mpz_clear(r);
	return m;
}

integer& integer::operator++ () {
   *this += 1;
   return *this;
}

integer integer::operator++ (int)
{
   integer r(*this);
   ++(*this);
   return r;
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
		s = "integer uninitialized";
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


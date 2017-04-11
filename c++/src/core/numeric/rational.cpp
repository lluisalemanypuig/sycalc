#include "rational.hpp"

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

rational::rational(const rational& v) {
	initialized = false;
	init(v);
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

void rational::init(const rational& v) {
	if (v.is_initialized()) {
		init();
		set(v);
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
void rational::set(const char *s, int base) 				{ mpq_set_str(val, s, base); }
void rational::set(const string& s, int base)				{ mpq_set_str(val, s.c_str(), base); }
void rational::set(const rational& v) 					{ mpq_set(val, v.val); }

/* OPERATORS */

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

rational& rational::operator= (const rational& v) {
	if (v.is_initialized()) {
		if (is_initialized()) set(v);
		else init(v);
	}
	return *this;
}


bool rational::operator== (const char *s) const			{ rational r(s); 	return mpq_cmp(val, r.val) == 0; }
bool rational::operator== (const string& s) const		{ rational r(s); 	return mpq_cmp(val, r.val) == 0; }
bool rational::operator== (const rational& v) const	{					return mpq_cmp(val, v.val) == 0; }

bool rational::operator!= (const char *s) const			{ rational r(s); 	return mpq_cmp(val, r.val) != 0; }
bool rational::operator!= (const string& s) const		{ rational r(s); 	return mpq_cmp(val, r.val) != 0; }
bool rational::operator!= (const rational& v) const	{ 					return mpq_cmp(val, v.val) != 0; }

bool rational::operator< (const char *s) const			{ rational r(s); 	return mpq_cmp(val, r.val) < 0; }
bool rational::operator< (const string& s) const		{ rational r(s); 	return mpq_cmp(val, r.val) < 0; }
bool rational::operator< (const rational& v) const		{					return mpq_cmp(val, v.val) < 0; }

bool rational::operator<= (const char *s) const			{ rational r(s); 	return mpq_cmp(val, r.val) <= 0; }
bool rational::operator<= (const string& s) const		{ rational r(s); 	return mpq_cmp(val, r.val) <= 0; }
bool rational::operator<= (const rational& v) const	{ 					return mpq_cmp(val, v.val) <= 0; }

bool rational::operator> (const char *s) const			{ rational r(s); 	return mpq_cmp(val, r.val) > 0; }
bool rational::operator> (const string& s) const		{ rational r(s); 	return mpq_cmp(val, r.val) > 0; }
bool rational::operator> (const rational& v) const		{ 					return mpq_cmp(val, v.val) > 0; }

bool rational::operator>= (const char *s) const			{ rational r(s); 	return mpq_cmp(val, r.val) >= 0; }
bool rational::operator>= (const string& s) const		{ rational r(s); 	return mpq_cmp(val, r.val) >= 0; }
bool rational::operator>= (const rational& v) const	{					return mpq_cmp(val, v.val) >= 0; }

rational rational::operator+ (const char *s) const			{ rational a(*this), k(s); 	mpq_add(a.val, a.val, k.val); return a; }
rational rational::operator+ (const string& s) const		{ rational a(*this), k(s); 	mpq_add(a.val, a.val, k.val); return a; }
rational rational::operator+ (const rational& v) const	{ rational a(*this);			mpq_add(a.val, a.val, v.val); return a; }

rational& rational::operator+= (const char *s)		{ rational k(s); 	mpq_add(val, val, k.val); return *this; }
rational& rational::operator+= (const string& s)	{ rational k(s); 	mpq_add(val, val, k.val); return *this; }
rational& rational::operator+= (const rational& v)	{					mpq_add(val, val, v.val); return *this; }

rational rational::operator- () const 						{ rational a(*this); 		mpq_neg(a.val, a.val); return a; }
rational rational::operator- (const char *s) const			{ rational a(*this), k(s);	mpq_sub(a.val, a.val, k.val); return a; }
rational rational::operator- (const string& s) const		{ rational a(*this), k(s);	mpq_sub(a.val, a.val, k.val); return a; }
rational rational::operator- (const rational& v) const	{ rational a(*this); 		mpq_sub(a.val, a.val, v.val); return a; }

rational& rational::operator- ()						{					mpq_neg(val, val); return *this; }
rational& rational::operator-= (const char *s)		{ rational k(s);	mpq_sub(val, val, k.val); return *this; }
rational& rational::operator-= (const string& s)	{ rational k(s);	mpq_sub(val, val, k.val); return *this; }
rational& rational::operator-= (const rational& v)	{					mpq_sub(val, val, v.val); return *this; }

rational rational::operator* (const char *s) const			{ rational a(*this), k(s); 	mpq_mul(a.val, a.val, k.val); return a; }
rational rational::operator* (const string& s) const		{ rational a(*this), k(s); 	mpq_mul(a.val, a.val, k.val); return a; }
rational rational::operator* (const rational& v) const	{ rational a(*this);			mpq_mul(a.val, a.val, v.val); return a; }

rational& rational::operator*= (const char *s)		{ rational k(s);	mpq_mul(val, val, k.val); return *this; }
rational& rational::operator*= (const string& s)	{ rational k(s);	mpq_mul(val, val, k.val); return *this; }
rational& rational::operator*= (const rational& v)	{					mpq_mul(val, val, v.val); return *this; }

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


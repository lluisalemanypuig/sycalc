#pragma once

/// C includes
#include <stdlib.h>
#include <gmp.h>

/// C++ includes
#include <fstream>
#include <string>
using namespace std;

/// Custom includes
#include "integer.hpp"

namespace sycalc {
namespace core {
namespace numeric {

class rational {
	private:
		mpq_t val;
		bool initialized;
	
	public:
		rational();
		rational(int n, unsigned int d = 1);
		rational(const char *s, int base = 10);
		rational(const string& s, int base = 10);
		rational(const rational& v);
		~rational();
		
		/* ALLOC AND DEALLOC */
		
		void init();
		void init_si(int n, unsigned int d = 1);
		void init_ui(unsigned int n, unsigned int d = 1);
		void init(const char *s, int base = 10);
		void init(const string& s, int base = 10);
		void init(const rational& v);
		
		void clear();
		
		/* SET VALUE */
		
		void set_si(int v, unsigned int d = 1);
		void set_ui(unsigned int n, unsigned int d = 1);
		void set(const char *s, int base = 10);
		void set(const string& s, int base = 10);
		void set(const rational& v);
		
		/* OPERATORS */
		
		rational& operator= (const char *s);
		rational& operator= (const string& s);
		rational& operator= (const rational& v);
		
		bool operator== (const char *s) const;
		bool operator== (const string& s) const;
		bool operator== (const rational& v) const;
		
		bool operator!= (const char *s) const;
		bool operator!= (const string& s) const;
		bool operator!= (const rational& v) const;
		
		bool operator< (const char *s) const;
		bool operator< (const string& s) const;
		bool operator< (const rational& v) const;
		
		bool operator<= (const char *s) const;
		bool operator<= (const string& s) const;
		bool operator<= (const rational& v) const;
		
		bool operator> (const char *s) const;
		bool operator> (const string& s) const;
		bool operator> (const rational& v) const;
		
		bool operator>= (const char *s) const;
		bool operator>= (const string& s) const;
		bool operator>= (const rational& v) const;
		
		rational operator+ (const char *s) const;
		rational operator+ (const string& s) const;
		rational operator+ (const rational& v) const;
		
		rational& operator+= (const char *s);
		rational& operator+= (const string& s);
		rational& operator+= (const rational& v);
		
		rational operator- () const;
		rational operator- (const char *s) const;
		rational operator- (const string& s) const;
		rational operator- (const rational& v) const;
		
		rational& operator- ();
		rational& operator-= (const char *s);
		rational& operator-= (const string& s);
		rational& operator-= (const rational& v);
		
		rational operator* (const char *s) const;
		rational operator* (const string& s) const;
		rational operator* (const rational& v) const;
		
		rational& operator*= (const char *s);
		rational& operator*= (const string& s);
		rational& operator*= (const rational& v);
		
		rational operator^ (unsigned int p) const;
		rational operator^ (const integer& p) const;
		
		rational& operator^= (unsigned int p);
		rational& operator^= (const integer& p);
		
		inline friend
		istream& operator>> (istream& is, rational& r) {
			string s;
			is >> s;
			r.set(s, 10);
			return is;
		}
		
		inline friend
		ostream& operator<< (ostream& os, const rational& v) {
			os << v.to_string();
			return os;
		}
		
		/* GETTERS */
		
		bool is_initialized() const;
		int get_sign() const;
		
		/* CONVERTERS */
		
		string to_string() const;
		void to_string(string& s) const;
};

}
}
}


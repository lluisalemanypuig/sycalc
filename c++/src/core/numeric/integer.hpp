#pragma once

/// C includes
#include <stdlib.h>
#include <gmp.h>

/// C++ includes
#include <fstream>
#include <string>
using namespace std;

/// Custom includes
#include "gmp_utils.hpp"

namespace sycalc {
namespace core {
namespace numeric {

class integer {
	private:
		mpz_t val;
		bool initialized;
	
	public:
		integer();
		integer(int i);
		integer(const char *s, int base = 10);
		integer(const string& s, int base = 10);
		integer(const integer& v);
		~integer();
		
		/* ALLOC AND DEALLOC */
		
		void init();
		void init_si(int i);
		void init_ui(unsigned int i);
		void init(const char *s, int base = 10);
		void init(const string& s, int base = 10);
		void init(const integer& v);
		
		void clear();
		
		/* SET VALUE */
		
		void set_si(int i);
		void set_ui(unsigned int i);
		void set(const char *s, int base = 10);
		void set(const string& v, int base = 10);
		void set(const integer& v);
		
		/* OPERATORS */
		
		integer& operator= (int i);
		integer& operator= (const char *s);
		integer& operator= (const string& v);
		integer& operator= (const integer& v);
		
		bool operator== (int i) const;
		bool operator== (const char *s) const;
		bool operator== (const string& s) const;
		bool operator== (const integer& v) const;
		
		bool operator!= (int i) const;
		bool operator!= (const char *s) const;
		bool operator!= (const string& s) const;
		bool operator!= (const integer& v) const;
		
		bool operator< (int i) const;
		bool operator< (const char *s) const;
		bool operator< (const string& s) const;
		bool operator< (const integer& v) const;
		
		bool operator<= (int i) const;
		bool operator<= (const char *s) const;
		bool operator<= (const string& s) const;
		bool operator<= (const integer& v) const;
		
		bool operator> (int i) const;
		bool operator> (const char *s) const;
		bool operator> (const string& s) const;
		bool operator> (const integer& v) const;
		
		bool operator>= (int i) const;
		bool operator>= (const char *s) const;
		bool operator>= (const string& s) const;
		bool operator>= (const integer& v) const;
		
		integer operator+ (unsigned int i) const;
		integer operator+ (const char *s) const;
		integer operator+ (const string& s) const;
		integer operator+ (const integer& v) const;
		
		integer& operator+= (unsigned int i);
		integer& operator+= (const char *s);
		integer& operator+= (const string& s);
		integer& operator+= (const integer& v);
		
		integer operator- () const;
		integer operator- (unsigned int i) const;
		integer operator- (const char *s) const;
		integer operator- (const string& s) const;
		integer operator- (const integer& v) const;
		
		integer& operator- ();
		integer& operator-= (unsigned int i);
		integer& operator-= (const char *s);
		integer& operator-= (const string& s);
		integer& operator-= (const integer& v);
		
		integer operator* (int i) const;
		integer operator* (const char *s) const;
		integer operator* (const string& s) const;
		integer operator* (const integer& v) const;
		
		integer& operator*= (int i);
		integer& operator*= (const char *s);
		integer& operator*= (const string& s);
		integer& operator*= (const integer& v);
		
		integer operator^ (unsigned int i) const;
		integer operator^ (const integer& i) const;
		
		integer& operator^= (unsigned int i);
		integer& operator^= (const integer& i);
		
		inline friend
		istream& operator>> (istream& is, integer& i) {
			string s;
			is >> s;
			i.init(s, 10);
			return is;
		}
		
		inline friend
		ostream& operator<< (ostream& os, const integer& v) {
			os << v.to_string();
			return os;
		}
		
		/* GETTERS */
		
		bool is_initialized() const;
		int get_sign() const;
		
		const mpz_t& get_raw_value() const;
		
		/* CONVERTERS */
		
		string to_string() const;
		void to_string(string& s) const;
};

}
}
}


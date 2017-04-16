#pragma once

/// C includes
#include <stdlib.h>
#include <gmp.h>

/// C++ includes
#include <utility>
#include <fstream>
#include <vector>
#include <list>
using namespace std;

/// Custom includes
#include "integer.hpp"

namespace sycalc {
namespace core {

using namespace numeric;

class sparse_pascal_triangle {
	private:
		typedef pair<int, integer> atom;
		typedef list<atom> tri_row;
		
		typedef tri_row::const_iterator citer;
		typedef tri_row::iterator iter;
		
		inline static
		iter find(tri_row& r, size_t i) {
			iter pos = r.end();
			iter it = r.begin();
			
			while (it != r.end() && pos == r.end()) {
				if (it->first == i) {
					pos = it;
				}
				++it;
			}
			
			return pos;
		}
		
		inline static
		citer cfind(const tri_row& r, size_t i) {
			citer pos = r.end();
			citer it = r.begin();
			
			while (it != r.end() && pos == r.end()) {
				if (it->first == i) {
					pos = it;
				}
				++it;
			}
			
			return pos;
		}
		
		// Returns the position where the atom should be placed at.
		// The element should be inserted to the right of this position.
		inline static
		iter desired_position(tri_row& r, size_t i) {
			iter pos = r.end();
			iter it = r.begin();
			
			while (it != r.end() and pos == r.end()) {
				if (i <= it->first) {
					pos = it;
				}
				++it;
			}
			
			return pos;
		}
		
		inline static
		void insert_atom(const atom& a, tri_row& r) {
			if (r.size() == 0) {
				// no elements on the row --> insert at beginning
				r.insert(r.begin(), a);
			}
			else {
				iter it = desired_position(r, a.first);
				r.insert(it, a);
			}
		}
		
		vector<tri_row> binom;
		
		void compute_binom(size_t n, size_t k);
		
	public:
		sparse_pascal_triangle();
		~sparse_pascal_triangle();
		
		inline friend
		ostream& operator<< (ostream& os, const sparse_pascal_triangle& sb) {
			for (size_t n = 0; n < sb.binom.size(); ++n) {
				const tri_row& r = sb.binom[n];
				
				os << n << ": ";
				for (citer it = r.begin(); it != r.end(); ++it) {
					os << "(" << it->first << " : " << it->second << ") ";
				}
				os << endl;
			}
			return os;
		}
		
		// Initializes the memory for this dense Pascal's triangle.
		// The memory reserved corresponds to those Newton's binomials
		// (m k) where 0 <= m, k < n, but more can be added after
		// initializing it.
		void init(size_t n);
		
		// Computes all unknown values of this Pascal's triangle.
		void fill();
		
		// Adds as many rows as needed to have n rows. Initializes the
		// values with -1.
		void fill_rows(size_t n);
		
		// Computes the Newton's binomial (n k), stores the value in this
		// triangle and returns the value in b. If this triangle has 'n'
		// rows, one can get the binomials (m k) where 0 <= m, k < n
		void get_binomial(size_t n, size_t k, integer& b);
		const integer& get_binomial(size_t n, size_t k);
		const integer& get_binomial(size_t n, size_t k) const;
		
		// Returns whether the binomial value corresponding to (n k)
		// exists in this triangle
		bool exists(size_t n, size_t k) const;
};

}
}


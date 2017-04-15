#pragma once

/// C includes
#include <stdlib.h>
#include <gmp.h>

/// C++ includes
#include <utility>
#include <fstream>
#include <vector>
using namespace std;

/// Custom includes
#include "integer.hpp"

namespace sycalc {
namespace core {

using namespace numeric;

class sparse_pascal_triangle {
	private:
		typedef pair<int, integer> atom;
		typedef vector<atom>::const_iterator citer;
		typedef vector<atom>::iterator iter;
		
		inline static
		iter find(vector<atom>& v, size_t i) {
			for (iter it = v.begin(); it != v.end(); ++it) {
				if (it->first == i) return it;
			}
			return v.end();
		}
		
		inline static
		citer cfind(const vector<atom>& v, size_t i) {
			for (citer it = v.begin(); it != v.end(); ++it) {
				if (it->first == i) return it;
			}
			return v.end();
		}
		
		// returns the position to the right of the position the
		// atom should be placed at
		inline static
		iter desired_position(const atom& a, vector<atom>& v) {
			iter d = v.begin();
			for (iter it = v.begin(); it != v.end() and d == v.begin(); ++it) {
				if (it->first >= a.first) d = it;
			}
			return d;
		}
		
		inline static
		void insert_atom(const atom& a, vector<atom>& v) {
			iter it = desired_position(a, v);
			if (it->first == a.first) v[a.first] = a;
			else {
				v.insert(it, a);
				swap(*it, *(it - 1));
			}
		}
		
		vector<vector<atom> > binom;
		
		void compute_binom(size_t n, size_t k);
		
	public:
		sparse_pascal_triangle();
		~sparse_pascal_triangle();
		
		inline friend
		ostream& operator<< (ostream& os, const sparse_pascal_triangle& db) {
			for (size_t n = 0; n < db.binom.size(); ++n) {
				os << db.binom[n].size() - 1 << ": ";
				for (size_t k = 0; k < db.binom[n].size(); ++k) {
					os << "(" << db.binom[n][k].first << " : " << db.binom[n][k].second.to_string() << ") ";
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
		integer& get_binomial(size_t n, size_t k);
		const integer& get_binomial(size_t n, size_t k) const;
		
		// Returns whether the binomial value corresponding to (n k)
		// exists in this triangle
		bool exists(size_t n, size_t k) const;
};

}
}


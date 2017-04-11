#pragma once

/// C includes
#include <stdlib.h>
#include <gmp.h>

/// C++ includes
#include <fstream>
#include <vector>
using namespace std;

/// Custom includes
#include "integer.hpp"

class dense_pascal_triangle {
	private:
		vector<vector<integer> > binom;
		
		void compute_binom(size_t n, size_t k);
		
	public:
		dense_pascal_triangle();
		~dense_pascal_triangle();
		
		inline friend
		ostream& operator<< (ostream& os, const dense_pascal_triangle& db) {
			for (size_t n = 0; n < db.binom.size(); ++n) {
				os << db.binom[n].size() - 1 << ": ";
				for (size_t k = 0; k < db.binom[n].size(); ++k) {
					os << db.binom[n][k].to_string() << " ";
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
		
		// Computes the Newton's binomial (n k), stores the value this
		// triangle. If this triangle has 'n' rows, one can get the
		// binomials (m k) where 0 <= m, k < n
		void store_binomial(size_t n, size_t k);
		
		// Computes the Newton's binomial (n k), stores the value this
		// triangle and returns the value in b. If this triangle has 'n'
		// rows, one can get the binomials (m k) where 0 <= m, k < n
		void get_binomial(size_t n, size_t k, integer& b);
		
		// Computes the Newton's binomial (n k), stores the value this
		// triangle and returns the value. If this triangle has 'n'
		// rows, one can get the binomials (m k) where 0 <= m, k < n
		integer& get_binomial(size_t n, size_t k);
		const integer& get_binomial(size_t n, size_t k) const;
};


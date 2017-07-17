#pragma once

/// C includes
#include <gmp.h>

/// C++ includes
#include <vector>
#include <map>
using namespace std;

/// Custom includes
#include "sparse_pascal_triangle.hpp"
#include "polynomial.hpp"
#include "integer.hpp"

namespace sycalc {
namespace algorithms {

using namespace core;
using namespace numeric;

	/// Nested sums
	
	void nested_sums(size_t n, size_t k, integer& r);
	integer nested_sums(size_t n, size_t k);
	
	void nested_sums(size_t a, size_t b, size_t k, integer& r);
	integer nested_sums(size_t a, size_t b, size_t k);
	
	/// Power sums
	
	void linear_power_sums(size_t a, size_t b, size_t p, integer& s);
	integer linear_power_sums(size_t a, size_t b, size_t p);
	
	// pre: p > 0
	void power_sums(size_t p, polynomial& s, const string& var_name = "n");
	void power_sums(size_t d, vector<polynomial>& polys, const string& var_name = "n");
	polynomial power_sums(size_t p, const string& var_name = "n");
	
	// q: sum_{i = 1}^{n} p(i)
	void polynomial_sum(const polynomial& p, polynomial& q);
	polynomial polynomial_sum(const polynomial& p);
	
}
}


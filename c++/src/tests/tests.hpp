#pragma once

/// C includes
#include <gmp.h>

/// C++ includes
#include <iostream>
using namespace std;

/// Custom includes
#include "rational.hpp"
#include "integer.hpp"

#include "dense_pascal_triangle.hpp"
#include "sparse_pascal_triangle.hpp"
#include "polynomial.hpp"
#include "monomial.hpp"

#include "algorithms.hpp"

namespace sycalc {
namespace tests {

using namespace sycalc;
using namespace core;
using namespace numeric;
	
	void triangle_tests();
	void integer_tests();
	void rational_tests();
	void monomial_tests();
	void polynomial_tests();
	void algorithms_tests();
	
	static
	void test_everything() {
		triangle_tests();
		integer_tests();
		rational_tests();
		monomial_tests();
		polynomial_tests();
		algorithms_tests();
	}
	
}
}


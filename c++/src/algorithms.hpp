#pragma once

/// C includes
#include <gmp.h>

/// Custom includes
#include "integer.hpp"

namespace sycalc {
namespace algorithms {

using namespace core;
using namespace numeric;

	/// Nested sums
	
	void recursive_nested_sums(size_t a, size_t b, size_t k, integer& r);
	
}
}


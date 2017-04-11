#include "../algorithms.hpp"

/// PRIVATE

/// PUBLIC

namespace algorithms {
	
	void recursive_nested_sums(size_t a, size_t b, size_t k, integer& s) {
		if (k == 1) {
			for (size_t i = a; i <= b; ++i) s += i;
			return;
		}
		
		for (size_t i = a; i <= b; ++i) {
			recursive_nested_sums(a, i, k - 1, s);
		}
	}
	
}


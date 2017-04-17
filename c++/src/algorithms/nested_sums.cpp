#include "../algorithms.hpp"

/// PRIVATE

/// PUBLIC

namespace sycalc {
namespace algorithms {
	
	void nested_sums(size_t n, size_t k, integer& r) {
		sparse_pascal_triangle spt;
		r = spt.get_binomial(n + k, k + 1);
	}
	
	integer nested_sums(size_t n, size_t k) {
		integer r;
		nested_sums(n, k, r);
		return r;
	}
	
	void nested_sums(size_t a, size_t b, size_t k, integer& s) {
		s = 0;
		if (k == 1) {
			for (integer i = a; i <= b; ++i) {
				s += i;
			}
		}
		else {
			for (size_t i = a; i <= b; ++i) {
				s += nested_sums(a, i, k - 1);
			}
		}
	}
	
	integer nested_sums(size_t a, size_t b, size_t k) {
		integer s;
		nested_sums(a, b, k, s);
		return s;
	}
	
}
}


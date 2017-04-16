#include "dense_pascal_triangle.hpp"

namespace sycalc {
namespace core {

/// PRIVATE

void dense_pascal_triangle::compute_binom(size_t n, size_t k) {
	if (binom[n][k] != -1) return;
	
	if (k == 0 || k == n) {
		binom[n][k] = 1;
		return;
	}
	
	compute_binom(n - 1, k);
	compute_binom(n - 1, k - 1);
	
	binom[n][k] = binom[n - 1][k] + binom[n - 1][k - 1];
}

/// PUBLIC

dense_pascal_triangle::dense_pascal_triangle() { }
dense_pascal_triangle::~dense_pascal_triangle() { }

void dense_pascal_triangle::init(size_t n) {
	binom.resize(n);
	for (size_t r = 0; r < binom.size(); ++r) {
		binom[r] = vector<integer>(r + 1, -1);
	}
	binom[0][0] = 1;
}

void dense_pascal_triangle::fill() {
	size_t n = binom.size() - 1;
	for (size_t k = 0; k < binom[n].size(); ++k) {
		compute_binom(n, k);
	}
}

void dense_pascal_triangle::fill_rows(size_t n) {
	size_t old_rows = binom.size();
	size_t to_be_added = n - binom.size() + 1;
	
	size_t idx;
	for (size_t r = 0; r < to_be_added; ++r) {
		idx = old_rows + r;
		binom.push_back(vector<integer>(idx + 1, -1));
	}
}

void dense_pascal_triangle::get_binomial(size_t n, size_t k, integer& b) {
	if (n >= binom.size()) fill_rows(n);
	
	compute_binom(n, k);
	b = binom[n][k];
}

const integer& dense_pascal_triangle::get_binomial(size_t n, size_t k) {
	if (n >= binom.size()) fill_rows(n);
	
	compute_binom(n, k);
	return binom[n][k];
}

const integer& dense_pascal_triangle::get_binomial(size_t n, size_t k) const {
	return binom[n][k];
}

}
}


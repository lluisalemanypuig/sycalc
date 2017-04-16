#include "sparse_pascal_triangle.hpp"

namespace sycalc {
namespace core {

/// PRIVATE

void sparse_pascal_triangle::compute_binom(size_t n, size_t k) {
	iter it = find(binom[n], k);
	if (it != binom[n].end() && it->second != -1) {
		return;
	}
	
	if (k == 0 || k == n) {
		if (it != binom[n].end()) {
			it->second = 1;
		}
		else {
			insert_atom(atom(k, integer(1)), binom[n]);
		}
		
		return;
	}
	
	compute_binom(n - 1, k);
	compute_binom(n - 1, k - 1);
	
	
	//integer R = binom[n - 1][k].second + binom[n - 1][k - 1].second;
	const integer& p = cfind(binom[n - 1], k)->second;		// binom[n - 1][k]
	const integer& q = cfind(binom[n - 1], k - 1)->second;	// binom[n - 1][k - 1]
	integer R = p + q;
	
	if (it != binom[n].end()) {
		it->second = R;
	}
	else {
		insert_atom(atom(k, R), binom[n]);
	}
}

/// PUBLIC

sparse_pascal_triangle::sparse_pascal_triangle() { }
sparse_pascal_triangle::~sparse_pascal_triangle() { }

void sparse_pascal_triangle::init(size_t n) {
	binom.resize(n);
	for (size_t r = 0; r < binom.size(); ++r) {
		binom[r] = tri_row();
	}
	
	insert_atom(atom(1, 1), binom[0]);
}

void sparse_pascal_triangle::fill() {
	size_t s = binom.size();
	size_t n = s - 1;
	for (size_t k = 0; k < s; ++k) {
		compute_binom(n, k);
	}
}

void sparse_pascal_triangle::fill_rows(size_t n) {
	size_t old_rows = binom.size();
	size_t to_be_added = n - binom.size() + 1;
	
	for (size_t r = 0; r < to_be_added; ++r) {
		binom.push_back(tri_row());
	}
}

void sparse_pascal_triangle::get_binomial(size_t n, size_t k, integer& b) {
	b = get_binomial(n, k);
}

const integer& sparse_pascal_triangle::get_binomial(size_t n, size_t k) {
	if (n >= binom.size()) {
		fill_rows(n);
	}
	
	compute_binom(n, k);
	return cfind(binom[n], k)->second;
}

const integer& sparse_pascal_triangle::get_binomial(size_t n, size_t k) const {
	return cfind(binom[n], k)->second;
}

bool sparse_pascal_triangle::exists(size_t n, size_t k) const {
	return cfind(binom[n], k) != binom[n].end();
}

}
}


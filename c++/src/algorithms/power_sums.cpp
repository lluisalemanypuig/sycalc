#include "algorithms.hpp"

/// PRIVATE

namespace sycalc {
namespace algorithms {
	
	inline size_t poly_idx(size_t p) { return p - 1; }
	
}
}

/// PUBLIC

namespace sycalc {
namespace algorithms {
	
	void linear_power_sums(size_t a, size_t b, size_t p, integer& s) {
		s = 0;
		integer pp = p;
		
		for (integer i = a; i <= b; ++i) {
			s += (i^pp);
		}
	}
	
	integer linear_power_sums(size_t a, size_t b, size_t p) {
		integer s;
		linear_power_sums(a, b, p, s);
		return s;
	}
	
	void power_sums(size_t p, polynomial& s) {
		vector<polynomial> polys(p, polynomial("n"));
		power_sums(p, polys);
		s = polys[p - 1];
	}
	
	void power_sums(size_t p, vector<polynomial>& polys) {
		polys[ poly_idx(1) ] += monomial("1/2", 2, "n");
		polys[ poly_idx(1) ] += monomial("1/2", 1, "n");
		
		for (size_t d = 2; d <= p; ++d) {
			
			// Polynomial for power (p - 1)
			const polynomial& last = polys[ poly_idx(d - 1) ];
			const rational& Bcoef = last.get_monomial_coefficient(d);
			
			rational new_Bcoef = Bcoef + 1;
			new_Bcoef.invert();
			
			// Polynomial for power p
			polynomial& S = polys[ poly_idx(d) ];
			S += monomial(Bcoef, d + 1, "n");
			
			rational coef1 = last.get_monomial_coefficient(0);
			rational coef2 = last.get_monomial_coefficient(1);
			for (size_t j = 0; j <= d - 1; ++j) {
				//const rational& coef1 = last.get_monomial_coefficient(j);
				//const rational& coef2 = last.get_monomial_coefficient(j + 1);
				
				S += monomial(coef1 + coef2, j + 1, "n");
				if (j > 0) {
					S -= polys[ poly_idx(j) ]*coef1;
				}
				
				coef1 = coef2;
				coef2 = last.get_monomial_coefficient(j + 2);
			}
			
			S += monomial(last.get_monomial_coefficient(0), 0, "n");
			S *= new_Bcoef;
		}
	}
	
	polynomial power_sums(size_t p) {
		polynomial s;
		power_sums(p, s);
		return s;
	}
	
}
}


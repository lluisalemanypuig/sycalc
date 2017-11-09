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
	
	void power_sums(size_t p, polynomial& s, const string& var_name) {
		vector<polynomial> polys(p, var_name);
		power_sums(p, polys, var_name);
		s = polys[ poly_idx(p) ];
	}
	
	/*
	inline
	void calculate_poly
	(
		size_t d, const string& var_name,
		vector<polynomial>& polys, polynomial& S,
		rational& new_Bcoef, rational& coef1, rational& coef2
	)
	{
		// Polynomial for power (d - 1)
		const polynomial& last = polys[ poly_idx(d - 1) ];
		const rational& Bcoef = last.get_monomial_coefficient(d);
		
		// start building polynomial for power d
		S += monomial(Bcoef, d + 1, var_name);
		
		coef2 = last.get_monomial_coefficient(1);
		S += monomial(coef2, 1, var_name);
		
		for (size_t j = 1; j <= d - 1; ++j) {
			coef1 = coef2;
			coef2 = last.get_monomial_coefficient(j + 1);
			
			S += monomial(coef1 + coef2, j + 1, var_name);
			S -= polys[ poly_idx(j) ]*coef1;
		}
		
		new_Bcoef = Bcoef + 1;
		new_Bcoef.invert();
		S *= new_Bcoef;
	}
	*/
	
	inline
	void calculate_poly
	(
		size_t d, const string& var_name,
		vector<polynomial>& polys, polynomial& S,
		rational& new_Bcoef, rational& coef1, rational& coef2
	)
	{
		// Polynomial for power (d - 1)
		const polynomial& last = polys[ poly_idx(d - 1) ];
		const rational& Bcoef = last.get_monomial_coefficient(d);
		
		polynomial C(var_name);
		C += monomial(1, 1, var_name);
		C += monomial(rational(1) - last.get_monomial_coefficient(d - 1), 0, var_name);
		
		S = last*C;
		
		for (size_t i = 1; i <= d - 2; ++i) {
			S -= polys[ poly_idx(i) ]*last.get_monomial_coefficient(i);
		}
		
		new_Bcoef = Bcoef + 1;
		new_Bcoef.invert();
		S *= new_Bcoef;
	}
	
	void power_sums(size_t p, vector<polynomial>& polys, const string& var_name) {
		polys[ poly_idx(1) ] += monomial("1/2", 2, var_name);
		polys[ poly_idx(1) ] += monomial("1/2", 1, var_name);
		
		rational new_Bcoef, coef1, coef2;
		
		for (size_t d = 2; d <= p; ++d) {
			
			calculate_poly
			(
				d, var_name, polys,
				polys[ poly_idx(d) ],
				new_Bcoef, coef1, coef2
			);
		}
		
	}
	
	void power_sums(size_t lower, size_t p, vector<polynomial>& polys, const string& var_name) {
		if (lower == 1) {
			polys[ poly_idx(1) ] += monomial("1/2", 2, var_name);
			polys[ poly_idx(1) ] += monomial("1/2", 1, var_name);
		}
		
		rational new_Bcoef, coef1, coef2;
		
		size_t start = (lower == 1 ? 2 : lower);
		for (size_t d = start; d <= p; ++d) {
			// Polynomial for power (d - 1)
			const polynomial& last = polys[ poly_idx(d - 1) ];
			const rational& Bcoef = last.get_monomial_coefficient(d);
			
			// initialize polynomial for power d
			polynomial& S = polys[ poly_idx(d) ];
			S += monomial(Bcoef, d + 1, var_name);
			
			coef2 = last.get_monomial_coefficient(1);
			S += monomial(coef2, 1, var_name);
			
			for (size_t j = 1; j <= d - 1; ++j) {
				coef1 = coef2;
				coef2 = last.get_monomial_coefficient(j + 1);
				
				S += monomial(coef1 + coef2, j + 1, var_name);
				S -= polys[ poly_idx(j) ]*coef1;
			}
			
			new_Bcoef = Bcoef + 1;
			new_Bcoef.invert();
			S *= new_Bcoef;
		}
	}
	
	polynomial power_sums(size_t p, const string& var_name) {
		polynomial s;
		power_sums(p, s, var_name);
		return s;
	}
	
	void polynomial_sum(const polynomial& p, polynomial& q) {
		vector<polynomial> power_sums_poly( p.get_degree().to_int() );
		power_sums(p.get_degree().to_int(), power_sums_poly, p.get_var_name());
		
		for (size_t m = 0; m < p.size(); ++m) {
			size_t sub_poly_idx = p[m].get_exponent().to_int();
			power_sums_poly[sub_poly_idx - 1].set_var_name( p.get_var_name() );
			
			q += power_sums_poly[sub_poly_idx - 1]*p[m].get_coefficient();
		}
	}
	
	polynomial polynomial_sum(const polynomial& p) {
		polynomial q;
		polynomial_sum(p, q);
		return q;
	}
	
}
}


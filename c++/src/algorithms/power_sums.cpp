#include "../algorithms.hpp"

/// PRIVATE

namespace sycalc {
namespace algorithms {
namespace private_section {

	void power_sums(size_t d, map<size_t, polynomial>& polys) {
		if (d == 1) {
			polynomial d1("n");
			d1 += monomial("1/2", 2, "n");
			d1 += monomial("1/2", 1, "n");
			polys.insert(pair<size_t, polynomial>(1, d1));
		}
		else {
			power_sums(d - 1, polys);
			
			// Polynomial for power (p - 1) and the coefficient
			// of highest degree monomial
			const polynomial& last = polys[d - 1];
			//cout << "    d-1=" << d - 1 << " -> " << last << endl;
			
			const rational& Bcoef = last.get_monomial_coefficient(d);
			//cout << "    Bcoef= " << Bcoef << endl;
			
			// Highest degree monomial's coefficient
			rational new_Bcoef = Bcoef + 1;
			new_Bcoef.invert();
			//cout << "    new_Bcoef= " << new_Bcoef << endl;
			
			// Polynomial for power p
			polynomial S;
			S += monomial(Bcoef, d + 1, "n");
			//cout << "    S= " << S << endl;
			
			for (size_t j = 0; j <= d - 1; ++j) {
				//cout << "    +++" << endl;
				
				const rational& coef1 = last.get_monomial_coefficient(j);
				const rational& coef2 = last.get_monomial_coefficient(j + 1);
				//cout << "    j=" << j << " -> coef1= " << coef1 << endl;
				//cout << "    j=" << j << " -> coef2= " << coef2 << endl;
				
				S += monomial(coef1 + coef2, j + 1, "n");
				//cout << "    added monomial: " << monomial(coef1 + coef2, j + 1, "n") << endl;
				
				if (j > 0) {
					S -= polys[j]*coef1;
					//cout << "    substracted polynomial: " << polys[d - j]*coef1 << endl;
				}
				
				//cout << "    ---" << endl;
			}
			
			S += monomial(last.get_monomial_coefficient(0), 0, "n");
			S *= new_Bcoef;
			
			polys.insert(pair<size_t, polynomial>(d, S));
		}
	}

}
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
		map<size_t, polynomial> polys;
		private_section::power_sums(p, polys);
		s = polys[p];
	}
	
	polynomial power_sums(size_t p) {
		polynomial s;
		power_sums(p, s);
		return s;
	}
	
}
}


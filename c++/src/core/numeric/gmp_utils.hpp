#pragma once

/// C includes
#include <gmp.h>

namespace sycalc {
namespace core {
namespace numeric {
namespace gmp_utils {
	
	/* Other arithmetic operations */
	
	void mpz_pow_mpz(mpz_t& r, const mpz_t& b, const mpz_t& e);
	
	/* Fast initializers */
	
	void zero(mpz_t& z);
	void one(mpz_t& o);
	
}
}
}
}


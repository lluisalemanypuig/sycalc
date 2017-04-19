#pragma once

/// C includes
#include <sys/resource.h>
#include <stdlib.h>

namespace sycalc {
namespace mtime {

	typedef double timing;

	// Returns the current time (in seconds)
	timing now();
	
	// Returns the time in seconds
	double elapsed_time(const timing& begin, const timing& end);

}
}


#include "timing.hpp"

namespace sycalc {
namespace mtime {

	timing now() {
		struct timeval tim;
		struct rusage ru;
		getrusage(RUSAGE_SELF, &ru);
		tim = ru.ru_utime;
		return ((double)tim.tv_sec + (double)tim.tv_usec/1000000.0);
	}

	double elapsed_time(const timing& begin, const timing& end) {
		return (end - begin >= 0.0 ? end - begin : 0.0);
	}

}
}


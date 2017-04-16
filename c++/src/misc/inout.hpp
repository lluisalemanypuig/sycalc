#pragma once

/// C++ includes
#include <iostream>
using namespace std;

namespace sycalc {
namespace inout {

#if defined(SYCALC_DEBUG)

#define DISPLAY_ERR(s)		\
	cerr << s << endl;

#else

#define DISPLAY_ERR(s)

#endif

}
}


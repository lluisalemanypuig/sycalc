#include "tests.hpp"

namespace sycalc {
namespace tests {

	void triangle_tests() {
		cout << "Triangles tests" << endl;
		
		cout << "-- Dense Pascal Triangle" << endl;
		
		dense_pascal_triangle dptri;
		dptri.init(10);
		cout << "After init(10)" << endl;
		cout << dptri << endl;
		
		dptri.fill();
		cout << "After fill()" << endl;
		cout << dptri << endl;
		
		cout << "After fill_rows(15)" << endl;
		dptri.fill_rows(15);
		cout << dptri << endl;
		
		cout << dptri.get_binomial(15, 3) << endl;
		cout << dptri.get_binomial(16, 3) << endl;
		cout << dptri.get_binomial(16, 9) << endl;
		cout << dptri.get_binomial(16, 14) << endl;
		
		cout << "After computing some particular values of the triangle:" << endl;
		cout << dptri << endl;
		
		cout << "-- Sparse Pascal Triangle" << endl;
		
		sparse_pascal_triangle ptri;
		ptri.init(10);
		cout << "After init(10)" << endl;
		cout << ptri << endl;
		
		ptri.fill();
		cout << "After fill()" << endl;
		cout << ptri << endl;
		
		cout << "After fill_rows(15)" << endl;
		ptri.fill_rows(15);
		cout << ptri << endl;
		
		cout << ptri.get_binomial(15, 3) << endl;
		cout << ptri.get_binomial(16, 3) << endl;
		cout << ptri.get_binomial(16, 9) << endl;
		cout << ptri.get_binomial(16, 14) << endl;
		
		cout << "After computing some particular values of the triangle:" << endl;
		cout << ptri << endl;
		
		cout << endl;
	}

}
}


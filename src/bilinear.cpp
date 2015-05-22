#include <Rcpp.h>
#include <RcppParallel.h>

using namespace Rcpp;
using namespace RcppParallel;

// xy: num[1:n, 1:2]
// x:  num[1:2, 1:n]
// y:  num[1:2, 1:n]
// v:  num[1:n, 1:4]
//     columns are: bottom-left, top-left, top-right, bottom-right

struct BilinearWorker : public Worker {
	// Inputs
	const RMatrix<double> xy;
	const RMatrix<double> x;
	const RMatrix<double> y;
	const RMatrix<double> v;
	
	// Outputs
	RVector<double> result;
	
	BilinearWorker(const NumericMatrix xy, NumericMatrix x, NumericMatrix y, NumericMatrix v,
		NumericVector result)
		: xy(xy), x(x), y(y), v(v), result(result) {
	}
	
	void operator()(std::size_t begin, std::size_t end) {
		for (size_t i = begin; i < end; i++) {
			double left = x(0,i);
			double right = x(1,i);
			double top = y(1,i);
			double bottom = y(0,i);
			
			double horiz = xy(i,0);
			double vert = xy(i,1);
			
			double denom = (right - left) * (top - bottom);
			
			double bottomLeftValue = v(i,0) / denom;
			double topLeftValue = v(i,1) / denom;
			double topRightValue = v(i,3) / denom;
			double bottomRightValue = v(i,2) / denom;
			result[i] = bottomLeftValue*(right-horiz)*(top-vert) + bottomRightValue*(horiz-left)*(top-vert) +
				topLeftValue*(right-horiz)*(vert-bottom) + topRightValue*(horiz-left)*(vert-bottom);
		}
	}
};

// [[Rcpp::export]]
NumericVector doBilinear(NumericMatrix xy, NumericMatrix x, NumericMatrix y, NumericMatrix v) {
	size_t len = v.nrow();
	NumericVector result(len);
	BilinearWorker worker(xy, x, y, v, result);
	parallelFor(0, len, worker, 10000);
	return result;
}

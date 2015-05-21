#include <Rcpp.h>
using namespace Rcpp;

double linearInterp(double a, double valueA, double b, double valueB, double x) {
  double dist = b - a;
  if (dist == 0)
    return valueA;
  return valueB * (x - a) / dist + valueA * (b - x) / dist;
}
// xy: num[1:n, 1:2]
// x:  num[1:2, 1:n]
// y:  num[1:2, 1:n]
// v:  num[1:n, 1:4]
//     columns are: top-left, bottom-left, bottom-right, top-right

// [[Rcpp::export]]
NumericVector doBilinear(NumericMatrix xy, NumericMatrix x, NumericMatrix y, NumericMatrix v) {
  size_t len = v.nrow();
  NumericVector result(len);
  
  for (size_t i = 0; i < len; i++) {
    double left = x(0,i);
    double right = x(1,i);
    double top = y(1,i);
    double bottom = y(0,i);
    
    double topLeftValue = v(i,0);
    double topRightValue = v(i,3);
    double bottomLeftValue = v(i,1);
    double bottomRightValue = v(i,2);

    double topInterp = linearInterp(left, topLeftValue, right, topRightValue, xy(i,0));
    double bottomInterp = linearInterp(left, bottomLeftValue, right, bottomRightValue, xy(i,0));
    result[i] = linearInterp(top, topInterp, bottom, bottomInterp, xy(i,1));
  }

  return result;
}

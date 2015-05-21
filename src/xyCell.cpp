#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector doCellFromXY(
    int ncols, int nrows, double xmin, double xmax, double ymin, double ymax,
    NumericVector x, NumericVector y) {
  
  size_t len = x.size();
  
  double yres_inv = nrows / (ymax - ymin);
  double xres_inv = ncols / (xmax - xmin);
  
  IntegerVector result(len);
  
  for (size_t i = 0; i < len; i++) {
    double row = static_cast<size_t>((ymax - y[i]) * yres_inv) + 1;
    double col = static_cast<size_t>((x[i] - xmin) * xres_inv) + 1;
    if (row < 1 || row > nrows || col < 1 || col > ncols) {
      result[i] = NA_INTEGER;
    } else {
      result[i] = (row-1) * ncols + col;
    }
  }
  
  return result;
}

// [[Rcpp::export]]
NumericMatrix doXYFromCell(
    int ncols, int nrows, double xmin, double xmax, double ymin, double ymax,
    IntegerVector cell
) {
  size_t len = cell.size();
  
  double yres = (ymax - ymin) / nrows;
  double xres = (xmax - xmin) / ncols;
  
  NumericMatrix result(len, 2);
  
  for (size_t i = 0; i < len; i++) {
    int c = cell[i];
    size_t col = c % ncols;
    size_t row = (c / ncols) + 1;
    result(i,0) = (col - 1) * xres + xmin;
    result(i,1) = -((row - 1) * yres - ymax);
  }
  
  return result;
}

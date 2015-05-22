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
    double row = (ymax - y[i]) * yres_inv;
    double col = (x[i] - xmin) * xres_inv;
    if (row < 0 || row >= nrows || col < 0 || col >= ncols) {
      result[i] = NA_INTEGER;
    } else {
      result[i] = static_cast<int>(row) * ncols + static_cast<int>(col) + 1;
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
    int c = cell[i] - 1;
    size_t col = c % ncols;
    size_t row = (c / ncols);
    result(i,0) = (col + 0.5) * xres + xmin;
    result(i,1) = -((row + 0.5) * yres - ymax);
  }
  
  return result;
}

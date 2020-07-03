#include <Rcpp.h>
using namespace Rcpp;

//' Set equal for lists
//'
//' Returns logical vector with index of where y is equal to x.
//'
//' It is assumed that y only appears in x once and that all vectors in x are
//' sorted.
//'
//' @param x list of integer vectors
//' @param y integer vectors
//'
//' @export
// [[Rcpp::export]]
LogicalVector list_setequal(List x, NumericVector y) {

  int x_size = x.length();
  LogicalVector res (x_size);
  int y_size = y.size();

  for (int i = 0; i < x_size; ++i) {
    NumericVector element = x[i];
    int element_size = element.size();

    if (element_size != y_size) continue;

    for (int j = 0; j < element_size; ++j) {

      if (y[j] != element[j]) break;

      if ((j + 1) == element_size) {
        res[i] ++;
        return(res);
      }
    }
  }
  return(res);
}

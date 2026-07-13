#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector round_up_core(NumericVector a, NumericVector b){

  int n = a.size();
  NumericVector ret(n);
  double current_b = b[0];
  double current_b_index = 0;

  for (int i = 0; i < n; ++i) {
    if (current_b > a[i]) {
      ret[i] = current_b;
    } else {
      while(current_b <= a[i]) {
        current_b_index += 1;
        current_b = b[current_b_index];
      }
      ret[i] = current_b;
    }
  }
  return ret;
}

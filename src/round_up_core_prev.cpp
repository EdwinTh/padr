#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector round_up_core_prev(IntegerVector a, IntegerVector b){

  int n = a.size();
  IntegerVector ret(n);
  int current_b = b[0];
  int current_b_index = 0;

  for (int i = 0; i < n; ++i) {
    if (current_b >= a[i]) {
      ret[i] = current_b;
    } else {
      while(current_b < a[i]) {
        current_b_index += 1;
        current_b = b[current_b_index];
      }
      ret[i] = current_b;
    }
  }
  return ret;
}

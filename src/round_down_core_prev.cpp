#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector round_down_core_prev(NumericVector a, NumericVector b){

  int n = a.size();
  NumericVector ret(n);
  double current_b = b[0];
  double next_b    = b[1];
  int current_b_index = 0;
  double last_b    = max(b);

  for (int i = 0; i < n; ++i) {
    if (next_b >= a[i] or current_b == last_b) {
      ret[i] = current_b;
    } else {
      while(next_b < a[i]) {
        current_b_index += 1;
        current_b = b[current_b_index];
        if (next_b == last_b) {
         break;
        }
        next_b    = b[current_b_index + 1];
      }
      ret[i] = current_b;
    }
  }
  return ret;
}

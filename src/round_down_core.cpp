#include <Rcpp.h>
using namespace Rcpp;

// Core of the round_up function

// [[Rcpp::export]]
IntegerVector round_down_core(IntegerVector a, IntegerVector b){

  int n = a.size();
  IntegerVector ret(n);
  int current_b = b[0];
  int next_b    = b[1];
  int current_b_index = 0;

  for (int i = 0; i < n; ++i) {
    if (next_b > a[i]) {
      ret[i] = current_b;
    } else {
      while(next_b <= a[i]) {
        current_b_index += 1;
        current_b = b[current_b_index];
        next_b    = b[current_b_index + 1];
      }
      ret[i] = current_b;
    }
  }
  return ret;
}

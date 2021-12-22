#include <Rcpp.h>
using namespace Rcpp;

//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param a  param for original distribution
//' @param b  param for original distribution
//' @param n  param for original distribution
//' @param N the number of samples
//' 
//' @return a random sample of size \code{n}
//' @examples
//' \dontrun{
//' rnC <- gibbsC(1,1,25,10000)
//' par(mfrow=c(2,1));
//' plot(rnC[,1],type='l')
//' plot(rnC[,2],type='l')
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix gibbsC(int a, int b, int n, int N) {
  NumericMatrix mat(N, 2);
  double x = 0, y = 0;
  for(int i = 0; i < N; i++) {
    x = rbinom(1,n,y)[0];
    y = rbeta(1,x+a,n-x+b)[0];
    mat(i, 0) = x;
    mat(i, 1) = y;
  }
  return(mat);
}


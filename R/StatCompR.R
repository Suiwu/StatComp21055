#' @title A Gibbs sampler using R
#' @description A Gibbs sampler using R
#' @param a  param for original distribution
#' @param b  param for original distribution
#' @param n  param for original distribution
#' @param N the number of samples
#' @return a random sample of size \code{n}
#' @examples
#' \dontrun{
#' rnR <- gibbsR(1,1,25,10000)
#' par(mfrow=c(2,1));
#' plot(rnR[,1],type='l')
#' plot(rnR[,2],type='l')
#' }
#' @importFrom Rcpp evalCpp
#' @useDynLib StatComp21055
#' @importFrom stats rbinom rbeta
#' @export
gibbsR=function(a,b,n,N){
  X=matrix(0, N, 2)  #样本阵
  X[1,]=c(0,0.5)
  for(i in 2:N){
    X2= X[i-1, 2]
    X[i,1]=rbinom(1,25,X2)
    X1=X[i,1]
    X[i,2]=rbeta(1,X1+a,25-X1+b)
  }
  return(X)
}
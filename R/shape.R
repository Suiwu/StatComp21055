library(shapes)
library(frechet)
library(stats)
library(BB)


#' @title Brain landmark data
#' @name brains 
#' @description 24 landmarks located in 58 adult healthy brains
#' @examples
#' \dontrun{
#' data(brains)
#'
#' }
NULL


## function for Fmean to minimize
objfunF =function(w, Y){
  k =dim(Y)[1]
  m =dim(Y)[2]
  n =dim(Y)[3]
  w =matrix(w, k, m)
  sum =0
  for (i in 1:n) {
    sum =sum + ((riemdist(w, Y[, , i])) ** 2)/n
  }
  sum
}

#' @title Frechet mean 
#' @description Compute Frechet mean for shapes data
#' @param Y A shapes data (landmark\*dim \*num)
#' @return A value in shapes space.
#' @examples
#' \dontrun{
#' # Compute Frechet mean
#' data("brains")
#' Y=brains$x[,,1:10]
#' Fm=Fmean(Y)
#' Fm
#' }
#' @export
Fmean=function(Y){
  k=dim(Y)[1]
  m=dim(Y)[2]
  n=dim(Y)[3]
  w=matrix(0,k,m)
  for(i in 1:n){
    w=w+Y[,,i]/n
  }
  out=spg(par=c(w),fn=objfunF,control = list(trace = FALSE),Y=Y)$par  
  B =matrix(out, k, m)
  B
}


## weight function
## n is the size of examples
## X is predict variable
## x0 is the location of estimation
s=function(n,X,x0){
  s=rep(0,n)
  k=ncol(X)
  for(i in 1:n){
    sum=rep(0,k)
    X.avg=colMeans(X)
    Sigma.hat=cov(X)* (n-1) / n
    invVa= solve(Sigma.hat)
    s[i]=1+t(X[i,]-X.avg)%*%invVa%*%(x0-X.avg)
  }
  return(s)
}


## the function for Frechet Regression to minimize
objfun =function(w, Y, X, x0){
  k =dim(Y)[1]
  m =dim(Y)[2]
  n =dim(Y)[3]
  s=s(n,X,x0)
  w =matrix(w, k, m)
  sum =0
  for (i in 1:n) {
    sum =sum + s[i]*((riemdist(w, Y[, , i])) ** 2)/n
  }
  sum
}


#' @title Frechet regression
#' @description Regression for shapes data through frechet mean
#' @param Y  A shapes data (landmark\*dim \*num)
#' @param X  variables in euclidean space
#' @param x0 location of estimation
#' @return A value in shapes space.
#' @examples
#' \dontrun{
#' # estimation
#' data("brains")
#' Y=brains$x[,,1:10]
#' X=cbind(brains$age,brains$grp)[1:10,]
#' em=FREShape(Y,X,X[1,])
#' em
#' }
#' @export
FREShape =function(Y,X,x0) {
  k=dim(Y)[1]
  m=dim(Y)[2]
  n=dim(Y)[3]
  w=matrix(0,k,m)
  for(i in 1:n){
    w=w+Y[,,i]/n
  }
  out=spg(par=c(w),fn=objfun,control = list(trace = FALSE),X=X,Y=Y,x0=x0 )$par  ## start at mean
  B =matrix(out, k, m)
  B
}


## Model selection
## R2=1-E/V  determination coefficient 
## R2.adj=R2-(1-R2)*q/(n-q-1)
## return R2 and R2.adj
#' @title R square
#' @description Determination coefficient for model
#' @param Y  A shapes data (landmark\*dim \*num)
#' @param X  variables in euclidean space
#' 
#' @return A \code{list} object comprising:
#' \item{R2}{Determination coefficient.}
#' \item{y}{Adjust determination coefficient.}
#' @examples
#' \dontrun{
#' # Compute determination coefficient
#' data("brains")
#' Y=brains$x[,,1:10]
#' X=cbind(brains$age,brains$grp)[1:10,]
#' r2=R2(Y,X)
#' r2
#' }
#' @export
R2=function(Y,X){
  k=dim(Y)[1]
  m=dim(Y)[2]
  n=dim(Y)[3]
  hat.mX=array(rep(0,k*m*n),dim=c(k,m,n))  ## estimation for X
  for(i in 1:n){
    hat.mX[,,i]=FREShape(Y,X,X[i,])
  }
  
  ##  E
  E=0
  for(i in 1:n){
    E=E+(riemdist(hat.mX[,,i], Y[, , i]) ** 2)
  }
  
  ##  V
  Fm=Fmean(Y)  ## Fmean 
  V=0
  for(i in 1:n){
    V=V+(riemdist(Fm, Y[, , i]) ** 2)
  }
  R2=1-(E/V)
  
  q=ncol(X)
  R2.adj=1-(1-R2)*(n-1)/(n-q-1)
  re=list(R2=0,R2.adj=0)
  re$R2=R2
  re$R2.adj=R2.adj
  return(re)
}

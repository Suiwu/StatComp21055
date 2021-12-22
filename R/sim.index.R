library(bestsubset)
library(stats)
library(abess)
library(glmnet)
#' Compute index.
#' @title Compute index
#' @description  Compute index for simulation
#' @param n The number of training observations
#' @param p The number of predictors.
#' @param nval The number of validation observations.
#' @param methods Methods to use.It could be "Lasso","Abess","Forward stepwise" or "Relaxed lasso". 
#' @param nrep Number of repetitions of which to average the results. Default is 20.
#' @param file Name of a file to save simulation results.
#' @param rho.vec  Choose of rho.
#' @param snr.vec  Choose of snr.
#' @param verbose,file.rep,s,seed,beta.type Arguments to pass to \code{sim.master}
#'
#' @return A list with index(saves in file)
#' @export 
#' 

compute.index= function(n, p, nval, methods, nrep=20, seed=NULL, verbose=FALSE,
                      file=NULL,  rho.vec=c(0.35), s=5, beta.type=1, snr.vec=c(0.8,1.6,2.4,3.6,4.8)) {
  
  set.seed(seed)
  stem = paste0("sim.n",n,".p",p)
  reg.funs=list()
  N = length(methods)
  for(j in 1:N){
    
    #### Lasso
    if(methods[j]=="Lasso"){
      reg.funs[[methods[j]]] = function(x,y) lasso(x,y,intercept=FALSE,nlam=50)
    }
    
    #### ABESS
    if(methods[j]=="Abess"){
      reg.funs[[j]] = function(x,y) {
        out=abess(x,y,tune.path ="sequence",support.size =0:(p-1))
        class(out) = "Abess"
        out$x=x
        out$y=y
        return(out)
      }
      
      coef.Abess = function(object, s=NULL, gamma=NULL) {
        return(object$beta)
      }
      
      predict.Abess = function(object, newx, s=NULL) {
        if (missing(newx)) newx = object$x
        return(newx %*% coef.Abess(object))
      }
    }
    
    #### Forward stepwize
    if(methods[j]=="Forward stepwise"){
      reg.funs[[methods[j]]] = function(x,y) fs(x,y,intercept=FALSE)
    }
    
    #### Relaxed lasso
    if(methods[j]=="Relaxed lasso"){
      reg.funs[[methods[j]]] = function(x,y) lasso(x,y,intercept=FALSE,
                                                   nrelax=10,nlam=50)
    }
    
  }
  
  
  file.list = c() # Vector of files for the saved rds files
  for (rho in rho.vec) {
    name = paste0(stem, ".beta", beta.type, sprintf(".rho%0.2f", rho))
    for (snr in snr.vec) {
      file = paste0("rd/", name, ".snr", round(snr,2), ".rds")
      cat("..... NEW SIMULATION .....\n")
      cat("--------------------------\n")
      cat(paste0("File: ", file, "\n\n"))
      
      sim.master(n, p, nval, reg.funs=reg.funs, nrep=nrep, seed=seed, s=s,
                 verbose=verbose, file=file, rho=rho, beta.type=beta.type, snr=snr)
      
      file.list = c(file.list, file)
      cat("\n")
    }
  }
  return(file.list)
}
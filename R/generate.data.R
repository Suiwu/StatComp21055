library(stats)
#' @title Generate simulated data
#' @description Generate simulated data under the linear model
#' @param n The number of observations.
#' @param p The number of predictors.
#' @param support.size The number of nonzero coefficients.
#' @param rho The pairwise correlation in predictors. Default is \code{0}.
#' @param family The distribution of the simulated response. 
#' @param beta The coefficient values.
#' @param snr Signal-to-noise ratio (SNR).Default is \code{snr = 10}.
#' @param sigma The variance of the gaussian noise. Default \code{sigma = NULL}
#' @param seed random seed. Default: \code{seed = 1}.
#' 
#' @return A \code{list} object comprising:
#' \item{x}{Design matrix of predictors.}
#' \item{y}{Response variable.}
#' \item{beta}{The true coefficients.}
#' @examples
#' \dontrun{
#' # Generate simulated data
#' n <- 70
#' p <- 30
#' support.size <- 10
#' data<- sim.data(n, p, support.size)
#' }
#' @export
sim.data <- function(n, p, support.size = NULL, rho = 0,beta = NULL,
                    family = "gaussian",
                    snr = 10, sigma = NULL, seed = 1){
  
  set.seed(seed)
  ## pre-treatment for beta
  if (!is.null(beta)) {
      stopifnot(is.vector(beta))
      support.size <- sum(abs(beta) > 1e-5)
      beta[abs(beta) <= 1e-5] <- 0
  } else {
    if (is.null(support.size)) {
      stop("Please provide an integer to support.size.")
    }
    stopifnot(is.numeric(support.size) & support.size >= 1)
  }

  Sigma <- diag(p)
  x <- matrix(rnorm(n * p), nrow = n, ncol = p)
  input_beta <- beta
  beta <- rep(0, p)
  nonzero <- sample(1:p, support.size)
  
  ## gaussian type
  if (family == "gaussian") {
    m <- 5 * sqrt(2 * log(p) / n)
    M <- 100 * m
    if (is.null(input_beta)) {
      beta[nonzero] <- stats::runif(support.size, m, M)
    } else {
      beta <- input_beta
    }
    if (is.null(sigma)) {
      sigma <- sqrt((t(beta) %*% Sigma %*% beta) / snr)
    }
    
    y <- x %*% beta + rnorm(n, 0, sigma)
  }
  
  ## binomial type
  if (family == "binomial") {
    m <- 5 * sqrt(2 * log(p) / n)
    if (is.null(input_beta)) {
      beta[nonzero] <- stats::runif(support.size, 2 * m, 10 * m)
    } else {
      beta <- input_beta
    }
    if (is.null(sigma)) {
      sigma <- sqrt((t(beta) %*% Sigma %*% beta) / snr)
    }
    
    eta <- x %*% beta + rnorm(n, 0, sigma)
    PB <- apply(eta, 1, generatedata2)
    y <- stats::rbinom(n, 1, PB)
  }
  
  ## possion type
  if (family == "poisson") {
    m <- 5 * sqrt(2 * log(p) / n)
    if (is.null(input_beta)) {
      beta[nonzero] <- stats::runif(support.size, -2 * m, 2 * m)
    } else {
      beta <- input_beta
    }
    if (is.null(sigma)) {
      sigma <- sqrt((t(beta) %*% Sigma %*% beta) / snr)
    }
    
    sigma <- 0
    eta <- x %*% beta + stats::rnorm(n, 0, sigma)
    eta <- ifelse(eta > 30, 30, eta)
    eta <- ifelse(eta < -30, -30, eta)
    eta <- exp(eta)
    y <- sapply(eta, stats::rpois, n = 1)
  }
  colnames(x) <- paste0("x", 1:p)
  return(list(x = x, y = y, beta = beta))
}


generatedata2 <- function(eta) {
  a <- exp(eta) / (1 + exp(eta))
  if (is.infinite(exp(eta))) {
    a <- 1
  }
  return(a)
}





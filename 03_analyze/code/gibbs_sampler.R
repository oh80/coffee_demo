gibbs_sampler <- function(X, y, iter=1000, burn_in = 500,
                          verbose_iter = 100, v0 = 5, sig0 = 10){
  n <- nrow(X)
  K <- ncol(X)
  
  beta_sample <- matrix(0, nrow = (iter-burn_in), ncol = K)
  sig_sample  <- rep(0, (iter-burn_in))
  
  XX <- t(X)%*%X
  Xy <- t(X)%*%y
  yy <- t(y)%*%y
  
  # initial params
  beta <- rep(0, K)
  sig  <- 10
  
  for(i in 1:iter){
    beta <- beta_sampler(K, XX, Xy, sig, sig0, v0)
    sig  <- sig_sampler(n, XX, Xy, yy, beta, sig0, v0)
    
    if(i > burn_in){
      sample_idx <- i - burn_in
      beta_sample[sample_idx, ] <- beta
      sig_sample[sample_idx]    <- sig
    }
    
    if(i%%verbose_iter == 0){
      print(paste0(i, " iter has been done!"))
    }
  }
  
  output <- list()
  output$beta <- beta_sample
  output$sig  <- sig_sample
  return(output)
}


beta_sampler <- function(K, XX, Xy, sig, sig0, v0){
  beta_var  <- solve(1/sig0*diag(K) + XX/sig)
  beta_mean <- beta_var %*% (Xy/sig)
  beta <- MASS::mvrnorm(n=1, beta_mean, beta_var)
  return(beta)
}


sig_sampler <- function(n, XX, Xy, yy, beta, sig0, v0){
  SSR <- yy - 2*t(beta)%*%Xy + t(beta)%*%XX%*%beta
  precision <- rgamma(n=1, (v0+n)/2, (v0*sig0 + SSR)/2)
  sig <- 1/precision
  return(sig)
}

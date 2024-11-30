main <- function(){
  # read data
  data_path <- here::here("02_build", "data", "features.obj")
  data <- readRDS(data_path)
  
  MCMC_path <- here::here("03_analyze", "code", "gibbs_sampler.R")
  source(MCMC_path)
  
  set.seed(42)
  
  # set X and y
  y <- data |> set_y()
  X <- data |> set_X()
  
  # run MCMC
  MCMC_res <- gibbs_sampler(X, y)
  
  # save
  save_samples(MCMC_res)
}


set_y <- function(data){
  y <- data$Overall |> as.matrix()
  return(y)
}


set_X <- function(data){
  X <- data |> dplyr::select(-c("ID", "Overall"))
  X <- cbind(1, X) |> as.matrix()
  return(X)
}


save_samples <- function(res){
  file_name <- "MCMC_res.obj"
  save_path <- here::here("03_analyze", "result", file_name)
  saveRDS(res, save_path)
}


main()
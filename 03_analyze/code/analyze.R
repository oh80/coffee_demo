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
  
  # PCA
  PCA_score <- pca_analyze(data)
  
  # save
  save_result(MCMC_res)
  save_result(PCA_score)
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


pca_analyze <- function(data){
  features <- c("Aroma", "Flavor", "Aftertaste", "Acidity", "Balance", "Body")
  X <- data |> dplyr::select(all_of(features))
  
  pca_res <- prcomp(X, scale=TRUE)
  pca_score <- pca_res$x
  return(pca_score)
}


save_result <- function(input){
  file_name <- paste0(deparse(substitute(input)),  ".obj")
  save_path <- here::here("03_analyze", "result", file_name)
  saveRDS(input, save_path)
}


main()
main <- function(){
  # read result
  res_path <- here::here("03_analyze", "result", "MCMC_res.obj")
  MCMC_res <- readRDS(res_path)
  
  # plot posterior
  posterior_plot <- plot_hist(MCMC_res)
  
  # save plot
  save_plot(posterior_plot)
  
}


plot_hist <- function(MCMC_res){
  beta_samples <- MCMC_res$beta
  feature_names <- c("intercept", "Aroma", "Flavor", "Aftertaste",
                     "Acidity", "Balance", "Body")
  output <- list()
  
  for(i in 2:length(feature_names)){
    # plot
    beta_i <- beta_samples[,i] |> as.data.frame() |> setNames("value")
    plot <- ggplot2::ggplot(data = beta_i, ggplot2::aes(x = value))+
      ggplot2::geom_histogram(binwidth = 0.1, fill = "cyan3") +
      ggplot2::coord_cartesian(xlim = c(-1.5, 1.5))+ 
      ggplot2::theme_minimal() +
      ggplot2::labs(title = feature_names[i]) +
      ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                     plot.title   = ggplot2::element_text(face = "bold", size = 20))
    show(plot)
    
    # add output
    if(i > 1){output[[i-1]] = plot}
  }
  return(output)
}


save_plot <- function(plot_list){
  for(i in 1:length(plot_list)){
    file_name <- paste0("beta_posterior_", i, ".pdf")
    save_path <- here::here("04_report", "result", file_name)
    ggplot2::ggsave(save_path, plot_list[[i]])
  }
}


main()

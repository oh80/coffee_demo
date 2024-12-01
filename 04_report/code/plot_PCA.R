main <- function(){
  # read data and PCA result
  res_path <- here::here("03_analyze", "result", "PCA_score.obj")
  score <- readRDS(res_path)
  
  data_path <- here::here("02_build", "data", "ID_country.obj")
  id_country <- readRDS(data_path)
  
  # preprocess for plot
  df_for_plot <- bind_data(id_country, score) |> 
    fill_by_other()
  
  # plot
  plot <- plot_pca_scatter(df_for_plot)
  
  #save
  save_plot(plot)
}


bind_data <- function(data1, data2){
  output <- data1 |> dplyr::bind_cols(data2)
  return(output)
}


fill_by_other <- function(data, threshold = 10){
  # sort country by count
  country_count <- data |> dplyr::group_by(Country) |>
    dplyr::summarise(count=dplyr::n())
  
  lot_counts <- country_count |> dplyr::filter(count >= threshold) |> 
    dplyr::select(Country)
  few_counts <- country_count |> dplyr::filter(count < threshold) |> 
    dplyr::select(Country)
  
  # fill by "other" if few counts countries
  score_and_country_lot <- data |>
    dplyr::filter(Country %in% lot_counts$Country)
  
  score_and_country_few <- data |>
    dplyr::filter(Country %in% few_counts$Country) |> 
    dplyr::mutate(Country = "other")
  
  df_for_plot <- score_and_country_lot |> dplyr::bind_rows(score_and_country_few)
  return(df_for_plot)
}


plot_pca_scatter <- function(data){
  # set county order and color
  unique_country <-  unique(data$Country)[unique(data$Country) != "other"]
  coutnry_order <- c(unique_country, "other")
  colors <- c(ggsci::pal_lancet()(length(unique_country)), "gray")
  
  plot <- ggplot2::ggplot(data = data,
                          ggplot2::aes(x=PC1, y=PC2, color=factor(Country, coutnry_order))) +
    ggplot2::geom_point(alpha = 0.8)  +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::theme_minimal() +
    ggplot2::coord_cartesian(xlim = c(-5, 5),
                             ylim = c(-5, 5)) +
    ggplot2::labs(title = "PCA scatter plot",
                  color = "country") 
  show(plot)
  return(plot)
}


save_plot <- function(plot){
  file_name <- paste0("2d_PCA_point",".pdf")
  save_path <- here::here("04_report", "result", file_name)
  ggplot2::ggsave(save_path, plot)
}


main()

main <- function(){
  # read data
  data_path <- here::here("01_data","data","df_1_arabica.csv")
  data <- read.csv(data_path)
  
  # preprocess
  data <- data  |> drop_isna_row() |> add_ID()
  
  features   <- data |> extract_feature()
  ID_country <- data |> extract_country()
  
  # save 
  save_path1 <- here::here("02_build","data","features.obj")
  saveRDS(features, save_path1)
  save_path2 <- here::here("02_build","data","ID_country.obj")
  saveRDS(ID_country, save_path2)
}


add_ID <- function(data){
  ID <- seq(1, nrow(data), 1)
  data <- data |> dplyr::mutate("ID" = ID)
  return(data)
}


drop_isna_row <- function(data){
  processd_cols <- c("Country.of.Origin", "Aroma", "Flavor", "Aftertaste", 
                     "Acidity", "Balance", "Body", "Overall")
  for(col in processd_cols){
    data <- data |> dplyr::filter(!is.na(col))
  }
  return(data)
}


extract_feature <- function(data){
  feature_cols <- c("ID", "Aroma", "Flavor", "Aftertaste", "Acidity",
                    "Balance", "Body", "Overall")
  output <- data |> dplyr::select(all_of(feature_cols))
  return(output)
}


extract_country <- function(data){
  ID_country_cols <- c("ID", "Country.of.Origin")
  output <- data |> dplyr::select(all_of(ID_country_cols)) |> 
    dplyr::rename("Country" = Country.of.Origin)
  return(output)
}


main()
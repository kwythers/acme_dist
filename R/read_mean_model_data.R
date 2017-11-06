#Script to read in and modify model output
read_mean_model <- function(path) {
  path <- path #"~/projects/globland/acme_dist/data/mean_model_output_out/"
  fileptrn <- "data_[A-Z]{2}-[A-Za-z0-9]{3}_run_101.csv"
  filenames <- list.files(path, full.names = TRUE, pattern = fileptrn, recursive = TRUE)
  mean_model_data_day_out <- lapply(filenames,modify_model)
  return(mean_model_data_day_out)
  
}

mean_model_to_year <- function(mean_model_data_day_out){
  list_out <- lapply(mean_model_data_day_out,get_model_year)
  mean_model_data_year <-ldply(list_out,data.frame)
  return(mean_model_data_year)
  
}

mean_model_to_month <- function(mean_model_data_day_out){
  list_out <- lapply(mean_model_data_day_out,get_model_month)
  mean_model_data_month <-ldply(list_out,data.frame)
  return(mean_model_data_month)
  
}



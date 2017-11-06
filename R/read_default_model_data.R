#Script to read in and modify model output
read_default_model <- function(path) {
  path <- path #"~/acme_dist/data/default_model_output_out/"
  fileptrn <- "data_[A-Z]{2}-[A-Za-z0-9]{3}_run_.csv"
  filenames <- list.files(path, full.names = TRUE, pattern = fileptrn, recursive = TRUE)
  default_model_data_day_out <- lapply(filenames,modify_model)
  return(default_model_data_day_out)
  
}

default_model_to_year <- function(default_model_data_day_out){
  list_out <- lapply(default_model_data_day_out,get_model_year)
  default_model_data_year <-ldply(list_out,data.frame)
  return(default_model_data_year)
  
}

default_model_to_month <- function(default_model_data_day_out){
  list_out <- lapply(default_model_data_day_out,get_model_month)
  default_model_data_month <-ldply(list_out,data.frame)
  return(default_model_data_month)
  
}



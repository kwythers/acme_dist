#Script to read in and modify model output
read_model <- function(path) {
  path <- path #"~/projects/acme_dist/data/model_output"
  fileptrn <- "data_[A-Z]{2}-[A-Za-z0-9]{3}_run_100.csv"
  filenames <- list.files(path, full.names = TRUE, pattern = fileptrn, recursive = TRUE)
  model_data <- lapply(filenames,modify_model)
  return(model_data)

}

modify_model <- function(filenames) {
  dat_in <- fread(filenames)
  
  #get dates
  dat_in$date <- as.Date(as.character(dat_in$date), format='%Y%m%d')
  dat_in$year <- format(dat_in$date, '%Y')
  dat_in$month <- format(dat_in$date, '%m')
  dat_in$day <- format(dat_in$date, '%d')
  
  #get the output from umol/second to annual
   dat_in %>%
   mutate(npp = npp * 3600,
          mr = mr * 3600,
          lmr = lmr * 3600,
          gpp = gpp * 3600) -> dat_modify
  
  dat_modify %>%
    group_by(run, country, site,year, month, day) %>%
    dplyr::summarise(mr = sum(mr, na.rm=TRUE),
                     npp=sum(npp, na.rm=TRUE),
                     gpp= sum(gpp, na.rm=TRUE),
                     lmr= sum(lmr, na.rm=TRUE)) -> data_day_out
  return(data_day_out)
  
}



model_to_year <- function(data_day_out){
  list_out <- lapply(data_day_out,get_model_year)
  model_data_year <-ldply(list_out,data.frame)
  return(model_data_year)
  
}

get_model_year <- function(data){
  data %>%
    group_by(run, country,site,year) %>%
    dplyr::summarise(mr = sum(mr, na.rm=TRUE),
                     npp=sum(npp, na.rm=TRUE),
                     gpp= sum(gpp, na.rm=TRUE),
                     lmr= sum(lmr, na.rm=TRUE)) %>%
    unite(site_code,country,site, sep="-")-> data_out
  return(data_out)
  
}
  
model_to_month <- function(data_day_out){
  list_out <- lapply(data_day_out,get_model_month)
  model_data_month <-ldply(list_out,data.frame)
  return(model_data_month)
  
}

get_model_month <- function(data){
  data %>%
    group_by(run, country,site,year,month) %>%
    dplyr::summarise(mr = sum(mr, na.rm=TRUE),
                     npp=sum(npp, na.rm=TRUE),
                     gpp= sum(gpp, na.rm=TRUE),
                     lmr= sum(lmr, na.rm=TRUE)) %>%
    unite(site_code,country,site, sep="-")-> data_out
  return(data_out)
  
}

# dat %>%
#   group_by(run, country, site,year, month, day) %>%
#   dplyr::summarise(mr = sum(mr, na.rm=TRUE),
#                    npp=sum(npp, na.rm=TRUE),
#                    gpp= sum(gpp, na.rm=TRUE),
#                    lmr= sum(lmr, na.rm=TRUE)) -> data_short
# 
# data_short %>%
#   group_by(run, country,site, year, month) %>%
#   dplyr::summarise(mr = sum(mr, na.rm=TRUE),
#                    npp=sum(npp, na.rm=TRUE),
#                    gpp= sum(gpp, na.rm=TRUE),
#                    lmr= sum(lmr, na.rm=TRUE)) -> data_shortmonth
# 

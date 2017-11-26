read_fluxnet_day <- function(path) {
  path <- path #"~/projects/globland/fluxnet"
  fileptrn_dd <- "FLX_[A-Z]{2}-[A-Za-z0-9]{3}_FLUXNET2015_FULLSET_DD_[0-9]{4}-[0-9]{4}_[0-9]{1}-[0-9]{1}.csv"
  filenames_dd <- list.files(path, full.names = TRUE, pattern = fileptrn_dd, recursive = TRUE)
  #run through all data directories, add column for SITE and fill with regular eppression
  tibble(File = filenames_dd) %>%
    extract(File, "SITE", "([A-Z]{2}-[A-Za-z0-9]{3})", remove = FALSE) %>%
    mutate(Data = lapply(File, read_csv)) %>%
    unnest(Data) %>%
    select(-File) %>%  # separate "timestamp" into date pieces and create a "DATE" column
    separate(TIMESTAMP, into = c("YEAR", "MONTHDAY"), sep = 4) %>%
    separate(MONTHDAY, into = c("MONTH", "DAY"), sep = 2) %>%
    unite("DATE", YEAR, MONTH, DAY, sep = "-", remove = FALSE) -> data_dd
  
  # pull out 4 daily variables for analysis
  data_dd %>%
    select(SITE, DATE, YEAR, MONTH, DAY, NEE_CUT_REF, 
        NEE_CUT_REF_JOINTUNC, RECO_NT_CUT_REF, GPP_NT_CUT_REF) %>% 
    mutate(DATE = as.Date(DATE)) %>% 
    mutate(NEE_CUT_REF = na_if(NEE_CUT_REF,"-9999")) %>% 
    mutate(NEE_CUT_REF_JOINTUNC = na_if(NEE_CUT_REF_JOINTUNC, "-9999")) %>% 
    mutate(RECO_NT_CUT_REF = na_if(RECO_NT_CUT_REF, "-9999")) %>%
    mutate(GPP_NT_CUT_REF = na_if(GPP_NT_CUT_REF,"-9999")) %>% 
    mutate_at(vars(contains("REF")), funs(as.numeric)) %>% 
    mutate_if(is.character, as.factor) -> data_dd_out
  return(data_dd_out)
  #write.csv(data_dd_out,"data/erase.csv")
  
}

read_fluxnet_year <- function(path) {
  path <- path #"~/projects/globland/fluxnet"
  fileptrn_yy <- "FLX_[A-Z]{2}-[A-Za-z0-9]{3}_FLUXNET2015_FULLSET_YY_[0-9]{4}-[0-9]{4}_[0-9]{1}-[0-9]{1}.csv"
  filenames_yy <- list.files(path, full.names = TRUE, pattern = fileptrn_yy, recursive = TRUE)
  #run through all data directories, add column for SITE and fill with regular eppression
  data_yy <- tibble(File = filenames_yy) %>%
    extract(File, "SITE", "([A-Z]{2}-[A-Za-z0-9]{3})", remove = FALSE) %>%
    mutate(Data = lapply(File, read_csv)) %>%
    unnest(Data) %>%
    select(-File)
  
  data_yy %>%
    select(SITE, TIMESTAMP, NEE_CUT_REF, 
                   NEE_CUT_REF_JOINTUNC, RECO_NT_CUT_REF, GPP_NT_CUT_REF) %>% 
    mutate(NEE_CUT_REF = na_if(NEE_CUT_REF, "-9999")) %>%
    mutate(NEE_CUT_REF_JOINTUNC = na_if(NEE_CUT_REF_JOINTUNC, "-9999")) %>%
    mutate(RECO_NT_CUT_REF = na_if(RECO_NT_CUT_REF, "-9999")) %>%
    mutate(GPP_NT_CUT_REF = na_if(GPP_NT_CUT_REF, "-9999")) %>%
    mutate_at(vars(contains("REF")), funs(as.numeric)) %>% 
    mutate_if(is.character, as.factor) -> data_yy_out
  
  return(data_yy_out)
  #write.csv(data_dd_out,"data/erase.csv")
  
}


read_fluxnet_month <- function(path) {
  path <- path #"~/projects/globland/fluxnet"
  fileptrn_mm <- "FLX_[A-Z]{2}-[A-Za-z0-9]{3}_FLUXNET2015_FULLSET_MM_[0-9]{4}-[0-9]{4}_[0-9]{1}-[0-9]{1}.csv"
  filenames_mm <- list.files(path, full.names = TRUE, pattern = fileptrn_mm, recursive = TRUE) #run through all data directories, add column for SITE and fill with regular eppression
   tibble(File = filenames_mm) %>%
    extract(File, "SITE", "([A-Z]{2}-[A-Za-z0-9]{3})", remove = FALSE) %>%
    mutate(Data = lapply(File, read_csv)) %>%
    unnest(Data) %>%
    select(-File) %>%
     separate(TIMESTAMP, into = c("YEAR", "MONTH"), sep = 4) %>% # separate "timestamp" into date pieces and create a "DATE" column
    mutate(DAY = 15) %>% # add column for d 
    unite("DATE", YEAR, MONTH, DAY, sep = "-", remove = FALSE) -> data_mm
  
  data_mm %>%
    select(SITE, DATE, YEAR, MONTH, NEE_CUT_REF, 
                   NEE_CUT_REF_JOINTUNC, RECO_NT_CUT_REF, GPP_NT_CUT_REF) %>% 
    mutate(DATE = as.Date(DATE)) %>%
    mutate(NEE_CUT_REF = na_if(NEE_CUT_REF, "-9999")) %>%
    mutate(NEE_CUT_REF_JOINTUNC = na_if(NEE_CUT_REF_JOINTUNC, "-9999")) %>%
    mutate(RECO_NT_CUT_REF = na_if(RECO_NT_CUT_REF, "-9999")) %>%
    mutate(GPP_NT_CUT_REF = na_if(GPP_NT_CUT_REF, "-9999")) %>%
    mutate_at(vars(contains("REF")), funs(as.numeric)) %>% 
    mutate_if(is.character, as.factor) -> data_mm_out
  
  data_mm_out %>%
    mutate(NEE_CUT_REF = NEE_CUT_REF * days_in_month(DATE)) %>%
    mutate(NEE_CUT_REF_JOINTUNC = NEE_CUT_REF_JOINTUNC * days_in_month(DATE)) %>%
    mutate(RECO_NT_CUT_REF = RECO_NT_CUT_REF * days_in_month(DATE)) %>%
    mutate(GPP_NT_CUT_REF = GPP_NT_CUT_REF * days_in_month(DATE)) -> data_mm_out
    
  return(data_mm_out)
 
}

read_fluxnet_sites_info <- function(){
  sites_pft <- read_csv("data/Fluxnet2015_longrecord.xlsx - Sheet1.csv")
  colnames(sites_pft) <-c("site_code","site_name","start_year","last_year",
                          "running_years","PFTname","lat","long","mat","map")
  
  return(sites_pft)
  
}


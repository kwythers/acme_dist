##### Load some libraries

require(tidyverse)
require(stringr)
require(lubridate)

readpath <- function()
{ 
  n <- readline(prompt = "Enter path to nc files without starting or finishing slash :")
  return(as.character(n))
}

readfilename <- function()
{ 
  n <- readline(prompt = "Enter file or file pattern : ")
  return(as.character(n))
}

#Current path ~/../shared/fluxnet/fluxnet ### at MSI
#Current path ~/projects/globland/fluxnet ### local disk

path <- print(readpath())

##### file and partial file patterns
# FLX_[A-Z]{2}-[A-Za-z0-9]{3}_FLUXNET2015_[A-Z]{4,7}_[A-Z]{2}_[0-9]{4}-[0-9]{4}_[0-9]{1}-[0-9]{1}.csv ## everything
# FLX_[A-Z]{2}-[A-Za-z0-9]{3}_FLUXNET2015_FULLSET_DD_[0-9]{4}-[0-9]{4}_[0-9]{1}-[0-9]{1}.csv ### FULLSET daly files
# FLX_[A-Z]{2}-[A-Za-z0-9]{3}_FLUXNET2015_FULLSET_YY_[0-9]{4}-[0-9]{4}_[0-9]{1}-[0-9]{1}.csv ### FULLSET yearly files
# FLX_[A-Z]{2}-[A-Za-z0-9]{3}$ #### sitename pattern
# site <- str_extract(filenames, "[A-Z]{2}-[A-Za-z0-9]{3}")

##### for DD files
fileptrn_dd <- print(readfilename())
filenames_dd <- list.files(path, full.names = TRUE, pattern = fileptrn_dd, recursive = TRUE) # DD
#run through all data directories, add column for SITE and fill with regular eppression
data_dd <- tibble(File = filenames_dd) %>%
  extract(File, "SITE", "([A-Z]{2}-[A-Za-z0-9]{3})", remove = FALSE) %>%
  mutate(Data = lapply(File, read_csv)) %>%
  unnest(Data) %>%
  select(-File)

# separate "timestamp" into date pieces and create a "DATE" column
data_dd1 <- separate(data_dd, TIMESTAMP, into = c("YEAR", "MONTHDAY"), sep = 4)
data_dd2 <- separate(data_dd1, MONTHDAY, into = c("MONTH", "DAY"), sep = 2)
data_dd3 <-unite(data_dd2, "DATE", YEAR, MONTH, DAY, sep = "-", remove = FALSE)

##### pull out 4 variables for analysis
dat_dd <- select(data_dd3, SITE, DATE, YEAR, MONTH, DAY, NEE_CUT_REF, 
                 NEE_CUT_REF_JOINTUNC, RECO_NT_CUT_REF, GPP_NT_CUT_REF) %>%
  filter(NEE_CUT_REF != "-9999") %>%
  filter(NEE_CUT_REF_JOINTUNC != "-9999") %>%
  filter(RECO_NT_CUT_REF != "-9999") %>%
  filter(GPP_NT_CUT_REF != "-9999")

##### for YY files
fileptrn_yy <- print(readfilename())
filenames_yy <- list.files(path, full.names = TRUE, pattern = fileptrn_yy, recursive = TRUE) # YY
#run through all data directories, add column for SITE and fill with regular eppression
data_yy <- tibble(File = filenames_yy) %>%
  extract(File, "SITE", "([A-Z]{2}-[A-Za-z0-9]{3})", remove = FALSE) %>%
  mutate(Data = lapply(File, read_csv)) %>%
  unnest(Data) %>%
  select(-File)

dat_yy <- select(data_yy, SITE, TIMESTAMP, NEE_CUT_REF, 
                 NEE_CUT_REF_JOINTUNC, RECO_NT_CUT_REF, GPP_NT_CUT_REF) %>%
  filter(NEE_CUT_REF != "-9999") %>%
  filter(NEE_CUT_REF_JOINTUNC != "-9999") %>%
  filter(RECO_NT_CUT_REF != "-9999") %>%
  filter(GPP_NT_CUT_REF != "-9999")

# tbl <- lapply(filenames, read_csv) %>% 
#   bind_rows()

##### Grouping for figures
# daily GPP by site and year
gpp_dd_siteyear <-dat_dd %>%
  group_by(SITE,YEAR) %>%
  dplyr::summarize(gpp_annual = sum(GPP_NT_CUT_REF, na.rm = TRUE)) %>%
  ungroup() %>% # -- Here's an example of why you need to ungroup! --
  dplyr::arrange(SITE)

# daily GPP by site and year
gpp_yy_site <- dat_yy %>%
  filter(GPP_NT_CUT_REF != "-9999.0000")

# # Global harvest by species category
# spcatch <- d %>%
#   group_by(year,spgroupname) %>%
#   dplyr::summarize(totalcatch=sum(catch, na.rm=T)) %>%
#   ungroup() %>% 
#   arrange(spgroupname)
# 
# # USA harvest by species category over time
# usa<- d %>%
#   filter(country=='United States of America') %>%
#   group_by(year,country,spgroupname) %>%
#   dplyr::summarize(totalcatch=sum(catch,na.rm=T)) %>%
#   ungroup() %>%
#   arrange(spgroupname)

##### plot some stuff
ggplot(data = gpp_yy_site, aes(x = SITE, y = GPP_NT_CUT_REF)) +
  geom_bar(stat="identity")




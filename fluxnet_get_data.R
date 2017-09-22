##### Load some libraries

require(tidyverse)
require(stringr)
require(readr)  # for read_csv()
require(dplyr)  # for mutate()
require(tidyr)  # for unnest()
require(purrr)  # for map(), reduce()
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

# FLX_[A-Z]{2}-[A-Za-z0-9]{3}_FLUXNET2015_[A-Z]{4,7}_[A-Z]{2}_[0-9]{4}-[0-9]{4}_[0-9]{1}-[0-9]{1}.csv ## everything
# FLX_[A-Z]{2}-[A-Za-z0-9]{3}_FLUXNET2015_FULLSET_DD_[0-9]{4}-[0-9]{4}_[0-9]{1}-[0-9]{1}.csv ### FULLSET DD files only
# FLX_[A-Z]{2}-[A-Za-z0-9]{3}$ #### sitename pattern

fileptrn <- print(readfilename())

filenames <- list.files(path, full.names = TRUE, pattern = fileptrn, recursive = TRUE)

# site <- str_extract(filenames, "FLX_[A-Z]{2}-[A-Za-z0-9]{3}_FLUXNET2015_[A-Z]{4,7}_[A-Z]{2}_[0-9]{4}-[0-9]{4}_[0-9]{1}-[0-9]{1}.csv")
# site <- str_extract(filenames, "[A-Z]{2}-[A-Za-z0-9]{3}")

##### run through all data directories, add column for SITE and fill with regular eppression
data <- tibble(File = filenames) %>%
  extract(File, "SITE", "([A-Z]{2}-[A-Za-z0-9]{3})", remove = FALSE) %>%
  mutate(Data = lapply(File, read_csv)) %>%
  unnest(Data) %>%
  select(-File)

# tbl <- lapply(filenames, read_csv) %>% 
#   bind_rows()

# separate "timestamp" into date pieces and create a "DATE" column
data1 <- separate(data, TIMESTAMP, into = c("YEAR", "MONTHDAY"), sep = 4)
data2 <- separate(data1, MONTHDAY, into = c("MONTH", "DAY"), sep = 2)
data3 <-unite(data2, "DATE", YEAR, MONTH, DAY, sep = "-", remove = FALSE)

##### pull out 4 variables for analysis
dat <- select(data3, SITE, DATE, YEAR, MONTH, DAY, NEE_CUT_REF, 
                     NEE_CUT_REF_JOINTUNC, RECO_NT_CUT_REF, GPP_NT_CUT_REF)

##### Grouping for figures
# GPP by site and year
gpp_siteyear <-dat %>%
  group_by(SITE,YEAR) %>%
  dplyr::summarize(gpp_annual = sum(GPP_NT_CUT_REF, na.rm = TRUE)) %>%
  ungroup() %>% # -- Here's an example of why you need to ungroup! --
  dplyr::arrange(SITE)

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
ggplot(data=gpp_siteyear, aes(x = SITE, y = gpp_annual)) +
  geom_bar(stat="identity")




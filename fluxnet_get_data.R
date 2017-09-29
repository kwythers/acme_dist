##### Load some libraries

require(tidyverse)
require(stringr)
require(lubridate)
require(zoo)

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
# FLX_[A-Z]{2}-[A-Za-z0-9]{3}_FLUXNET2015_FULLSET_DD_[0-9]{4}-[0-9]{4}_[0-9]{1}-[0-9]{1}.csv ### FULLSET daily files
# FLX_[A-Z]{2}-[A-Za-z0-9]{3}_FLUXNET2015_FULLSET_MM_[0-9]{4}-[0-9]{4}_[0-9]{1}-[0-9]{1}.csv ### FULLSET monthly files
# FLX_[A-Z]{2}-[A-Za-z0-9]{3}_FLUXNET2015_FULLSET_YY_[0-9]{4}-[0-9]{4}_[0-9]{1}-[0-9]{1}.csv ### FULLSET annual files
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

# pull out 4 daily variables for analysis
dat_dd <- select(data_dd3, SITE, DATE, YEAR, MONTH, DAY, NEE_CUT_REF, 
                 NEE_CUT_REF_JOINTUNC, RECO_NT_CUT_REF, GPP_NT_CUT_REF) %>% 
  mutate(DATE = as.Date(DATE)) %>% 
  mutate(NEE_CUT_REF = na_if(NEE_CUT_REF,"-9999")) %>% 
  mutate(NEE_CUT_REF_JOINTUNC = na_if(NEE_CUT_REF_JOINTUNC, "-9999")) %>% 
  mutate(RECO_NT_CUT_REF = na_if(RECO_NT_CUT_REF, "-9999")) %>%
  mutate(GPP_NT_CUT_REF = na_if(GPP_NT_CUT_REF,"-9999")) %>% 
  mutate_at(vars(contains("REF")), funs(as.numeric)) %>% 
  mutate_if(is.character, as.factor)

##### for MM files
fileptrn_mm <- print(readfilename())
filenames_mm <- list.files(path, full.names = TRUE, pattern = fileptrn_mm, recursive = TRUE) # MM
#run through all data directories, add column for SITE and fill with regular eppression
data_mm <- tibble(File = filenames_mm) %>%
  extract(File, "SITE", "([A-Z]{2}-[A-Za-z0-9]{3})", remove = FALSE) %>%
  mutate(Data = lapply(File, read_csv)) %>%
  unnest(Data) %>%
  select(-File)

# separate "timestamp" into date pieces and create a "DATE" column
data_mm1 <- separate(data_mm, TIMESTAMP, into = c("YEAR", "MONTH"), sep = 4) %>% 
  mutate(DAY = 15) %>% # add column for d 
  unite("DATE", YEAR, MONTH, DAY, sep = "-", remove = FALSE)

dat_mm <- select(data_mm1, SITE, DATE, YEAR, MONTH, NEE_CUT_REF, 
                 NEE_CUT_REF_JOINTUNC, RECO_NT_CUT_REF, GPP_NT_CUT_REF) %>% 
  mutate(DATE = as.Date(DATE)) %>%
  mutate(NEE_CUT_REF = na_if(NEE_CUT_REF, "-9999")) %>%
  mutate(NEE_CUT_REF_JOINTUNC = na_if(NEE_CUT_REF_JOINTUNC, "-9999")) %>%
  mutate(RECO_NT_CUT_REF = na_if(RECO_NT_CUT_REF, "-9999")) %>%
  mutate(GPP_NT_CUT_REF = na_if(GPP_NT_CUT_REF, "-9999")) %>%
  mutate_at(vars(contains("REF")), funs(as.numeric)) %>% 
  mutate_if(is.character, as.factor)

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
  mutate(NEE_CUT_REF = na_if(NEE_CUT_REF, "-9999")) %>%
  mutate(NEE_CUT_REF_JOINTUNC = na_if(NEE_CUT_REF_JOINTUNC, "-9999")) %>%
  mutate(RECO_NT_CUT_REF = na_if(RECO_NT_CUT_REF, "-9999")) %>%
  mutate(GPP_NT_CUT_REF = na_if(GPP_NT_CUT_REF, "-9999")) %>%
  mutate_at(vars(contains("REF")), funs(as.numeric)) %>% 
  mutate_if(is.character, as.factor)

# tbl <- lapply(filenames, read_csv) %>% 
#   bind_rows()

##### Grouping
# GPP by site
gpp_summary <- dat_yy %>% # the names of the new data frame and the data frame to be summarised
  group_by(SITE) %>%   # the grouping variable
  summarise(mean_GPP = mean(GPP_NT_CUT_REF, na.rm = TRUE),  # calculates the mean of each group
            sd_GPP = sd(GPP_NT_CUT_REF, na.rm = TRUE), # calculates the standard deviation of each group
            n_GPP = n(),  # calculates the sample size per group
            SE_GPP = sd(GPP_NT_CUT_REF, na.rm = TRUE)/sqrt(n())) # calculates the standard error of each group

# plot GPP by site with sd
ggplot(gpp_summary, aes(SITE, mean_GPP)) + 
  geom_col() +  
  geom_errorbar(aes(ymin = mean_GPP - sd_GPP, ymax = mean_GPP + sd_GPP), width=0.2) + 
  labs(y="GPP ± s.d.", x = "Species") + 
  geom_text(aes(label = n_GPP), size = 3, hjust = 0.5, vjust = 3, position = "stack") # + theme_classic()

# plot annual GPP by site
ggplot(dat_yy, aes(as.factor(TIMESTAMP), GPP_NT_CUT_REF)) + 
  geom_col(position = "dodge") + 
  facet_wrap(~ SITE, drop = TRUE,scales = "free_x") + 
  labs(title ="GPP (NT_CUT_REF)", x = "year", y = "annual gpp") + # lable control
  theme(axis.text.x = element_text(angle = 90)) #+ # rotate tic text to verticle

# plot GPP traces by site with daily data 
ggplot(dat_mm, aes(DATE, GPP_NT_CUT_REF)) +
  geom_line() +
  facet_wrap(~ SITE,scales = "free") +
  labs(title ="GPP (NT_CUT_REF)", x = "DATE", y = "DAILY GPP") + # lable control
  theme(axis.text.x = element_text(angle = 90)) # rotate tic text to verticle

# plot GPP traces by site with monthly data 
ggplot(dat_mm, aes(DATE, GPP_NT_CUT_REF)) +
  geom_line() +
  facet_wrap(~ SITE,scales = "free") +
  labs(title ="GPP (NT_CUT_REF)", x = "DATE", y = "MONTHLY GPP") + # lable control
  theme(axis.text.x = element_text(angle = 90)) # rotate tic text to verticle


# Pull out one site and plot
# au_tum_yy <- filter(dat_yy, SITE == "AU-Tum")
# ggplot(au_tum_yy, aes(as.factor(TIMESTAMP), GPP_NT_CUT_REF)) +
#   geom_bar(stat = "identity") +
#expand_limits(y = 0) +
#labs(title ="AU-Tum", x = "year", y = "annual gpp") # lable control
# scale_y_discrete(name = "GPP", breaks = c("0","1000","2000","3000", "4000"), 
#                  labels = c("0","1000","2000","3000", "4000")) +
# scale_x_discrete(name = "YEAR", breaks = c("2000","2005","2010", "2015"), 
#                  labels = c("2000","2005","2010", "2015"))
# theme(axis.text.x = element_text(angle = 90)) + # rotate tic text to verticle
# theme(axis.ticks = element_blank(), axis.text.y = element_blank()) # hide ticks and text

# gpp_yy_summary <-dat_dd %>%
#     group_by(SITE) %>%
#     dplyr::summarize(gpp_annual = sum(GPP_NT_CUT_REF, na.rm = TRUE)) %>%
#     ungroup() %>% # -- Here's an example of why you need to ungroup! --
#     dplyr::arrange(SITE)

# dist
# ggplot(dat_yy, aes(TIMESTAMP, GPP_NT_CUT_REF)) + 
#   geom_density(aes(y = GPP_NT_CUT_REF)) + 
#   facet_wrap(~ SITE, drop = TRUE, 
#              labeller = label_parsed) + 
#   labs(title ="GPP (NT_CUT_REF)", x = "year", y = "annual gpp") + # lable control
#   theme(axis.text.x = element_text(angle = 90)) + # rotate tic text to verticle
#   theme(axis.ticks = element_blank(), axis.text.y = element_blank()) # hide ticks and text

 # Pull out one site and plot
# au_tum_dd <- filter(dat_dd, SITE == "AU-Tum")
# ggplot(au_tum_dd, aes(DATE, as.numeric(GPP_NT_CUT_REF))) + 
#   geom_line()  
  # labs(title ="GPP (NT_CUT_REF)", x = "DATE", y = "DAILY GPP")  # lable control
  # theme(axis.text.x = element_text(angle = 90)) + # rotate tic text to verticle
  # theme(axis.ticks = element_blank(), axis.text.y = element_blank()) # hide ticks and text


# ggplot(data = dat_yy, aes(x = SITE, y = GPP_NT_CUT_REF)) +
#   geom_col() +  # geom_col() is replacement for geom_bar(stat = "identity")
#   # independent x-axis scale in each facet, 
#   # drop absent factor levels (actually not required here)
#   facet_wrap(~ TIMESTAMP, scales = "free_x", drop = TRUE) +
#   # use named character vector to replace x-axis labels
#   scale_x_discrete(labels = dat_yy[, setNames(as.character(SITE), GPP_NT_CUT_REF)]) + 
#   # replace x-axis title
#   xlab(NULL) +
#   # rotate x-axis labels
#   theme(axis.text.x = element_text(angle = 90, hjust=1, vjust=.5))






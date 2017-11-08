read_global_cell <- function() {
  global_cell <- read_csv("data/global_cell.csv")
  
  gather(global_cell,key = SITE,value = GPP,-X1) %>%
    rename(year=X1) -> global_cell_data
  return(global_cell_data)
}


get_gpp_yr_global_cell <- function(global_cell_data){
  ##### Grouping
  # GPP by site
  global_cell_data %>% # the names of the new data frame and the data frame to be summarised
    group_by(SITE) %>%   # the grouping variable
    summarise(mean_GPP = mean(GPP, na.rm = TRUE),  # calculates the mean of each group
              sd_GPP = sd(GPP, na.rm = TRUE), # calculates the standard deviation of each group
              n_GPP = n(),  # calculates the sample size per group
              SE_GPP = sd(GPP, na.rm = TRUE)/sqrt(n())) -> gpp_global_cell_summary # calculates the standard error of each group
  gpp_global_cell_summary$type <- "Global cell model"
  return(gpp_global_cell_summary)
}

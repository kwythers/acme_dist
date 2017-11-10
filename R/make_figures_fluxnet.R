get_gpp_yr_fluxnet <- function(data_yy_out){
  ##### Grouping
  # GPP by site
  data_yy_out %>% # the names of the new data frame and the data frame to be summarised
    group_by(SITE) %>%   # the grouping variable
    summarise(mean_GPP = mean(GPP_NT_CUT_REF, na.rm = TRUE),  # calculates the mean of each group
              sd_GPP = sd(GPP_NT_CUT_REF, na.rm = TRUE), # calculates the standard deviation of each group
              n_GPP = n(),  # calculates the sample size per group
              SE_GPP = sd(GPP_NT_CUT_REF, na.rm = TRUE)/sqrt(n())) -> gpp_summary # calculates the standard error of each group
  gpp_summary$type <- "Fluxnet"
  return(gpp_summary)
}

#same function as above but for model data
get_gpp_yr_model <- function(model_data_year){
  ##### Grouping
  # GPP by site
  model_data_year %>% # the names of the new data frame and the data frame to be summarised
    group_by(site_code) %>%   # the grouping variable
    summarise(mean_GPP = mean(gpp, na.rm = TRUE),  # calculates the mean of each group
              sd_GPP = sd(gpp, na.rm = TRUE), # calculates the standard deviation of each group
              n_GPP = n(),  # calculates the sample size per group
              SE_GPP = sd(gpp, na.rm = TRUE)/sqrt(n()))%>%
    dplyr::rename(SITE=site_code)-> gpp_model_summary # calculates the standard error of each group
  gpp_model_summary$type <- "PFT distribution model"
  return(gpp_model_summary)
}

get_gpp_yr_mean_model <- function(mean_model_data_year){
  ##### Grouping
  # GPP by site
  mean_model_data_year %>% # the names of the new data frame and the data frame to be summarised
    group_by(site_code) %>%   # the grouping variable
    summarise(mean_GPP = mean(gpp, na.rm = TRUE),  # calculates the mean of each group
              sd_GPP = sd(gpp, na.rm = TRUE), # calculates the standard deviation of each group
              n_GPP = n(),  # calculates the sample size per group
              SE_GPP = sd(gpp, na.rm = TRUE)/sqrt(n()))%>%
    dplyr::rename(SITE = site_code) -> gpp_mean_model_summary # calculates the standard error of each group
  gpp_mean_model_summary$type <- "Mean model"
  return(gpp_mean_model_summary)
}

get_gpp_yr_default_model <- function(default_model_data_year){
  ##### Grouping
  # GPP by site
  default_model_data_year %>% # the names of the new data frame and the data frame to be summarised
    group_by(site_code) %>%   # the grouping variable
    summarise(mean_GPP = mean(gpp, na.rm = TRUE),  # calculates the mean of each group
              sd_GPP = sd(gpp, na.rm = TRUE), # calculates the standard deviation of each group
              n_GPP = n(),  # calculates the sample size per group
              SE_GPP = sd(gpp, na.rm = TRUE)/sqrt(n()))%>%
    dplyr::rename(SITE = site_code) -> gpp_default_model_summary # calculates the standard error of each group
  gpp_default_model_summary$type <- "Default model"
  return(gpp_default_model_summary)
}

yr_gpp_fluxnet <- function(gpp_summary,gpp_model_summary,gpp_mean_model_summary,gpp_default_model_summary,gpp_global_cell_summary) {
  #gpp data for model and fluxnet site
  gpp_all <- bind_rows(gpp_summary,
                       gpp_model_summary,
                       gpp_mean_model_summary,
                       gpp_default_model_summary,
                       gpp_global_cell_summary)
  
  gpp_all$type <- factor(gpp_all$type, c("Fluxnet",
                                         "Default model",
                                         "Global cell model",
                                         "Mean model",
                                         "PFT distribution model"))
  
  # plot GPP by site with sd
  plot_out <-ggplot(gpp_all, aes(SITE, mean_GPP, fill=type)) + 
    scale_fill_brewer(palette = "Set1")+
    geom_col(position=position_dodge()) +  
    geom_errorbar(aes(ymin = mean_GPP - sd_GPP, ymax = mean_GPP + sd_GPP), width=0.2,position=position_dodge(.95)) + 
    labs(y="GPP ± s.d.", x = "Sites") + 
    geom_text(data=gpp_summary,aes(x=SITE,y=mean_GPP,label=n_GPP),vjust=-1.5,nudge_x = 0.15)+
    #geom_text(data = gpp_summary,aes(x=SITE, y=mean_GPP), size = 3, hjust = 0.5, vjust = 3, position = "stack") #+
    theme_classic()
  
  pdf("figures/fluxnet_sites_annual_gpp.pdf", width = 10)
  print(plot_out)
  dev.off()
  
  
}

panel_fluxnet_yrly_gpp <- function(data_yy_out) {
  
  plot_out <-ggplot(data_yy_out, aes(as.factor(TIMESTAMP), GPP_NT_CUT_REF)) + 
    geom_col(position = "dodge") + 
    facet_wrap(~ SITE, drop = TRUE,scales = "free_x") + 
    labs(title ="GPP (NT_CUT_REF)", x = "year", y = "annual gpp") + # lable control
    theme(axis.text.x = element_text(angle = 90)) #+ # rotate tic text to verticle
  
  pdf("figures/fluxnet_panel_yearly_gpp.pdf", width = 10)
  print(plot_out)
  dev.off()
  
}


panel_fluxnet_mnthly_ts_gpp <- function(data_mm_out,model_data_month,mean_model_data_month) {
  model_data_month %>%
    dplyr::rename(SITE=site_code,
                  YEAR=year,
                  MONTH=month,
                  GPP_NT_CUT_REF=gpp) %>%
    mutate(DAY = 15) %>% # add column for d 
    unite("DATE", YEAR, MONTH, DAY, sep = "-", remove = FALSE) %>%
    mutate(DATE = as.Date(DATE)) %>%
    select(-run,-mr,-npp,-lmr,
           -YEAR,-MONTH,-DAY) -> model_data_month

    mean_model_data_month %>%
    dplyr::rename(SITE=site_code,
                  YEAR=year,
                  MONTH=month,
                  GPP_NT_CUT_REF=gpp) %>%
    mutate(DAY = 15) %>% # add column for d 
    unite("DATE", YEAR, MONTH, DAY, sep = "-", remove = FALSE) %>%
    mutate(DATE = as.Date(DATE)) %>%
    select(-run,-mr,-npp,-lmr,
           -YEAR,-MONTH,-DAY) -> mean_model_data_month
  

  data_mm_out %>%
    select(-NEE_CUT_REF * days_in_month(DATE),
           -NEE_CUT_REF_JOINTUNC * days_in_month(DATE),
           -RECO_NT_CUT_REF * days_in_month(DATE),
           -YEAR,
           -MONTH) -> data_mm_out
  
  model_data_month$type <- "model"

  mean_model_data_month$type <- "mean_model"
    
  data_mm_out$type <- "fluxnet"
  
  # monthly_gpp_all <- bind_rows(model_data_month,data_mm_out,mean_model_data_month)
  monthly_gpp_all <- data_mm_out
  
  plot_out <- ggplot(monthly_gpp_all, aes(DATE, GPP_NT_CUT_REF,colour=type)) +
  geom_line() +
    scale_colour_brewer(palette="Set1")+
  facet_wrap(SITE ~ type,scales = "free") +
  labs(title ="GPP (NT_CUT_REF)", x = "DATE", y = "MONTHLY GPP") + # lable control
  theme(axis.text.x = element_text(angle = 90)) # rotate tic text to verticle
  
  pdf("figures/fluxnet_panel_mm_ts_gpp.pdf", width = 10)
  print(plot_out)
   dev.off()
   
}

panel_fluxnet_dly_ts_gpp <- function(data_dd_out) {
  
  plot_out <- ggplot(data_dd_out, aes(DATE, GPP_NT_CUT_REF)) +
  geom_line() +
  facet_wrap(~ SITE,scales = "free") +
  labs(title ="GPP (NT_CUT_REF)", x = "DATE", y = "DAILY GPP") + # lable control
  theme(axis.text.x = element_text(angle = 90)) # rotate tic text to verticle
  
  pdf("figures/fluxnet_panel_dd_ts_gpp.pdf", width = 10)
  print(plot_out)
  dev.off()
  
}


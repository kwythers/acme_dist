get_gpp_yr_fluxnet <- function(data_yy_out){
  ##### Grouping
  # GPP by site
  data_yy_out %>% # the names of the new data frame and the data frame to be summarised
    group_by(SITE) %>%   # the grouping variable
    summarise(mean_GPP = mean(GPP_NT_CUT_REF, na.rm = TRUE),  # calculates the mean of each group
              sd_GPP = sd(GPP_NT_CUT_REF, na.rm = TRUE), # calculates the standard deviation of each group
              n_GPP = n(),  # calculates the sample size per group
              SE_GPP = sd(GPP_NT_CUT_REF, na.rm = TRUE)/sqrt(n())) -> gpp_summary # calculates the standard error of each group
  return(gpp_summary)
}


yr_gpp_fluxnet <- function(gpp_summary) {
  
  # plot GPP by site with sd
  plot_out <-ggplot(gpp_summary, aes(SITE, mean_GPP)) + 
    geom_col() +  
    geom_errorbar(aes(ymin = mean_GPP - sd_GPP, ymax = mean_GPP + sd_GPP), width=0.2) + 
    labs(y="GPP ± s.d.", x = "Species") + 
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


panel_fluxnet_mnthly_ts_gpp <- function(data_mm_out) {
  
  plot_out <- ggplot(data_mm_out, aes(DATE, GPP_NT_CUT_REF)) +
  geom_line() +
  facet_wrap(~ SITE,scales = "free") +
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


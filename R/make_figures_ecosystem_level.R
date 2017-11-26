gpp_lai <- function(model_data_year, sites_pft){
  
  model_data_year %>%
    left_join(sites_pft, 
              by=c("site_code"), copy = FALSE) %>%
    mutate(short_pft =toupper(gsub("([a-zA-Z]{2})([a-z]{3,99})","\\1",PFTname))) %>%
    unite(col = site_pft, site_code,short_pft, sep="-") -> model_data_year
  
  p_out <-ggplot(model_data_year, aes(x = lai, y = gpp)) +
    geom_point(alpha=0.3, colour="orangered4")+
    geom_smooth(method = lm,colour="gray34")+
    scale_x_log10()+
    scale_y_log10()+
    facet_wrap(~site_pft, drop = TRUE, scales = "free")
  
  pdf("figures/gpp_lai.pdf", width = 10)
  print(p_out)
  dev.off()
  
} 


make_pft_key <- function() {
  pft_key <- data_frame(PFTNum= as.character(c(1:14)), PFTname=c("Temperate evergreen needleleaf",
                                                                 "Boreal evergreen needleleaf",
                                                                 "Boreal needleleaf deciduous tree",
                                                                 "Tropical evergreen broadleaf",
                                                                 "Temperate evergreen broadleaf",
                                                                 "Tropical deciduous broadleaf",
                                                                 "Temperate deciduous broadleaf",
                                                                 "Boreal broadleaf deciduous tree",
                                                                 "Temperate broadleaf evergreen shrub",
                                                                 "Temperate deciduous shrub",
                                                                 "Boreal deciduous shrub",
                                                                 "Arctic C3 grassland",
                                                                 "C3 grassland",
                                                                 "C4 grassland"))
  return(pft_key)
  
}

read_trait_data <- function(pft_key) {
  trait_dat <- read_csv("data/subPFT2.csv")
  #make trait data into three columns
  trait_dat<-lapply(split(as.list(trait_dat), cut(1:ncol(trait_dat), 14, labels = FALSE)), as.data.frame)
  #match name of columns across data sets
  trait_dat<-lapply(trait_dat,function(x){ colnames(x)<-c("sla","lnm","lls"); return(x)})
  #add run id to trait data 
  trait_dat<-lapply(trait_dat,function(x){ x$run<-as.numeric(row.names(x)); return(x)})
  trait_dat <-bind_rows(trait_dat,.id = "PFT")
  #join trait data to pft class
  trait_dat <- left_join(trait_dat,pft_key,by= c("PFT"="PFTNum"))
  return(trait_dat)
  
}

pft_lnm_sla <- function(trait_dat) {
  colourCount = length(unique(trait_dat$PFTname))
  getPalette = colorRampPalette(brewer.pal(11, "Spectral"))
  
  
  p_out <- ggplot(trait_dat, aes(y=lnm, x= sla ,color=PFTname)) + 
    geom_point()+
    geom_smooth(method = "lm", se=FALSE)+
    scale_colour_manual(values = getPalette(colourCount))+
    #scale_colour_brewer(type = "seq", palette = "Spectral")+
    scale_x_log10()+
    scale_y_log10()+
    facet_wrap(~PFTname)+
    theme_bw()+
    theme(legend.position = "none")
  
  
  pdf("figures/pft_traits_lnm_sla.pdf", width = 10)
  print(p_out)
  dev.off()
  
  
  
  
} 

pft_lls_sla <- function(trait_dat) {
  
  colourCount = length(unique(trait_dat$PFTname))
  getPalette = colorRampPalette(brewer.pal(11, "Spectral"))
  
  p_out <- ggplot(trait_dat, aes(y=lls, x= sla ,color=PFTname)) + 
    geom_point()+
    geom_smooth(method = "lm", se=FALSE)+
    scale_colour_manual(values = getPalette(colourCount))+
    #scale_colour_brewer(type = "seq", palette = "Spectral")+
    scale_x_log10()+
    scale_y_log10()+
    facet_wrap(~PFTname)+
    theme_bw()+
    theme(legend.position = "none")
  
  
  pdf("figures/pft_traits_lls_sla.pdf", width = 10)
  print(p_out)
  dev.off()
  
  
} 

pft_lnm_lls <- function(trait_dat) {
  colourCount = length(unique(trait_dat$PFTname))
  getPalette = colorRampPalette(brewer.pal(11, "Spectral"))
  
  p_out <- ggplot(trait_dat, aes(y=lnm, x= lls ,color=PFTname)) + 
    geom_point()+
    geom_smooth(method = "lm", se=FALSE)+
    scale_colour_manual(values = getPalette(colourCount))+
    #scale_colour_brewer(type = "seq", palette = "Spectral")+
    scale_x_log10()+
    scale_y_log10()+
    facet_wrap(~PFTname)+
    theme_bw()+
    theme(legend.position = "none")
    
    pdf("figures/pft_traits_lnm_lls.pdf", width = 10)
    print(p_out)
    dev.off()
  
} 

ternary_trait <- function(trait_dat,model_data_month_lai) {
  trait_dat %>%
    left_join(model_data_month_lai,
              by = c("PFT", "sla", "lnm", "lls", "run", "PFTname")) %>%
    filter(!is.na(npp), !is.na(gpp))%>%
    dplyr::mutate(sla=scale(log10(sla)),
                  lnm=scale(log10(lnm)),lls=scale(log10(lls))) -> trait_datz
  
  panel_out <- ggtern(data=trait_datz,
    aes(x = sla,y = lnm,z = lls, colour=as.numeric(gpp))) + 
    theme_rgbw() + 
    geom_point() +
    scale_colour_gradientn(colours = terrain.colors(unique(trait_datz$gpp)))+
    #scale_colour_manual(values = getPalette(colourCount))+
    facet_wrap(~PFTname, drop = TRUE)+
    labs(x="SLA",y="Leaf CN?",z="LLS",title="Title") +
    theme()
  
  p_out <- ggtern(data=trait_datz,
                      aes(x = sla,y = lnm,z = lls, colour=as.numeric(gpp))) + 
    theme_rgbw() + 
    geom_point() +
    scale_colour_gradientn(colours = terrain.colors(unique(trait_datz$gpp)))+
    labs(x="SLA",y="Leaf CN?",z="LLS",title="Title") +
    theme()
  
  legend <- get_legend(p_out)
  
  all <- plot_grid(p_out + theme(legend.position="none"),
                   panel_out + theme(legend.position="none"),
                   nrow=1, labels = c("A","B"))
  
  all_out <- plot_grid( all, legend, rel_widths = c(3, .3))  
  
  save_plot("figures/ternary_trait_plot.pdf", all_out, width=12)
  
}

max_gpp_by_lai <- function(sites_pft, trait_dat, model_data_month) {
  trait_dat <- left_join(trait_dat, sites_pft, by = "PFTname")
  
  model_data_month %>%
    group_by(run, site_code, year) %>%
    filter(lai==max(lai)) %>%
    ungroup() %>%
    group_by(run, site_code) %>%
    dplyr::summarise(gpp=mean(gpp,na.rm = TRUE),
                     npp=mean(npp,na.rm = TRUE),
                     lai=mean(lai, na.rm = TRUE)) %>%
    ungroup() %>%
    left_join(trait_dat, 
              by=c("run", "site_code"), copy = FALSE) %>%
    mutate(short_pft =toupper(gsub("([a-zA-Z]{2})([a-z]{3,99})","\\1",PFTname))) %>%
    unite(col = site_pft, site_code,short_pft, sep="-") -> model_data_month_lai
  
  model_data_month_lai %>%
    filter(!is.na(sla) & !is.na(lnm) & !is.na(lls)) -> model_data_month_lai
  return(model_data_month_lai)
  
}

  
plot_gpp_sla <- function(model_data_month_lai) {
  
  p_out <- ggplot(model_data_month_lai, aes(x = sla, y = gpp))+
    geom_point()+
    scale_x_log10()+
    scale_y_log10()+
    geom_smooth(method = lm)+
    facet_wrap(~ site_pft, drop = TRUE)
  
  pdf("figures/high_gpp_sla.pdf", width = 10)
  print(p_out)
  dev.off()
  
  
}

plot_gpp_lls <- function(model_data_month_lai) {
  
  p_out <- ggplot(model_data_month_lai, aes(x = lls, y = gpp))+
    geom_point()+
    scale_x_log10()+
    scale_y_log10()+
    geom_smooth(method = lm)+
    facet_wrap(~ site_pft, drop = TRUE)
  
  pdf("figures/high_gpp_lls.pdf", width = 10)
  print(p_out)
  dev.off()
  
  
}

plot_gpp_lnm <- function(model_data_month_lai) {
  
  p_out <- ggplot(model_data_month_lai, aes(x = lnm, y = gpp))+
    geom_point()+
    scale_x_log10()+
    scale_y_log10()+
    geom_smooth(method = lm)+
    facet_wrap(~ site_pft, drop = TRUE)
  
  pdf("figures/high_gpp_lnm.pdf", width = 10)
  print(p_out)
  dev.off()
  
}

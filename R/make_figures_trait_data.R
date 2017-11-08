
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

read_trait_data <- function(path,pft_key) {
  trait_dat <- read_csv(path)
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



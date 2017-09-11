library(tidyverse)
library(data.table)
library(RCurl)
library(ggjoy)
library(data.table)
dat <- fread("data/data_runAU-Tum_100.csv",sep = ",")
trait_dat <- read_csv("data/subPFT2.csv")
sites_pft <- read_csv("data/Fluxnet2015_longrecord.xlsx - Sheet1.csv",col_names = FALSE)
colnames(sites_pft) <-c("site_code","site_name","start_year","last_year","running_years","PFTname","lat","long","mat","map")
#add pft number to site data
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

###dat <- scp(host = "login.msi.umn.edu", path = "~/data_runs.csv", key = "~/.ssh/id_rsa.pub", user = "wythe001", keypasswd = "")

#make trait data into three columns
trait_dat<-lapply(split(as.list(trait_dat), cut(1:ncol(trait_dat), 14, labels = FALSE)), as.data.frame)
#match name of columns across data sets
trait_dat<-lapply(trait_dat,function(x){ colnames(x)<-c("sla","lnm","lls"); return(x)})
#add run id to trait data 
trait_dat<-lapply(trait_dat,function(x){ x$run<-as.numeric(row.names(x)); return(x)})
trait_dat <-bind_rows(trait_dat,.id = "PFT")
#join trait data to pft class
trait_dat <- left_join(trait_dat,pft_key,by= c("PFT"="PFTNum"))

#get dates
dat$date <- as.Date(as.character(dat$date), format='%Y%m%d')
dat$year <- format(dat$date, '%Y')
dat$month <- format(dat$date, '%m')
dat$day <- format(dat$date, '%d')

dat %>%
  mutate(npp=npp*3600,mr=mr*3600,lmr=lmr*3600, gpp=gpp*3600) -> dat

dat %>%
  group_by(run, country, site,year, month, day) %>%
  dplyr::summarise(mr = sum(mr, na.rm=TRUE),
                   npp=sum(npp, na.rm=TRUE),
                   gpp= sum(gpp, na.rm=TRUE),
                   lmr= sum(lmr, na.rm=TRUE)) -> data_short

data_short %>%
  group_by(run, country,site, year, month) %>%
  dplyr::summarise(mr = sum(mr, na.rm=TRUE),
                   npp=sum(npp, na.rm=TRUE),
                   gpp= sum(gpp, na.rm=TRUE),
                   lmr= sum(lmr, na.rm=TRUE)) -> data_shortmonth

data_short %>%
  group_by(run, country,site,year) %>%
  dplyr::summarise(mr = sum(mr, na.rm=TRUE),
                   npp=sum(npp, na.rm=TRUE),
                   gpp= sum(gpp, na.rm=TRUE),
                   lmr= sum(lmr, na.rm=TRUE)) %>%
  unite(site_code,country,site, sep="-")-> data_shortyear

data_shortyear <- left_join(data_shortyear,sites_pft )

data_shortyear <- left_join(data_shortyear,trait_dat,by = c("run", "PFTname"))

data_shortyear %>%
  mutate(lls=round(lls,2),sla=round(sla,2),lnm=round(lnm,2))->data_shortyear

data_short %>%
  group_by(run, country,site) %>%
  dplyr::summarise(mr = mean(mr, na.rm=TRUE),
                   npp=mean(npp, na.rm=TRUE),
                   gpp= mean(gpp, na.rm=TRUE),
                   lmr= mean(lmr, na.rm=TRUE)) %>%
  unite(site_code,country,site, sep="-")-> data_average
data_average <- left_join(data_average,sites_pft )

data_average <- left_join(data_average,trait_dat,by = c("run", "PFTname"))

data_average %>%
  mutate(lls=round(lls,2),sla=round(sla,2),lnm=round(lnm,2))->data_average

##

ggplot(data_average,aes(x=lnm,y=mr))+
  geom_bar(aes(fill=lls),stat="identity",colour="black")+
  #ylab()+labs(title = title)+
  theme_classic(base_size = 12)+scale_fill_continuous()

ggplot(data_average) + geom_density(aes(x=npp))+
  #scale_fill_brewer(type = "qual",palette = "Set1")+
  #scale_fill_brewer(type = "div")+
  #scale_fill_manual(values = getPalette(colourCount))+
  #facet_wrap(~lls)+
  xlab("Average Annual NPP")+
  theme_bw(base_size = 20)+
  theme()



library(RColorBrewer)
colourCount = length(unique(data_shortyear$lls))
getPalette = colorRampPalette(brewer.pal(11, "Spectral"))

ggplot(trait_dat, aes(y=sla, x= lnm,color=PFTname)) + 
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  scale_colour_manual(values = getPalette(colourCount))+
  #scale_colour_brewer(type = "seq", palette = "Spectral")+
  scale_x_log10()+
  scale_y_log10()+
  facet_wrap(~PFTname)+
  theme_bw()+
  theme()


cc <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=100))
ggplot(data_shortyear, aes(y=npp, x= mr,color=as.factor(lnm))) + 
  geom_point()+
  geom_smooth(method = "lm", se=FALSE)+
  scale_colour_manual(values=cc)+
  #scale_x_log10()+
  #scale_y_log10()+
  facet_wrap(~lnm)+
  theme_classic()+
  theme(legend.position = "none")

library(RColorBrewer)
colourCount = length(unique(data_shortyear$lls))
getPalette = colorRampPalette(brewer.pal(9, "YlGnBu"))

ggplot(data_shortyear) + geom_density(aes(x=mr, y=..scaled.., fill=as.factor(lls)), alpha=.5)+
  #scale_fill_brewer(type = "qual",palette = "Set1")+
  #scale_fill_brewer(type = "div")+
  scale_fill_manual(values = getPalette(colourCount))+
  facet_wrap(~lls)+
  xlab("Maintenance respiration")+
  theme_bw(base_size = 20)+
  theme()

ggplot(data_shortyear) + geom_density(aes(x=npp, y=..scaled.., fill=as.factor(lls)), alpha=.5)+
  #scale_fill_brewer(type = "qual",palette = "Set1")+
  #scale_fill_brewer(type = "div")+
  scale_fill_manual(values = getPalette(colourCount))+
  facet_wrap(~lls)+
  xlab("NPP")+
  theme_bw(base_size = 20)+
  theme()

data_short$date <- paste(data_short$year,data_short$month,sep= "_")
ggplot(data_short, aes(y=npp, x= mr,color=as.factor(run))) + 
  geom_point()+
  geom_smooth(colour="black")+
  facet_wrap(~run)+
  scale_color_brewer(type = "qual",palette = "Set1")+
  theme_classic()

ggplot(data_short, aes(y=npp, x= lmr,color=as.factor(run))) + 
  geom_point()+
  geom_smooth()+
  scale_color_brewer(type = "qual",palette = "Set1")+
  theme_classic()

ggplot(data_short, aes(y=npp, x= time,color=as.factor(run))) + 
  geom_point()+
  geom_smooth()+
  scale_color_brewer(type = "qual",palette = "Set1")+
  theme_classic()


mr <- ggplot(data_short) + geom_density(aes(x=mr, y=..scaled.., fill=as.factor(run)), alpha=.5)+
  scale_fill_brewer(type = "qual",palette = "Set1")+
  xlab("Maintenance respiration")+
  theme_bw(base_size = 20)
npp <-ggplot(data_short) + geom_density(aes(x=npp, y=..scaled.., fill=as.factor(run)), alpha=.5)+
  scale_fill_brewer(type = "qual",palette = "Set1")+
  xlab("NPP")+
  theme_bw(base_size = 20)
gpp <-ggplot(data_short) + geom_density(aes(x=gpp, y=..scaled.., fill=as.factor(run)), alpha=.5)+
  scale_fill_brewer(type = "qual",palette = "Set1")+
  xlab("GPP")+
  theme_bw(base_size = 20)
lmr <- ggplot(data_short) + geom_density(aes(x=lmr, y=..scaled.., fill=as.factor(run)), alpha=.5)+
  scale_fill_brewer(type = "qual",palette = "Set1")+
  xlab("Leaf maintenance respiration")+
  theme_bw(base_size = 20)
library(cowplot)

plotout <- plot_grid(mr, lmr, npp,gpp)
ggsave(plotout,"ecosystem_process_dist.pdf", device = NULL)
pdf("ecosystem_process_dist.pdf",width = 12,height = 10)

print(plotout)
dev.off()

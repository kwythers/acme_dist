library(tidyverse)
library(data.table)
library(RCurl)
library(ggjoy)
dat <- fread("data/data_runs.csv",sep = ",")
###dat <- scp(host = "login.msi.umn.edu", path = "~/data_runs.csv", key = "~/.ssh/id_rsa.pub", user = "wythe001", keypasswd = "")

dat$date <- as.Date(as.character(dat$date), format='%Y%m%d')
dat$year <- format(dat$date, '%Y')
dat$month <- format(dat$date, '%m')
dat$day <- format(dat$date, '%d')

dat %>%
  mutate(npp=npp*3600,mr=mr*3600,lmr=lmr*3600, gpp=gpp*3600) -> dat

dat %>%
  group_by(run, country,year, month, day) %>%
  dplyr::summarise(mr = sum(mr, na.rm=TRUE),
                   npp=sum(npp, na.rm=TRUE),
                   gpp= sum(gpp, na.rm=TRUE),
                   lmr= sum(lmr, na.rm=TRUE)) -> data_short

data_short %>%
  group_by(run, country,year, month) %>%
  dplyr::summarise(mr = sum(mr, na.rm=TRUE),
                   npp=sum(npp, na.rm=TRUE),
                   gpp= sum(gpp, na.rm=TRUE),
                   lmr= sum(lmr, na.rm=TRUE)) -> data_shortmonth

data_short %>%
  group_by(run, country,year) %>%
  dplyr::summarise(mr = sum(mr, na.rm=TRUE),
                   npp=sum(npp, na.rm=TRUE),
                   gpp= sum(gpp, na.rm=TRUE),
                   lmr= sum(lmr, na.rm=TRUE)) -> data_shortyear

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

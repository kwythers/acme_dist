### still fighting with the xyz format I want for ggtern...

library(RCurl)
library(tidyverse)
library(data.table)
library(ggtern)


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

trait_dat <- read_csv("data/subPFT2.csv")
trait_dat <- mutate(trait_dat, run_n = 1:100)
trait_dat <- select(trait_dat, run_n, everything())

trait_dat <-rename(trait_dat, 
           sla.tem_evgr_nl = sla, 
           sla.bor_evgr_nl = sla.1, 
           sla.bor_nl_dec_tree = sla.2, 
           sla.trop_evgr_bl = sla.3, 
           sla.tem_evgr_bl = sla.4, 
           sla.trop_dec_bl = sla.5, 
           sla.tem_dec_bl = sla.6, 
           sla.bor_bl_dec_tree = sla.7, 
           sla.tem_bl_evgr_shrub = sla.8, 
           sla.tem_dec_shrub = sla.9, 
           sla.bor_dec_shrub = sla.10, 
           sla.arc_c3_gl = sla.11, 
           sla.c3_gl = sla.12, 
           sla.c4_gl = sla.13,
           lnm.tem_evgr_nl = lnm, 
           lnm.bor_evgr_nl = lnm.1, 
           lnm.bor_nl_dec_tree = lnm.2, 
           lnm.trop_evgr_bl = lnm.3, 
           lnm.tem_evgr_bl = lnm.4, 
           lnm.trop_dec_bl = lnm.5, 
           lnm.tem_dec_bl = lnm.6, 
           lnm.bor_bl_dec_tree = lnm.7, 
           lnm.tem_bl_evgr_shrub = lnm.8, 
           lnm.tem_dec_shrub = lnm.9, 
           lnm.bor_dec_shrub = lnm.10, 
           lnm.arc_c3_gl = lnm.11, 
           lnm.c3_gl = lnm.12, 
           lnm.c4_gl = lnm.13,
           lls.tem_evgr_nl = lls, 
           lls.bor_evgr_nl = lls.1, 
           lls.bor_nl_dec_tree = lls.2, 
           lls.trop_evgr_bl = lls.3, 
           lls.tem_evgr_bl = lls.4, 
           lls.trop_dec_bl = lls.5, 
           lls.tem_dec_bl = lls.6, 
           lls.bor_bl_dec_tree = lls.7, 
           lls.tem_bl_evgr_shrub = lls.8, 
           lls.tem_dec_shrub = lls.9, 
           lls.bor_dec_shrub = lls.10, 
           lls.arc_c3_gl = lls.11, 
           lls.c3_gl = lls.12, 
           lls.c4_gl = lls.13
           )

# same result: x <- gather(trait_dat, starts_with("sla."),  starts_with("lnm."), starts_with("lls."), key = "pft", value = "value")

trait_dat_long <-  gather(trait_dat, 
             `sla.tem_evgr_nl`, 
             `lnm.tem_evgr_nl`, 
             `lls.tem_evgr_nl`, 
             `sla.bor_evgr_nl`, 
             `lnm.bor_evgr_nl`, 
             `lls.bor_evgr_nl`, 
             `sla.bor_nl_dec_tree`, 
             `lnm.bor_nl_dec_tree`, 
             `lls.bor_nl_dec_tree`,
             `sla.trop_evgr_bl`, 
             `lnm.trop_evgr_bl`, 
             `lls.trop_evgr_bl`, 
             `sla.tem_evgr_bl`, 
             `lnm.tem_evgr_bl`, 
             `lls.tem_evgr_bl`, 
             `sla.trop_dec_bl`, 
             `lnm.trop_dec_bl`, 
             `lls.trop_dec_bl`, 
             `sla.tem_dec_bl`, 
             `lnm.tem_dec_bl`, 
             `lls.tem_dec_bl`, 
             `sla.bor_bl_dec_tree`, 
             `lnm.bor_bl_dec_tree`, 
             `lls.bor_bl_dec_tree`,
             `sla.tem_bl_evgr_shrub`, 
             `lnm.tem_bl_evgr_shrub`, 
             `lls.tem_bl_evgr_shrub`, 
             `sla.tem_dec_shrub`, 
             `lnm.tem_dec_shrub`, 
             `lls.tem_dec_shrub`, 
             `sla.bor_dec_shrub`, 
             `lnm.bor_dec_shrub`, 
             `lls.bor_dec_shrub`, 
             `sla.arc_c3_gl`, 
             `lnm.arc_c3_gl`, 
             `lls.arc_c3_gl`, 
             `sla.c3_gl`, 
             `lnm.c3_gl`, 
             `lls.c3_gl`, 
             `sla.c4_gl`, 
             `lnm.c4_gl`, 
             `lls.c4_gl`, 
             key = "pft", value = "value")

# same result: x <- gather(trait_dat, starts_with("sla."),  starts_with("lnm."), starts_with("lls."), key = "pft", value = "value")

sla <- gather(trait_dat, starts_with("sla."), key = "sla", value = "value") %>%
  select(run_n, sla, value)

lnm <- gather(trait_dat, starts_with("lnm."), key = "lnm", value = "value") %>%
  select(run_n, lnm, value)

lls <- gather(trait_dat, starts_with("lls."), key = "lls", value = "value") %>%
  select(run_n, lls, value)

##### plot some data with ggtern ##### 
plot <- ggtern(data = df, aes(x = Xyp, y = XO, z = XY)) + 
  geom_point(aes(fill = Root),
             size = 4, 
             shape = 21, 
             color = "black") + 
  ggtitle("SLA LNM LLS by PFT") + 
  labs(fill = " xxx ") + 
  theme_tern_rgbw() + 
  theme(legend.position      = c(0,1), 
        legend.justification = c(0, 1))

#Render
plot

#dat <- scp(host = "wythe001@login.msi.umn.edu", path = "/home/reichpb/wythe001/acme_dist/data/model_output/data_AU-Tum_run_100.csv", key = "~/.ssh/id_rsa.pub", user = "wythe001", keypasswd = "")

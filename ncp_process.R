# Load some libraries
#library(rPython)
#library(RNetCDF)
library(tidyverse)
library(ncdf4)
library(data.table)
library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(zoo)
library(scales)

readpath <- function()
{ 
  n <- readline(prompt="Enter path to nc files without starting or finishing slash (e.g. ACME/source/run/trait_dist) : ")
  return(as.character(n))
}

readfilename <- function()
{ 
  n <- readline(prompt="Enter file or file pattern : ")
  return(as.character(n))
}

#Current path ~/../chen1718/ACME/source/run
path <- print(readpath())
#current file pattern trait_dist[0-9]_US-UMB_I20TRCLM45CN.clm2.h0.+.nc
fileptrn <- print(readfilename())
#"trait_dist[0-9]_US-UMB_I20TRCLM45CBCN.clm2.h0.+.nc"
#filenames <- list.files(path,full.names = TRUE,pattern = "trait_dist*",recursive = TRUE)
filenames <- list.files(path,full.names = TRUE,pattern = fileptrn,recursive = TRUE)
#grep(pattern = "trait_dist[0-9]_US-UMB_I20TRCLM45CBCN.clm2.h0.+.nc",filenames,value = TRUE,perl = TRUE)

get_npp <- function(filenames){
  # read individual netcdf variables into arrays
  nc <- nc_open(filenames)
  
  lmr <- ncvar_get(nc, "LEAF_MR")
  mr <- ncvar_get(nc, "MR")
  gpp <- ncvar_get(nc, "GPP")
  npp <- ncvar_get(nc, "NPP")
  mcdate <- ncvar_get(nc, "mcdate") #ncdf4
  
  nc_close(nc)
  
  name_run <- strsplit(x=filenames,
                       split = "\\/|\\.nc",perl = TRUE)[[1]]
  name_run <- grep(pattern = "trait_dist.+clm2.h0.+",x = name_run,value = TRUE)
  run_id <- name_run
  
  #TODO if lenght of all variables is not the same throw error
  data <- data.frame(date = mcdate, gpp=gpp, npp=npp, lmr=lmr, mr=mr)
  
  
  data <- cbind(run_id,data)
  data$run_id <- gsub("trait_dist",replacement = "",x = data$run_id)
  
  data <-separate(data,col = run_id,sep = "\\.|_|\\-",
                  into = c("run","country","site","model_version","clm_version","V6","year","month","day","sec"))
  data$V6 <- NULL
  return(data)
  
}

data <- lapply(filenames,get_npp)
data_all <- ldply(data,data.frame)


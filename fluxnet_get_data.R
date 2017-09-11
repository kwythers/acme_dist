##### Load some libraries

#library(rPython)
#library(RNetCDF)
library(tidyverse)
#library(ncdf4)
library(data.table)
library(plyr)
library(ggplot2)
library(grid)
library(gridExtra)
#library(zoo)
library(scales)

readpath <- function()
{ 
  n <- readline(prompt="Enter path to nc files without starting or finishing slash (e.g. ~/Google Drive/globland/data/fluxnet/) : ")
  return(as.character(n))
}

readfilename <- function()
{ 
  n <- readline(prompt="Enter file or file pattern : ")
  return(as.character(n))
}

#Current path ~/../shared/fluxnet/fluxnet/15_site_fluxnet_data/
#Current path ~/Google Drive\\data/fluxnet/15_site_fluxnet_data/
path <- print(readpath())
#current file pattern FLX_[A-Z]{2}-[a-zA-Z0-9]{3}FLUXNET2015*[0-9]{4}-[0-9]{4}_[0-9]-[0-9].csv
fileptrn <- print(readfilename())
#filenames <- list.files(path,full.names = TRUE,pattern = "trait_dist*",recursive = TRUE)
filenames <- list.files(path, full.names = TRUE, pattern = fileptrn, recursive = TRUE)
#grep(pattern = "trait_dist[0-9]_US-UMB_I20TRCLM45CBCN.clm2.h0.+.nc",filenames,value = TRUE,perl = TRUE)

library(parallel)
library(plyr)
library(data.table)
library(ncdf4)
library(tidyr)

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

#Enter path to files
path <- print(readpath())
#enter file pattern
fileptrn <- print(readfilename())
#get list of iles
filenames <- list.files(path,full.names = TRUE,pattern = fileptrn,recursive = TRUE)


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


#create cluster
#Calculate number of cores
# TO DO 11 cores in MSI?
no_cores <- detectCores() - 1
#Initialise cluster
cl <- makeCluster(no_cores)
#get library support needed to run the code
clusterEvalQ(cl,{
  library(ncdf4)
  library(tidyr)
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
  
  
})

# parallel replicate...
ptm <- proc.time()
data <-parLapply(cl,filenames,get_npp)
proc.time() - ptm
#stop the cluster
stopCluster(cl)
data_all <- ldply(data,data.frame)
fwrite(data_all,file = "data_runs.csv",sep = ",",row.names = FALSE,col.names = TRUE)

#function to get model output from file name
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


#appply get_npp_data in parallel
#get_npp data is a function wrappped inside paralist
paralist <- function(filenames){
  
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
  #return(data)
  data_all <- ldply(data,data.frame)
  #return(data_all)
  name_run2 <- strsplit(x=filenames,split = "\\/|\\.nc",perl = TRUE)[[1]]
  name_run2 <- grep(pattern = "trait_dist.+clm2.h0.+",x = name_run2,value = TRUE)
  run_id2 <- name_run2
  run_id2 <- gsub("trait_dist",replacement = "",x = run_id2)
  run_id2 <- strsplit(run_id2, split = "_")
  runnm <- run_id2[[1]][1]
  sitenm <- run_id2[[1]][2]
  fwrite(data_all,file = paste0("data/model_output/data_",sitenm,"_run_",runnm,".csv"),sep = ",",row.names = FALSE,col.names = TRUE)
  
}


#get the ouput of the mote carlo from site level models
get_model_data <- function(path) {
  path <- path
  fileptrn <- "trait_dist[0-9]{1,3}_[A-Z]{2}-[a-z,A-Z0-9]{0,3}_I20TRCLM45CN.clm2.h0.+.nc"
  
  #get list of files
  filenames <- list.files(path,full.names = TRUE,pattern = fileptrn,recursive = TRUE)
  
  #make list of file names per site
  dat_in <-as.data.frame(filenames)
  
  dat_in %>%
    separate(filenames,sep = "/",into = c("V1","V2","V3",
                                          "V4","V5","V6",
                                          "V7","V8","V9",
                                          "V10","V11","V12"),remove = FALSE) %>%
    separate(V12,sep="_",c("L1","L2","L3")) %>%
    select(filenames,L3)->fl_out
  file_list <-split(fl_out$filenames,fl_out$L3)
  #make list back to characters
  file_list <-sapply(file_list,as.character)
  dir.create(path = "data/model_output",FALSE,TRUE)
  
  lapply(file_list,paralist)
  remake:::zip_dir("data/model_output","model_output.zip")

    
}

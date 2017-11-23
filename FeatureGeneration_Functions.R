


###############
########
## Function that adds the gender information to the feature table
##Input
# features: The feature table
##Output:
# The feature table including gender information
########
###############
addGender <- function(features) {
  GENDER <- c(1,1,1,2,2,1,1,1,2,2,1,2,2,1,1,2,2,2,1,2,2,1,1,1,2) ##Male: 1, Female: 2 
  
  features$Gender <- unlist(lapply(split(features$Part,features$Part),function(x){rep(GENDER[x[1]],length(x))}))
  return(features)
}





###############
########
## This function generates the features for a given feature type and writes them to a file
##Input:
# participants: The list of valid participants
# FEAT_TYPE: Whether we generate "Affectiva" or "Physiology" features
# CENTERING_MODE: 
#     -"TRIAL": raw signal is centered on the trial level, by subtracting the mean of the strip 
#     -"PART": features are centered on the participant level, by subtracting the mean of the whole participant
#     -"BOTH": raw signal is centered on the trial level, by subtracting the mean of the strip. Then, features are centered on the participant level, by subtracting the mean of the whole participant
#     -"MIXED": raw signal is centered on the trial level for EDA only. Then, ALL the features are centered on the participant level, by subtracting the mean of the whole participant. This mode is only valid if FEAT_TYPE  == "Physiology"
#   -In any case, features are divided by standard deviation of the participant.
# raw_data_dir: the path to the raw data folder
# separator: the path separator used by the operative system ("/" or "\\") 
# affect_data_dir: the path to the affectiva data folder (only reqiured for FEAT_TYPE == Affecitva)
## Output:
#The feature table
########
###############

generateFeatures <- function (participants, FEAT_TYPE, CENTERING_MODE, raw_data_dir, separator, affect_data_dir) {
  if (CENTERING_MODE == "MIXED" & FEAT_TYPE != "Physiology"){
    print("MIXED mode only makes sense using physiology features!!")
    stop()
  }
  data_p <- data.frame()
  for (Curr_part in 1:length(participants)) {
  	Part <- participants[Curr_part]
  	base_part_fname <- paste(raw_data_dir,Part,Part,sep=separator)

  	if (FEAT_TYPE == "Physiology") {
  	  # Read raw physiological data
      feat <- readRawPhys(base_part_fname)
  	}
  	else if (FEAT_TYPE == "Affectiva") {
  	  base_part_aff_fname <- paste(affect_data_dir,Part,sep=separator)
  	  feat <- readRawAffect(base_part_fname,base_part_aff_fname)
  	}

  	#Extract for each feedback, the Tini and Tend of each window, and features
  	data <- data.frame()
  	#We start from event 3 as the first is the ini marker, and the second is the first strip which we reject for sanity purposes
  	for (i in 3:(length(feat$events$Event)-1)) {
  	  #ignore strips that the user pressed "next", that is, did not give feedback on the event
  	  if (feat$events$Event[i] == "0") next
  	  #define the time windows
  	  wT <- c(feat$events$Time[i-1],feat$events$Time[i]) ##window for the whole strip
  	  wI <- c(wT[1],wT[1]+4)                   ##window for the first 4 seconds of strip
  	  wM <- c(diff(wT)/2+wT[1]-2,diff(wT)/2+wT[1]+2)       ##window for the middle 4 seconds of strip
  	  wE <- c(wT[2]-4,wT[2])                   ##window for the last 4 seconds of strip
  	  wS <- c(wT[2]-2,wT[2]+5)       ##special window for the -2,+5 seconds at the end of strip
  	  
  	  if (CENTERING_MODE == "TRIAL" | CENTERING_MODE == "BOTH") {
  	    #We select only the raw data relevant for the current strip, and
  	    #the raw signal is centered around the mean of WT, including the signal within wS.
  	    aux <- lapply(feat$data[,feat$FEAT_RANGE],function(x){
  	        x[feat$data$Time > wT[1] & feat$data$Time < wS[2]]-mean(x[feat$data$Time > wT[1] & feat$data$Time < wT[2]],na.rm=TRUE)
  	      })
  	    aux_feat <- as.data.frame(aux)
  	  }
  	  else if (CENTERING_MODE == "MIXED"){
  	      #we center the raw EDA signal around the mean of the WT
  	      aux_feat_Eda <- feat$data$Eda[feat$data$Time > wT[1] & feat$data$Time < wS[2]] - mean(feat$data$Eda[feat$data$Time > wT[1] & feat$data$Time < wT[2]],na.rm=TRUE)
  	      #We select only the raw data relevant for the current strip
  	      aux_feat <- feat$data[feat$data$Time > wT[1] & feat$data$Time < wS[2],feat$FEAT_RANGE]
  	      #We overwrite the Eda signal with the centered one
  	      aux_feat$Eda <- aux_feat_Eda
  	  }
  	  else if (CENTERING_MODE == "PART"){
  	    #We select only the raw data relevant for the current strip
  	    aux_feat <- feat$data[feat$data$Time > wT[1] & feat$data$Time < wS[2],feat$FEAT_RANGE]
  	  }
  	  aux_time <- feat$data$Time[feat$data$Time > wT[1] & feat$data$Time < wS[2]]
  	  
  	  #Generate features for each time window
  	  windows <- list("wT"=wT,"wI"=wI,"wM"=wM,"wE"=wE,"wS"=wS)
  	  feat_per_windows <- lapply(windows,function(w){
  	    ind_window <- aux_time > w[1] & aux_time < w[2]
  	    Sum <-  lapply(aux_feat[ind_window,],mean,na.rm=TRUE)
  	    Diff <- lapply(aux_feat[ind_window,],function(x){mean(diff(x),na.rm=TRUE)})
  	    Diffsq <- lapply(aux_feat[ind_window,],function(x){mean(diff(x)^2,na.rm=TRUE)})
  	    if (FEAT_TYPE == "Affectiva") {
  	      list("sum"=Sum,"diff"=Diff,"diffsq"=Diffsq)
  	    }
  	    else if (FEAT_TYPE == "Physiology") {  #In case of physiology features, add features related to SCRs
  	      nscr <- sum(feat$SCRs$OnsetTime > w[1] & feat$SCRs$OnsetTime < w[2],na.rm=TRUE)
  	      if (nscr > 0) {
  	        scr <- list(
  	          "nSCR" = nscr,
  	          "meanAmp" = mean(feat$SCRs$maxAmp[feat$SCRs$OnsetTime > w[1] & feat$SCRs$OnsetTime < w[2]],na.rm=TRUE),
  	          "max" = max(feat$SCRs$maxAmp[feat$SCRs$OnsetTime > w[1] & feat$SCRs$OnsetTime < w[2]],na.rm=TRUE),
  	          "latMax" = feat$SCRs$OnsetTime[feat$SCRs$OnsetTime > w[1] & feat$SCRs$OnsetTime < w[2]][which.max(feat$SCRs$maxAmp[feat$SCRs$OnsetTime > w[1] & feat$SCRs$OnsetTime < w[2]])] - w[1],
  	          "latFirst" = feat$SCRs$OnsetTime[feat$SCRs$OnsetTime > w[1] & feat$SCRs$OnsetTime < w[2]][1] - w[1]
  	        )
  	      } else { 
  	        scr <- list("nSCR" = 0,"meanAmp" = 0,"max" = 0,"latMax" = 0,"latFirst" = 0)
  	      }
  	      list("sum"=Sum,"diff"=Diff,"diffsq"=Diffsq,"scr"=scr)
  	    }
  	    
  	  })
  	  
  	  
  	  if (CENTERING_MODE == "TRIAL" | CENTERING_MODE == "BOTH") { 
  	    #Get rid of the sum features for the wT window as it will be 0
  	    feat_per_windows[["wT"]]<- feat_per_windows[["wT"]][-1]
  	  }
  	  else if (CENTERING_MODE == "MIXED") {
  	    #Get rid of the sum feature for the wT window of EDA, as it will be 0
  	    feat_per_windows[["wT"]][["sum"]]<- feat_per_windows[["wT"]][["sum"]][-which(names(feat_per_windows[["wT"]][["sum"]]) == "Eda")]
  	    
  	  }
  	  
  	  if (FEAT_TYPE == "Physiology") { ##Additional post processing for physioogy features
  	    #Add latMaxRelat and latFirstRelat for wT window
  	    feat_per_windows[["wT"]][["scr"]]$latMaxRelat <- feat_per_windows[["wT"]][["scr"]]$latMax / diff(windows[["wT"]])
  	    feat_per_windows[["wT"]][["scr"]]$latFirstRelat <- feat_per_windows[["wT"]][["scr"]]$latFirst / diff(windows[["wT"]])
  	    
  	    #Get rid of the scr features for the wS window as it will be overlapping in next strip
  	    feat_per_windows[["wS"]]<-feat_per_windows[["wS"]][-4]
  	  }
  	  
  	  data <- rbind(data,data.frame(cbind("Part"=as.numeric(strsplit(Part,"P")[[1]][2]),"Class"=feat$events$Event[i],"Duration"=diff(windows[["wT"]]),as.data.frame(feat_per_windows))))
  	}
  
  	data_p <- rbind(data_p,data)
  }
  
  data_p <- addGender(data_p)
  #Order the table as follows: "Part","Class","Gender","Duration",FEATURES
  data_p <- data_p[,c(1:2,ncol(data_p),3,4:(ncol(data_p)-1))]
  FEATURE_RANGE <- 5:ncol(data_p)
  
  if (CENTERING_MODE != "TRIAL") { 
    #We perform participant centering on the features set
    aux <- lapply(split(data_p, data_p$Part),function(y) {apply(y[,FEATURE_RANGE], 2, function(x){x-mean(x,na.rm=TRUE) }) })
    data_p[,FEATURE_RANGE] <- do.call(rbind,aux)
  }
  
  ## Normalize dividing by standard deviation of participant
  aux <- lapply(split(data_p, data_p$Part),function(y) {apply(y[,FEATURE_RANGE], 2, function(x){x/sd(x,na.rm=TRUE) }) })
  data_p[,FEATURE_RANGE] <- do.call(rbind,aux)
  
  return(data_p)
}




###############
########
## Function that splits the feature table in two, so that each subtable contains every other participant, keeping gender balance
##Input
# features: The feature table
##Output:
# A list containing the two subtables ("Train" and "Test")
########
###############
splitFeatures <- function (features) {
  #Select every other subject while keeping genderbalance
  train_Part <- unlist(
    lapply(split(features,features$Gender),function(x){
      unique(x$Part)[c(TRUE, FALSE)]
    })
  )
  test_Part <- unlist(lapply(split(features,features$Gender),function(x){
    unique(x$Part)[c(FALSE, TRUE)]
  })
  )
  # list("Train" = features[features$Part %in% train_Part,], "Test" = features[features$Part %in% test_Part,])
  
  #For debug purposes
  train_Part <- c(1,3,4,8,9,12,14,16,19,23,25)
  list("Train" = features[features$Part %in% train_Part,], "Test" = features[!(features$Part %in% train_Part),])
  
}






# ###############
# ########
# ## This function reads the raw physiological data for a participant and makes the initial pre-processing
##Input:
# base_fname: the path to the participant folder (including stem of the filename)##Output:
# events: The participant's events, starting from Time 0
# data: the physiological features
# SCRs: the information regarding skin conductance response (scr) events
# FEAT_RANGE: the column numbers of features to be used
# ########
# ###############
readRawPhys <- function(base_fname) {
  #read events information
  events <- read.csv(paste(base_fname,".csv",sep=""))
  
  #read physiology information
  phys <- read.csv(paste(base_fname,"_phys.txt",sep=""))
  names(phys) <-c("Time","Theta","Alpha","Beta","Gamma1","Gamma2","EKGIBI","EKGHRfromIBI","EKGHRSmoothed","EKGIBIpeakfreq","EKGVLFPercpower","EKGLFPercpower","EKGHFPercpower","EKGVLFTotalpower","EKGLFTotalpower","EKGHFTotalpower","Event")
  phys$Eda <- read.csv(paste(base_fname,"_eda256.txt",sep=""))[[1]]
  #Add Engage feature
  phys$Engage <- phys$Beta/(phys$Alpha+phys$Theta)
  #Set divisions by zero to 0 (as lack of Alpha+Theta can e interpreted as lack of Engage)
  phys$Engage[!is.finite(phys$Engage)] <- 0

  #Eda 8hz used for peakdetection algorithm
  eda8 <- read.csv(paste(base_fname,"_eda8.txt",sep=""))
  names(eda8) <- c("Time","Eda")
  
  
  ##Homogenize Timestamps so that iniMarker indictates time 0
  #events iniMarker Timestamp to 0
  events$Time <- events$Time - events$Time[which(events$Event == 88)]
  
  #physiological features timestamp to 0
  iniTime <- phys$Time[which(phys$Event == "M1")] 
  phys$Time <- phys$Time - iniTime
  eda8$Time <- eda8$Time - iniTime
  #get rid of data before iniMarker 
  phys <- phys[phys$Time >= 0,]
  eda8 <- eda8[eda8$Time >= 0,]
  

  
  ##extract SCRs using peak_detector function
  scr <- detectPeaks(eda8, percAmplitude=0.01, useRelativeValues = TRUE)
  
  # We are considering only the following signal outputs from Biograph software:
  #"Theta","Alpha","Beta","Gamma1","Gamma2","EKGIBI","EKGIBIpeakfreq","EKGVLFPercpower","EKGLFPercpower","EKGHFPercpower","Eda","Engage" 
  feat_range <- c(2:7,10:13,18:19)
  
  list("events" = events, "data" = phys, "SCRs" = scr, "FEAT_RANGE" = feat_range)
  
}


# ###############
# ########
# ## This function reads the raw affectiva data for a participant and makes the initial pre-processing
##Input:
# base_fname: the path to the participant folder (including stem of the filename) containing the event data
# base_aff_fname: the path to the participant folder (including stem of the filename) containing the affectiva data
##Output:
# events: The participant's events, starting from Time 0
# data: the affectiva features
# FEAT_RANGE: the column numbers of features to be used
# ########
# ###############
readRawAffect <- function(base_fname,base_aff_fname) {
  #read events information
  events <- read.csv(paste(base_fname,".csv",sep=""))

  #events iniMarker Timestamp to 0
  events$Time <- events$Time - events$Time[which(events$Event == 88)]
  #read camera features
  affect <- read.csv(paste(base_aff_fname,".csv",sep=""))
  
  
  EMOTION_RANGE <-c(10:18)
  FAC_EXPR_RANGE <- c(19:33)
  EMOJI_RANGE <- c(34:45)
  # We are considering facial expression and emotion features    
  feat_range <- c(FAC_EXPR_RANGE,EMOTION_RANGE)
  list("events" = events, "data" = affect, "FEAT_RANGE" = feat_range)
  
}




# ###############
# ########
# ## This function reads the physiological features data
##Input:
# feat_file_path the path to the data file
##Output:
# The formatted physiological features data
# ########
# ###############
readPhysData<- function(feat_file_path) {
  feat <- read.csv(feat_file_path)
  
  # ##in the case of phys features, only scr.diff and scr.diffsq are NA, in case there is only 1 or 0 scrs. We therefore assign to 0.
  # ## We can check this with the below:
  # ## a <- apply(feat[which(!complete.cases(feat)),],2,function(x){sum(is.na(x))>0})
  # ## a[a==TRUE]
  # feat[is.na(feat)] <- 0
  # 
  # #get rid of non-valid participant
  # feat <- feat[feat$Part != 18,]
  
  #get rid of WTF class
  feat <- feat[feat$Class != 2,]
  feat$Class <- factor(feat$Class,labels=c('notfun','fun'))

  return(feat)
}



# ###############
# ########
# ## This function reads the physiological and affectiva data
##Input:
# aff_file_path: the path to the affectiva data file
# phys_file_path: the path to the physiological data file
##Output:
# The formatted features data containing only trials where both affectiva and physiological data is available
# ########
# ###############
readAffectivaData<- function(aff_file_path,phys_file_path) {
  feat <- read.csv(aff_file_path)
  feat2 <- read.csv(phys_file_path)
  
  # feat2[is.na(feat2)] <- 0
  # #get rid of non-valid participant
  # feat2 <- feat2[feat2$Part != 18,]
  
  #We only consider participants that have data both for Physiology and for Affectiva
  feat2 <- feat2[feat2$Part %in% feat$Part,]
  
  #Bind the two datasets
  feat <- cbind(feat[,1:4],feat2[,-(1:4)],feat[,-(1:4)])
  
  #Select only trials for which we have enough affective data
  feat <- feat[complete.cases(feat),]
  
  #get rid of WTF class
  feat <- feat[feat$Class != 2,]
  feat$Class <- factor(feat$Class,labels=c('notfun','fun'))
  
  return (feat)
}






# ###############
# ########
# ## This function saves the prediction table to a file
##Input:
# p: The Predition Table to save
# path: the path to the folder where to save the files
# pred_algo: the algorithm used for prediction
# opt_text: optional argument to be appended at the end of the filename
##Output:
# 3 Files:
# -predictionTable_NFEAT.csv, containing the prediction table using NFEAT features
# -varImp.csv, containing the feature weights
# -TrainRatio.csv, containing the class distribution in the training set
# -TestRatio.csv, containing the class distribution in the test set
# ########
# ###############
savePredictionTable <-function(p,path,pred_algo,opt_text) {
  if(missing(opt_text)){
    write.csv(p$predict_table,paste(path,"predictionTable_",pred_algo,"_",length(colnames(p$varImp)),".csv",sep=""),row.names=F)
    write.csv(p$varImp,paste(path,"varImp_",pred_algo,"_",length(colnames(p$varImp)),".csv",sep=""))
    write.csv(p$train_ratio,paste(path,"TrainRatio_",pred_algo,"_",length(colnames(p$varImp)),".csv",sep=""),row.names=F)
    write.csv(p$test_ratio,paste(path,"TestRatio_",pred_algo,"_",length(colnames(p$varImp)),".csv",sep=""),row.names=F)
  } else {
    write.csv(p$predict_table,paste(path,"predictionTable_",pred_algo,"_",length(colnames(p$varImp)),"_",opt_text,".csv",sep=""),row.names=F)
    write.csv(p$varImp,paste(path,"varImp_",pred_algo,"_",length(colnames(p$varImp)),"_",opt_text,".csv",sep=""))
    write.csv(p$train_ratio,paste(path,"TrainRatio_",pred_algo,"_",length(colnames(p$varImp)),"_",opt_text,".csv",sep=""),row.names=F)
    write.csv(p$test_ratio,paste(path,"TestRatio_",pred_algo,"_",length(colnames(p$varImp)),"_",opt_text,".csv",sep=""),row.names=F)
  }
}


###############
########
## This function saves the feature ranking to a file
##Input:
# rank: The feature rank to save
# path: the path to the folder where to save the files
##Output:
# rank.csv, containing the rank
########
###############
saveRank <- function(rank,path) {
  print(paste("Rank saved to: ",path,sep=""))
  write.csv(rank,paste(path,"rank.csv",sep=""))
}


###############
########
## This function saves the feature ranking to a file
##Input:
# feat: The features to save
# fname: the file name (including path) of the feature file
########
###############
saveFeatures <- function(feat,fname) {
  print(paste("Features saved to: ",fname,sep=""))
  write.csv(feat,fname, row.names = FALSE)
}


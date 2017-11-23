###### Set the File paths
ROOT_DIR <<-"C:\\Users\\MindSee\\Desktop\\physio9GAG\\For Paper(Final)"
SEPARATOR <<- "\\"

PHYS_FEAT_FILE <<- paste(ROOT_DIR,"finalFeatures_both_train.csv",sep=SEPARATOR)
AFFECT_FEAT_FILE <<- paste(ROOT_DIR,"cameraFinalFeatures_both_train.csv",sep=SEPARATOR)
#######

####Set random seed
set.seed(7)

####Source auxiliary functions
source (paste(ROOT_DIR,"FeatSelection_Functions.R",sep=SEPARATOR))
source (paste(ROOT_DIR,"InputOutput_Functions.R",sep=SEPARATOR))

#wrapper to generate and save rank
generateRank <- function(feat,FEAT_RANGE,path) {
  # Generate ranking
  rank <- generateTableAcross(feat,FEAT_RANGE)
  N_orig <- length(rank)
  
  # Remove colinear features with higher correlation than 0.75 from rank
  rank <- removeColinearFeaturesFromRank(rank,feat,FEAT_RANGE,0.75,"meanCor")
  N_curr <- length(rank)
  
  #Correct p-value with the current number of features
  rank <- rank/N_orig*N_curr

  # Save Ranking
  saveRank(rank,path)
}


# #OLD Version, with the automatic ColinearRemoval
# #wrapper to generate and save rank
# generateRank <- function(feat,FEAT_RANGE,path) {
#   # Remove colinear features with higher correlation than 0.75
#   data <- removeColinearFeatures(feat,FEAT_RANGE,0.75)
# 
#   # Generate ranking
#   rank <- generateTableAcross(data$data,data$FEAT_RANGE)#,"Ranked Extended features across")
#   # Save Ranking
#   saveRank(rank,path)
# }

#Index to the first feature The first feature (not including "part","class","gender" and "duration")
FEAT_INI <- 5

####ALL PHYSIOLOGY
CURRENT_DIR <<- paste(ROOT_DIR,"AllPhysiology","",sep=SEPARATOR)

#Read Physiological features
phys_feat <- readPhysData(PHYS_FEAT_FILE)
FEAT_RANGE <- FEAT_INI:ncol(phys_feat)

generateRank(phys_feat,FEAT_RANGE,CURRENT_DIR)


###INDIVIDUAL PHYSIOLOGY
##EDA
signal <- "EDA"
CURRENT_DIR <<- paste(ROOT_DIR,"IndividualPhysiology",signal,"",sep=SEPARATOR)

#Select features only for the given signal
d_signal <- trimDataBySignal(phys_feat,FEAT_INI,signal)

generateRank(d_signal$data,d_signal$FEAT_RANGE,CURRENT_DIR)


##ECG
signal <- "ECG"
CURRENT_DIR <<- paste(ROOT_DIR,"IndividualPhysiology",signal,"",sep=SEPARATOR)

#Select features only for the given signal
d_signal <- trimDataBySignal(phys_feat,FEAT_INI,signal)

generateRank(d_signal$data,d_signal$FEAT_RANGE,CURRENT_DIR)


#EEG
signal <- "EEG"
CURRENT_DIR <<- paste(ROOT_DIR,"IndividualPhysiology",signal,"",sep=SEPARATOR)

#Select features only for the given signal
d_signal <- trimDataBySignal(phys_feat,FEAT_INI,signal)

generateRank(d_signal$data,d_signal$FEAT_RANGE,CURRENT_DIR)




####AFFECTIVA

#Read Affectva features
affect_feat <- readAffectivaData(AFFECT_FEAT_FILE,PHYS_FEAT_FILE)
FEAT_RANGE <- FEAT_INI:ncol(affect_feat)

##Both affectiva and physiology
signal <- "Both"
CURRENT_DIR <<- paste(ROOT_DIR,"PhysiologyVsAffectiva",signal,"",sep=SEPARATOR)

generateRank(affect_feat,FEAT_RANGE,CURRENT_DIR)


##Affectiva alone
signal <- "Affectiva"
CURRENT_DIR <<- paste(ROOT_DIR,"PhysiologyVsAffectiva",signal,"",sep=SEPARATOR)

#Select features only for the given signal
d_signal <- trimDataBySignal(affect_feat,FEAT_INI,signal)

generateRank(d_signal$data,d_signal$FEAT_RANGE,CURRENT_DIR)

##Physiology alone
signal <- "Physiology"
CURRENT_DIR <<- paste(ROOT_DIR,"PhysiologyVsAffectiva",signal,"",sep=SEPARATOR)

#Select features only for the given signal
d_signal <- trimDataBySignal(affect_feat,FEAT_INI,signal)

generateRank(d_signal$data,d_signal$FEAT_RANGE,CURRENT_DIR)

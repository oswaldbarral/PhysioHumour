###### Set the File paths
ROOT_DIR <<-"C:\\Users\\MindSee\\Desktop\\physio9GAG\\For Paper(Final)"
SEPARATOR <<- "\\"

PHYS_FEAT_FILE <<- paste(ROOT_DIR,"finalFeatures_both_test.csv",sep=SEPARATOR)
AFFECT_FEAT_FILE <<- paste(ROOT_DIR,"cameraFinalFeatures_both_test.csv",sep=SEPARATOR)
#######

####Set random seed
set.seed(123)

####Source auxiliary functions
source (paste(ROOT_DIR,"Prediction_Functions.R",sep=SEPARATOR))
source (paste(ROOT_DIR,"FeatSelection_Functions.R",sep=SEPARATOR))
source (paste(ROOT_DIR,"InputOutput_Functions.R",sep=SEPARATOR))



##wrapper to generate and save prediction
generatePredictions <- function(data,Mode,pred_algo,kCross) {
  set.seed(134)
  
  PATH_TO_FOLDER <- paste(CURRENT_DIR,Mode,"",sep=SEPARATOR)
  
  p <- generatePredictionTable(data$data,data$FEAT_RANGE,pred_algo,Mode,kCross)
  if(kCross == -1) savePredictionTable(p,PATH_TO_FOLDER,pred_algo)
  else savePredictionTable(p,PATH_TO_FOLDER,pred_algo,kCross)
  
  return(p)
}

####MAIN
#The prediction algorithm to be used ("rf" for Random forest, "svm" for support vector machines, etc.)
PRED_ALGO <- "rf"
#Index to the first feature The first feature (not including "part","class","gender" and "duration")
FEAT_INI <- 5


# #########################
# ######################### Physiology Features Only
# 
# CURRENT_DIR <<- paste(ROOT_DIR,"AllPhysiology",sep=SEPARATOR)
# RANK_FILE <- paste(CURRENT_DIR,"rank.csv",sep=SEPARATOR)
# 
# #Read Physiological features
# data <- readPhysData(PHYS_FEAT_FILE)
# 
# ##FOR DEBUG PURPOSES! create a dataset with only first 30 strips of the first 2 participants
# #data1 <- data[data$Part == unique(data$Part)[1],]
# #data1 <- data1[1:30,]
# #data2 <- data[data$Part == unique(data$Part)[2],]
# #data2 <- data2[1:30,]
# #data <- rbind(data1,data2)
# 
# 
# 
# #Trim the dataset to select only Top Ranked features. selectedFeat stores the names of the top features (p.value smaller than 0.05)
# selectedFeat <- selectFeaturesFromRank(RANK_FILE,0.05)
# 
# daux <- trimDataByFeatures(data,FEAT_INI,selectedFeat)
# 
# ##Run across participants
#  generatePredictions(daux,"across",PRED_ALGO,-1)
# 
# ##Run within participants
# generatePredictions(daux,"within",PRED_ALGO,-1)
# 
# ##Run within participants, Online mode, increasing amount of samples
# generatePredictions(daux,"withinOnline",PRED_ALGO,3)
# generatePredictions(daux,"withinOnline",PRED_ALGO,4)
# generatePredictions(daux,"withinOnline",PRED_ALGO,5)
# generatePredictions(daux,"withinOnline",PRED_ALGO,10)
# generatePredictions(daux,"withinOnline",PRED_ALGO,15)
# generatePredictions(daux,"withinOnline",PRED_ALGO,20)
# generatePredictions(daux,"withinOnline",PRED_ALGO,25)
# generatePredictions(daux,"withinOnline",PRED_ALGO,30)
# generatePredictions(daux,"withinOnline",PRED_ALGO,35)
# generatePredictions(daux,"withinOnline",PRED_ALGO,40)
# 
# #########################
# #########################



# ######################### Individual Physiology
# #########################
# 
# ##Read Physiological features
# data <- readPhysData(PHYS_FEAT_FILE)
# 
# # ####For testing purposes
# #data1 <- data[data$Part == unique(data$Part)[1],]
# #data1 <- data1[1:10,]
# #data2 <- data[data$Part == unique(data$Part)[2],]
# #data2 <- data2[1:10,]
# #data <- rbind(data1,data2)
# 
# 
# ####EDA
# CURRENT_DIR <<- paste(ROOT_DIR,"IndividualPhysiology","EDA",sep=SEPARATOR)
# 
# #Trim the dataset to select only Top Ranked features
# RANK_FILE <- paste(CURRENT_DIR,"rank.csv",sep=SEPARATOR)
# selectedFeat <- selectFeaturesFromRank(RANK_FILE,0.05)
# daux <- trimDataByFeatures(data,FEAT_INI,selectedFeat)
# 
# ##Run across participants
# generatePredictions(daux,"across",PRED_ALGO,-1)
# ##Run within participants
# generatePredictions(daux,"within",PRED_ALGO,-1)
# 
# ####EEG
# CURRENT_DIR <<- paste(ROOT_DIR,"IndividualPhysiology","EEG",sep=SEPARATOR)
# 
# #Trim the dataset to select only Top Ranked features
# RANK_FILE <- paste(CURRENT_DIR,"rank.csv",sep=SEPARATOR)
# selectedFeat <- selectFeaturesFromRank(RANK_FILE,0.05)
# daux <- trimDataByFeatures(data,FEAT_INI,selectedFeat)
# 
# ##Run across participants
# generatePredictions(daux,"across",PRED_ALGO,-1)
# ##Run within participants
# generatePredictions(daux,"within",PRED_ALGO,-1)
# 
# 
# ####ECG
# CURRENT_DIR <<- paste(ROOT_DIR,"IndividualPhysiology","ECG",sep=SEPARATOR)
# 
# #Trim the dataset to select only Top Ranked features
# RANK_FILE <- paste(CURRENT_DIR,"rank.csv",sep=SEPARATOR)
# selectedFeat <- selectFeaturesFromRank(RANK_FILE,0.05)
# daux <- trimDataByFeatures(data,FEAT_INI,selectedFeat)
# 
# ##Run across participants
# generatePredictions(daux,"across",PRED_ALGO,-1)
# ##Run within participants
# generatePredictions(daux,"within",PRED_ALGO,-1)
# 
# #########################
# #########################



######################### Benchmark with Affectiva Features
#########################

##Read Affectiva and Physiology features
data <- readAffectivaData(AFFECT_FEAT_FILE,PHYS_FEAT_FILE)

# ####For testing purposes
#data1 <- data[data$Part == unique(data$Part)[1],]
#data1 <- data1[1:10,]
#data2 <- data[data$Part == unique(data$Part)[2],]
#data2 <- data2[1:10,]
#data <- rbind(data1,data2)

####Physiology alone
CURRENT_DIR <<- paste(ROOT_DIR,"PhysiologyVsAffectiva","Physiology",sep=SEPARATOR)

#Trim the dataset to select only Top Ranked features
RANK_FILE <- paste(CURRENT_DIR,"rank.csv",sep=SEPARATOR)
selectedFeat <- selectFeaturesFromRank(RANK_FILE,0.05)
daux <- trimDataByFeatures(data,FEAT_INI,selectedFeat)

##Run across participants
generatePredictions(daux,"across",PRED_ALGO,-1)
##Run within participants
generatePredictions(daux,"within",PRED_ALGO,-1)

####Affectiva alone
CURRENT_DIR <<- paste(ROOT_DIR,"PhysiologyVsAffectiva","Affectiva",sep=SEPARATOR)

#Trim the dataset to select only Top Ranked features
RANK_FILE <- paste(CURRENT_DIR,"rank.csv",sep=SEPARATOR)
selectedFeat <- selectFeaturesFromRank(RANK_FILE,0.05)
daux <- trimDataByFeatures(data,FEAT_INI,selectedFeat)

##Run across participants
generatePredictions(daux,"across",PRED_ALGO,-1)
##Run within participants
generatePredictions(daux,"within",PRED_ALGO,-1)

####Both physiology and affectiva
CURRENT_DIR <<- paste(ROOT_DIR,"PhysiologyVsAffectiva","Both",sep=SEPARATOR)

#Trim the dataset to select only Top Ranked features
RANK_FILE <- paste(CURRENT_DIR,"rank.csv",sep=SEPARATOR)
selectedFeat <- selectFeaturesFromRank(RANK_FILE,0.05)
daux <- trimDataByFeatures(data,FEAT_INI,selectedFeat)

##Run across participants
generatePredictions(daux,"across",PRED_ALGO,-1)
##Run within participants
generatePredictions(daux,"within",PRED_ALGO,-1)

#########################
#########################





########################################### OLD STUFF TO BE DELETED
###########################################
###########################################
###########################################
###########################################
###########################################
###########################################
###########################################
###########################################
###########################################

##
# FEAT_FILE <<- "/Users/barralme/Documents/MindSee/Physio9GAG/Data/Prediction/newfeatures_train_NORM.csv"
# RANK_FILE <<- "/Users/barralme/Documents/MindSee/Physio9GAG/Data/Prediction/Scripts/For Paper(Final)/AllPhysiology/rank.csv"
# CURRENT_DIR <<-"/Users/barralme/Documents/MindSee/Physio9GAG/Data/Prediction/Scripts/For Paper(Final)/AllPhysiology"
# SEPARATOR <<- "/"
# 
# 
# 
# 
# 
# ####MAIN
# main <- function(kCross) {
#   set.seed(134)
#   PREDICT_ALGO <<- "rf"
#   Mode <<- "withinOnline"
#   # kCross <- 2
#   
#   feat <- read.csv(FEAT_FILE)
#   feat[is.na(feat)] <- 0
#   
#   feat <- feat[feat$Part != 18,]
#   
#   #get rid of WTF class
#   feat <- feat[feat$Class != 2,]
#   feat$Class <- factor(feat$Class,labels=c('notfun','fun'))
#   
#   FEAT_INI <<- 5
#   FEAT_RANGE <<- FEAT_INI:ncol(feat)
#   
#   
#   # Read feature rank
#   feat_rank <- read.csv(RANK_FILE,stringsAsFactors = FALSE)
#   
#   TOP_FEAT <- sum(feat_rank[,2] < 0.05)
#   print(TOP_FEAT)
#   feat_rank <- feat_rank[,1]
#   feat <- feat[c(names(feat[1:FEAT_INI-1]),feat_rank[1:TOP_FEAT])]
#   FEAT_RANGE <<- FEAT_INI:ncol(feat)
#   p <- generatePredictionTable(feat,Mode,"",kCross)
#   
#   
#   if (Mode == "within") {
#     write.csv(p$predict_table,paste(CURRENT_DIR,SEPARATOR,Mode,SEPARATOR,"predictionTable_",TOP_FEAT,"_K_",kCross,".csv",sep=""),row.names=F)
#     colnames(p$varImp) <- feat_rank[1:TOP_FEAT]
#     rownames(p$varImp) <- unique(feat$Part)
#     write.csv(p$varImp,paste(CURRENT_DIR,SEPARATOR,Mode,SEPARATOR,"varImp","_K_",kCross,".csv",sep=""))
#     write.csv(p$train_ratio,paste(CURRENT_DIR,SEPARATOR,Mode,SEPARATOR,"TrainRatio","_K_",kCross,".csv",sep=""),row.names=F)
#     write.csv(p$test_ratio,paste(CURRENT_DIR,SEPARATOR,Mode,SEPARATOR,"TestRatio","_K_",kCross,".csv",sep=""),row.names=F)
#   } else if (Mode == "withinOnline") {
#     write.csv(p$predict_table,paste(CURRENT_DIR,SEPARATOR,"within",SEPARATOR,"predictionTable_",TOP_FEAT,"_Online_",kCross,".csv",sep=""),row.names=F)
#     colnames(p$varImp) <- feat_rank[1:TOP_FEAT]
#     rownames(p$varImp) <- unique(feat$Part)
#     write.csv(p$varImp,paste(CURRENT_DIR,SEPARATOR,"within",SEPARATOR,"Online",SEPARATOR,"varImp","_Online_",kCross,".csv",sep=""))
#     write.csv(p$train_ratio,paste(CURRENT_DIR,SEPARATOR,"within",SEPARATOR,"Online",SEPARATOR,"TrainRatio","_Online_",kCross,".csv",sep=""),row.names=F)
#     write.csv(p$test_ratio,paste(CURRENT_DIR,SEPARATOR,"within",SEPARATOR,"Online",SEPARATOR,"TestRatio","_Online_",kCross,".csv",sep=""),row.names=F)
#   }
#   else{
#     write.csv(p$predict_table,paste(CURRENT_DIR,SEPARATOR,Mode,SEPARATOR,"predictionTable_",TOP_FEAT,".csv",sep=""),row.names=F)
#     colnames(p$varImp) <- feat_rank[1:TOP_FEAT]
#     rownames(p$varImp) <- unique(feat$Part)
#     write.csv(p$varImp,paste(CURRENT_DIR,SEPARATOR,Mode,SEPARATOR,"varImp.csv",sep=""))
#     colnames(p$train_ratio) <- unique(feat$Part)
#     write.csv(t(p$train_ratio),paste(CURRENT_DIR,SEPARATOR,Mode,SEPARATOR,"TrainRatio.csv",sep=""),row.names=F)
#     colnames(p$test_ratio) <- unique(feat$Part)
#     write.csv(t(p$test_ratio),paste(CURRENT_DIR,SEPARATOR,Mode,SEPARATOR,"TestRatio.csv",sep=""),row.names=F)
#   }
#   
# }

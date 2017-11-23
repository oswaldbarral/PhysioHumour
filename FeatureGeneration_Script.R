
###### Set the File paths
ROOT_DIR <<-"C:\\Users\\MindSee\\Desktop\\physio9GAG\\For Paper(Final)"
RAW_DATA_DIR <<- "C:\\Users\\MindSee\\Desktop\\physio9GAG\\Logs"
AFFECT_DATA_DIR <<- "C:\\Users\\MindSee\\Desktop\\physio9GAG\\Logs\\CameraFeatures"
SEPARATOR <<- "\\"
#######


####Source auxiliary functions
source (paste(ROOT_DIR,"peak_detector.R",sep=SEPARATOR))
source (paste(ROOT_DIR,"InputOutput_Functions.R",sep=SEPARATOR))
source (paste(ROOT_DIR,"FeatureGeneration_Functions.R",sep=SEPARATOR))


####Generate Physiology features
#participants with valid physiologicala data (P06,P18,P20 not valid data)
participants_phys <- c("P01","P02","P03","P04","P05","P07","P08","P09","P10","P11","P12","P13","P14","P15","P16","P17","P19","P21","P22","P23","P24","P25")

# #########Generate feature table trial-based centered
# feat_phys <- generateFeatures (participants_phys, "Physiology", "TRIAL",RAW_DATA_DIR,SEPARATOR)
# 
# #Split into train and test sets
# feats <- splitFeatures(feat_phys)
# 
# #Store the feature tables
# saveFeatures(feats$Train,paste(ROOT_DIR,"finalFeatures_train.csv",sep=SEPARATOR))
# saveFeatures(feats$Test,paste(ROOT_DIR,"finalFeatures_test.csv",sep=SEPARATOR))
# ###
# 
# #########Generate feature table participant-based centered
# feat_phys <- generateFeatures (participants_phys, "Physiology", "PART",RAW_DATA_DIR,SEPARATOR)
# 
# #Split into train and test sets
# feats <- splitFeatures(feat_phys)
# 
# #Store the feature tables
# saveFeatures(feats$Train,paste(ROOT_DIR,"finalFeatures_pc_train.csv",sep=SEPARATOR))
# saveFeatures(feats$Test,paste(ROOT_DIR,"finalFeatures_pc_test.csv",sep=SEPARATOR))
# #############
# 
# 
# #########Generate feature table both trial and participant-based centered
# feat_phys <- generateFeatures (participants_phys, "Physiology", "BOTH",RAW_DATA_DIR,SEPARATOR)
# 
# #Split into train and test sets
# feats <- splitFeatures(feat_phys)
# 
# #Store the feature tables
# saveFeatures(feats$Train,paste(ROOT_DIR,"finalFeatures_both_train.csv",sep=SEPARATOR))
# saveFeatures(feats$Test,paste(ROOT_DIR,"finalFeatures_both_test.csv",sep=SEPARATOR))
# #############

#########Generate feature table participant-based centered, including additional trial-centering for EDA
feat_phys <- generateFeatures (participants_phys, "Physiology", "MIXED",RAW_DATA_DIR,SEPARATOR)

#Split into train and test sets
feats <- splitFeatures(feat_phys)

#Store the feature tables
saveFeatures(feats$Train,paste(ROOT_DIR,"finalFeatures_mixed_train.csv",sep=SEPARATOR))
saveFeatures(feats$Test,paste(ROOT_DIR,"finalFeatures_mixed_test.csv",sep=SEPARATOR))
#############


####Generate Affectiva features

#Participants with valid affectiva data. P01 data is missing. P09,P12,P21 rejected as Affectiva software crashed. Additionally P06,P18,P20 not valid data
participants_aff <<- c("P02","P03","P04","P05","P07","P08","P10","P11","P13","P14","P15","P16","P17","P19","P22","P23","P24","P25")
# 
# #########Generate feature table trial-based centered
# aff_phys <- generateFeatures (participants_aff, "Affectiva", "TRIAL", RAW_DATA_DIR,SEPARATOR,AFFECT_DATA_DIR)
# 
# #Split into train and test sets
# feats <- splitFeatures(aff_phys)
# 
# #Store the feature tables
# saveFeatures(feats$Train,paste(ROOT_DIR,"cameraFinalFeatures_train.csv",sep=SEPARATOR))
# saveFeatures(feats$Test,paste(ROOT_DIR,"cameraFinalFeatures_test.csv",sep=SEPARATOR))
# ###
# 
# #########Generate feature table participant-based centered
# aff_phys <- generateFeatures (participants_aff, "Affectiva", "PART", RAW_DATA_DIR,SEPARATOR,AFFECT_DATA_DIR)
# 
# #Split into train and test sets
# feats <- splitFeatures(aff_phys)
# 
# #Store the feature tables
# saveFeatures(feats$Train,paste(ROOT_DIR,"cameraFinalFeatures_pc_train.csv",sep=SEPARATOR))
# saveFeatures(feats$Test,paste(ROOT_DIR,"cameraFinalFeatures_pc_test.csv",sep=SEPARATOR))
# ###########
# 
# #########Generate feature table both trial and participant-based centered
# aff_phys <- generateFeatures (participants_aff, "Affectiva", "BOTH", RAW_DATA_DIR,SEPARATOR,AFFECT_DATA_DIR)
# 
# #Split into train and test sets
# feats <- splitFeatures(aff_phys)
# 
# #Store the feature tables
# saveFeatures(feats$Train,paste(ROOT_DIR,"cameraFinalFeatures_both_train.csv",sep=SEPARATOR))
# saveFeatures(feats$Test,paste(ROOT_DIR,"cameraFinalFeatures_both_test.csv",sep=SEPARATOR))
# ###########



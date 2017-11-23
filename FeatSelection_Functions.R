library(xtable)
library(mlbench)
library(caret)




###############
########
## This function generates the feature ranking across participants, based on wilcoxon-test p.value 
##Input:
# data: the dataset
# FEAT_RANGE: the column numbers of the features in the dataset
# capt: optional, the caption of the latex-format table
##Output:
# -The fearture ranking: a sorted named vecttor of bonferroni-corrected p.values (decreasing order)
# -In case capt is provided, the table is printed to console in latex-format
########
###############
generateTableAcross <- function(data,FEAT_RANGE,capt) {
	s <- lapply(data[,FEAT_RANGE], function(x){
		aux <- wilcox.test(x[data$Class == 'notfun'],x[data$Class == 'fun'])$p.value
		names(aux) <- NULL
		aux
	})
	s1 <- unlist(s)
	
	#In case we have a caption, print the table in latex-format
	if (!missing(capt)) {
	  tab <- data.frame("p.value" = s1)
	  row.names(tab) <- names(s1)
	  print (xtable(tab[order(-tab),,drop=FALSE],caption = capt))
	}
	
	rank <- sort(s1)
	
	#bonferroni correction of p.values
	rank <- rank*length(FEAT_RANGE)
	return (rank)
}

###############
########
## This function removes the colinear features from the given rank, by iteratively rejecting the 
## feature with highest p-value from the most correlated pair of features within the dataset
##Input:
# rank: the features rank
# feat: the dataset that might contain colinear features (which the rank is based on)
# FEAT_RANGE: the column numbers of the features in the dataset
# cut_off: features with higher correlation coefficient than cut_off will be excluded
# MODE: 
# -pValue: from each pair, the feature with highest p.value is selected to be rejected
# -meanCor: from each pair, the feature with highest mean correlation is selected to be rejected
##Output:
# -the new rank, without the colinear features
########
###############
removeColinearFeaturesFromRank <- function (rank,feat,FEAT_RANGE,cut_off,MODE) {
  feat_ini<- FEAT_RANGE[1]
  deleted_feat <- "Deleted features: "
  while (TRUE) {
    # calculate correlation matrix
    mat <- cor(feat[,FEAT_RANGE],use="pairwise.complete.obs")
    mat[is.na(mat)] <- 0
    #mask the identity with 0
    mat[mat == 1] <- 0
    #find the index of max correlation, higher than cut_off
    ind <- which(abs(mat) == max(abs(mat)) & abs(mat) > cut_off, arr.ind = TRUE)
    if (length(ind) == 0) {
      #If there is no pair of features with correlation > cut_off, break the loop
      break
    } 
    else {
      if (MODE == "pValue") {
        #get the feature name to remove: the one with highest p.value, from the highest correlated pair of features
        to_remove <- names(which.max(c(rank[which(names(rank) == row.names(ind)[1])],rank[which(names(rank) == row.names(ind)[2])])))
      }
      else if (MODE == "meanCor") {
        #get the feature name to remove: the one with highest absolute mean correlation
        if (mean(abs(mat[row.names(ind)[1],])) > mean(abs(mat[row.names(ind)[2],]))) to_remove <- row.names(ind)[1]
        else to_remove <- row.names(ind)[2]
        # hc <- findCorrelation(mat, cutoff=cut_off,names=TRUE)
        # if (row.names(ind)[1] %in% hc) to_remove <- row.names(ind)[1]
        # else if (row.names(ind)[2] %in% hc) to_remove <- row.names(ind)[2]
        # else print("Feature Name not found in the Highly correlated. Please revise the function removeColinearFeaturesFromRank!")
      }
      #remove colinear feature from rank
      rank <- rank[-which(names(rank) == to_remove)]
      #remove colinear feature from dataset
      feat <- feat[,-which(names(feat) == to_remove)]
      #update feature range
      FEAT_RANGE <- feat_ini:ncol(feat)
      
      #save the name of the deleted feature for later print
      deleted_feat <- c(deleted_feat,to_remove)
    }
  }
  print(paste(deleted_feat,collapse=" , "))
  return (rank)
}









###############
########
## This function removes the colinear features of the dataset
##Input:
# feat: the dataset that might contain colinear features
# FEAT_RANGE: the column numbers of the features in the dataset
# cut_off: features with higher correlation coefficient than cut_off will be excluded
##Output:
# data: the dataset without colinear features
# FEAT_RANGE: a vector containing the feature columns
########
###############
removeColinearFeatures <- function(feat, FEAT_RANGE, cut_off) {
  feat_ini<- FEAT_RANGE[1]
  # calculate correlation matrix
  correlationMatrix <- cor(feat[,FEAT_RANGE],use="pairwise.complete.obs")

  # find attributes that are highly corrected (ideally >0.75)
  correlationMatrix[is.na(correlationMatrix)] <- 0
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=cut_off,names=FALSE,exact = FALSE)
  #add offset of feature_ini
  to_remove <- highlyCorrelated+(feat_ini-1)
  # print highly correlated attributes
  print(paste(c("Deleted features: ",names(feat)[to_remove]),collapse=" , "))
  
  #remove highly colinear features
  feat <- feat[,-to_remove]
  
  FEAT_RANGE <- feat_ini:ncol(feat)
  return(list(data = feat, FEAT_RANGE = FEAT_RANGE))
}




###############
########
# ## This function selects the top features that have p.value smaller than the cutoff criteria
##Input:
# rank_fname: the path to the ranked features file
# cutoff: the cutoff at which the top features are selected 
##Output:
#  A vector containing the names of the selected features
########
###############
selectFeaturesFromRank <- function(rank_fname,cutoff) {
  feat_rank <- read.csv(rank_fname,stringsAsFactors = FALSE)
  top_feat <- sum(feat_rank[,2] < cutoff)
  print(top_feat)
  
  return(feat_rank[1:top_feat,1])
}


###############
########
# ## This function trims the dataset based on the signal type
##Input:
# feat: the dataset to be trimmed
# feat_ini: the column number where the features start in the dataset
# SIGNAL: the signal for which features are kept {EDA,ECG,EEG,Affectiva,Physiology}
##Output:
# data: the trimmed dataset containing only features for the given signal
# FEAT_RANGE: a vector containing the feature columns
########
###############
trimDataBySignal <- function(feat,feat_ini,SIGNAL) {
  if (SIGNAL =="EDA"){
    FEAT_RANGE <- c(grep("Eda",colnames(feat)),grep("scr",colnames(feat)))
  } else if (SIGNAL =="ECG") {
    FEAT_RANGE <- grep("EKG",colnames(feat))
    
  } else if (SIGNAL =="EEG") {
    FEAT_RANGE <- c(grep("Alpha",colnames(feat)),grep("Beta",colnames(feat)),grep("Gamma",colnames(feat)),grep("Theta",colnames(feat)),grep("Engage",colnames(feat)))
  } else if (SIGNAL =="Affectiva") {
    #We know that the first affectiva feature is related to smile, and the last is the last of the feature set
    FEAT_RANGE <- grep("smile",colnames(feat))[1]:ncol(feat)
  } else if (SIGNAL =="Physiology") {
    #We know that the first physiology feature is in feat_ini, and the last is the last related to Engage
    FEAT_RANGE <- feat_ini:tail(grep("Engage",colnames(feat)),1)
  }
  feat <- feat[,c(1:feat_ini-1,FEAT_RANGE)]
  FEAT_RANGE <- feat_ini:ncol(feat)
  return (list(data=feat,FEAT_RANGE=FEAT_RANGE))
}



###############
########
# ## This function trims the dataset based on a subset of features
##Input:
# feat: the dataset to be trimmed
# FEAT_INI: the column number where the features start in the dataset
# selectedFeat: a vector containing the names of the features to keep in the dataset
##Output:
# data: the trimmed dataset
# FEAT_RANGE: a vector containing the feature columns
########
###############
trimDataByFeatures <- function (feat,feat_ini,selectedFeat) {
  d <- feat[c(names(feat)[1:(feat_ini-1)],selectedFeat)]
  feat_range <- feat_ini:ncol(d)
  return(list(data = d,FEAT_RANGE = feat_range))
}

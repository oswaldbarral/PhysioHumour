library(caret)
library(MASS)
library (pROC)
library(pROC)
library(psych)


###############
########
##This function generates the predictions on the given Dataset.
##
##Input:
# data: Dataset
# FEAT_RANGE: The columns from the dataset that will be used as features
# pred_algo: the algorithm used for prediction
# mode: 
#   -across: Performs leave-out-participant-out cross-validation
#   -within:
#     -kCross = -1: performs leave-one-strip-out cross-validation
#     -kCross > -1: performs k-validation (k = Kcross)
#   -withinOnline: trains using the first N samples that have at least kCross samples for each class, predicts the rest
##Output:
# prediction: The prediction table table
# validCases: Index for valid trials (not NA)
# Class: True classes
# ratioTrain: a vector containing the class ratio in the training set, for each iteration
# ratioTest: a vector containing the class ratio in the test set, for each iteration
# varImp: the feature weights of the features (scaled between 0 and 100)
#   -mode = across: a matrix [NPart, NFeat] in which each row represents the feature weights of the features used to predict a single participant
#   -mode = within: a vector containing the averaged feature weights over all cross-validation models for the given data
#   -mode = withinOnline: a vector containing the feature weights for the trained model using the given data
########
###############
predictPhys <- function(data,FEAT_RANGE,pred_algo,mode,kCross) {
	
	#if rows with NA, print error and exit
  if (sum(is.na(data)) > 0) {
    print("The dataset contains missing values!!")
    return()
  }
	# valid_ind <- complete.cases(da[,FEAT_RANGE])
	# data <- da[valid_ind,]
	
	# pred <- c()
	pred <- rep(NA,nrow(data))
	ratio_train <- c()
	ratio_test <- c()
	importnce <- c()
	if (mode == "within") {
		#split for leave-one-strip-out cross validation
	  if (kCross == -1) K <- nrow(data)
	  else {
	    #split for K-crossvalidation
	    ind <- 1:nrow(data)
	    ind <- split(sample(ind), 1:kCross)
	    K <- kCross
	  }
	}
	else if (mode == "withinOnline") {
	  K <- 1
	}
	else if (mode == "across"){
		ind <- split(1:nrow(data), data$Part)
		K <- length(unique(data$Part))
		names(ind) <-NULL
	}
	
	for (i in 1:K) {
	  if (mode == "within" & kCross == -1) {
	    ind_test <- i
	    ind_train <- (1:K)[-i]
	  }
	  else if (mode == "withinOnline") {
	    #we train using the first N samples that have at least kCross samples for each class, predict the rest
	    N <- max(which(data$Class=="fun")[kCross],which(data$Class=="notfun")[kCross])
	    if(is.na(N)) return(list(prediction = data.frame("Participant"=data$Part,"Class"=data$Class,"Pred"=rep(NA,length(data$Part))), ratioTrain = NA, ratioTest = NA,varImp = rep(NA,length(FEAT_RANGE)))) #From at least one of the classes there are less than kCross samples
	    ind_train <- 1:N
	    ind_test <- (N+1):nrow(data)
	  }
	  else if (mode == "across"){
		  ind_test <- unlist (ind[i])
		  ind_train <- unlist(ind[-i])
	  }
	  ctrl <- trainControl(method = "repeatedcv", classProbs=TRUE, summaryFunction = twoClassSummary, number=5)
		fit <- tryCatch(train(x = data[ind_train, FEAT_RANGE,drop=F], y = data$Class[ind_train], method = pred_algo, metric ="ROC", trControl = ctrl), error = function(e) {
		  print(e)
		  NA
		  })
		if (!is.na(fit[1])){
		  if (pred_algo == "svmRadial") {
		    importnce <- rbind(importnce,varImp(fit)$importance$fun)
		  }
		  else if (pred_algo == "rf") {
		    if (mode == "within") {
		      importnce <- rbind(importnce,varImp(fit,scale=F)$importance$Overall) #we will scale after doing the mean
		    }
		    else {
		      importnce <- rbind(importnce,varImp(fit)$importance$Overall)
		    }
		  }
			predictedProbs <- tryCatch(predict(fit, newdata = data[ind_test,FEAT_RANGE,drop=F], type = "prob"), error = function(e) {
			  print(e)
			  matrix(data=NA, ncol=1, nrow=1)
			  })
			if (!is.na(predictedProbs[1,1])) {
				# pred <- c(pred,predictedProbs$fun)
			  pred[ind_test] <- predictedProbs$fun
				ratio_train <- c(ratio_train,sum(data[ind_train,]$Class == "fun")/length(data[ind_train,]$Class))
				ratio_test <- c(ratio_test,sum(data[ind_test,]$Class == "fun")/length(data[ind_test,]$Class))
			}
			else {
				print(2)
				ratio_train <- c(ratio_train,NA)
				ratio_test <- c(ratio_test,NA)
			}
		}	
		else {
			print(1)
			ratio_train <- c(ratio_train,NA)	
			ratio_test <- c(ratio_test,NA)			
		}
	}
	if (mode == "within") {
	  importnce <- apply(importnce,2,mean,na.rm=T)
	  #scale between 0 and 100 (to be comparable to across results)
	  importnce <- ((importnce-min(importnce))/(max(importnce)-min(importnce)))*100
	}
	
	# ##format the prediction table using the original amount of trials
	# Pred <- rep(NA,length(valid_ind))
	# Pred[valid_ind] <- pred
	# if (mode == "across") prediction_table <- data.frame("Participant"=data$Part,"Class"=data$Class,"Pred"=pred)
	# else prediction_table <- data.frame("Participant"=rep(data$Part[1],length(pred)),"Class"=data$Class,"Pred"=pred)
	
	prediction_table <- data.frame("Participant"=data$Part,"Class"=data$Class,"Pred"=pred)
	
	list(prediction = prediction_table, ratioTrain = ratio_train, ratioTest = ratio_test,varImp = importnce)

}


###############
########
## This function is a wrapper for "predictPhys" function. In case we predict within participants, the function iterates over participants.
## Refer to "predictPhys" above for further information on the arguments
########
###############
generatePredictionTable <- function(da,FEAT_RANGE,pred_algo,mode,kCross) {
  predict_table <- c()
  train_ratio <- c()
  test_ratio <- c()
  var_imp <- c()
	if (mode == "across") {
	  aux <- predictPhys(da,FEAT_RANGE,pred_algo,mode,-1)
	  predict_table <- rbind(predict_table,aux$prediction)
	  train_ratio <- rbind(train_ratio,aux$ratioTrain)
	  test_ratio <- rbind(test_ratio,aux$ratioTest)
	  var_imp <- aux$varImp
	  colnames(train_ratio) <- unique(da$Part)
	  colnames(test_ratio) <- unique(da$Part)
	}
	else if (mode == "within" | mode == "withinOnline") {
		for (part in unique(da$Part)) {
			aux <- predictPhys(da[da$Part == part,],FEAT_RANGE,pred_algo,mode,kCross)
			predict_table <- rbind(predict_table,aux$prediction)
			train_ratio <- c(train_ratio,aux$ratioTrain)
			test_ratio <- c(test_ratio,aux$ratioTest)
			var_imp <- rbind(var_imp,aux$varImp)
		}
	}
  colnames(var_imp) <- names(da[,FEAT_RANGE])
  rownames(var_imp) <- unique(da$Part)
	list ("predict_table" = predict_table, "train_ratio" = train_ratio, "test_ratio" = test_ratio, "varImp" = var_imp)
}	



###############
########
# ## This function computes classification performance measures
##Input
# x: class probabilities
# y: true class labels
# mode: type of performance measure
#   -roc: computes the area under the roc curve 
#   -acc: computes the accuracy
#   -prec: computes the precision
##Output:
# The classification performance measure
########
###############
acc <- function(x, y, mode) {
  if (mode == "roc") tryCatch(auc(y,x,levels=c('fun','notfun'),direction=">"),error = function(e) NA)
  else if (mode == "acc") sum (!xor( (x > 0.5), (y == 'fun') ), na.rm=TRUE) / (length(x)-sum(is.na(x)))
  else if (mode == "prec") tryCatch(sum(x > 0.5 & y =='fun',na.rm=T)/sum(x > 0.5,na.rm=T),error = function(e) NA)
}


###############
########
# ## This function plots the AUCs per participant
##Input
# pred_table: a prediction table
########
###############
plotAUC <- function(pred_table) {
	pred_aux <- split(pred_table,pred_table$Part)
	
	pred_auc <- lapply(pred_aux,function(x){acc(x$Pred,x$Class,"roc")})
	pred_auc <- do.call(rbind,pred_auc)
	#plot(density(unlist(pred_auc),na.rm=T))
	hist(unlist(pred_auc))
	abline (v=0.5)
}


 

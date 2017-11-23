library(ggplot2)
library(ggrepel)
library(gridExtra)




source("/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/Prediction_Functions.R")

addToDF <- function (dat,path,name,mod) {
  d <- read.csv(path)
  dacc <- unlist(lapply(split(d,d$Participant),function(x){acc(x$Pred,x$Class,mod)}))
  dacc <- c(dacc,mean(dacc,na.rm=T))
  names(dacc) <- paste("P",names(dacc),sep="")
  names(dacc)[length(dacc)] <- "mean"
  
  #add the overallROC
  dacc <- c(dacc,acc(d$Pred,d$Class,"roc"))
  names(dacc)[length(dacc)] <- "overall"
  
  dat$DF <- rbind(dat$DF,data.frame("Part"=names(dacc),"Source" = rep(name,length(dacc)),"Dat"=dacc,"Mode"=rep(mod,length(dacc))))
  dat$RAW <- rbind(dat$RAW,data.frame("Source"=rep(name,nrow(d)),"Mode"=rep(mod,nrow(d)),d))
  return (dat)
  
}

#generate barplots per source type
generateBarPlot <- function(dat) {
  dat$Part <- factor(dat$Part, levels = dat$Part) #Keep the order as it is (convert to ordered factor)
  p <- ggplot(data= dat,aes(x = Part,y=Dat,fill = interaction(Source,Mode)))
  p <- p+ geom_bar(stat="identity",position = "dodge")
  
  
  return (p)
}

#plots the boxplot of class probabilities for the two classes
boxProbs <- function(dat) {
  p <- ggplot(data=dat,aes(x=Class,y=Pred))
  p <- p+geom_boxplot()
  return (p)
}



#generates boxplots per source type

boxplots <- function (d2,ylabel,yl,xla){
  p <- ggplot(d2[d2$Part != "mean",], aes(x=Source, y=Dat, fill=Source)) + geom_boxplot() + guides(fill=FALSE) + 
    geom_hline(yintercept = 0.5,linetype="dashed")
  if (!is.null(yl)) p <- p + ylim(yl)
  if (!is.null(xla) )p <- p + labs(x = " ", y = ylabel)
  else p <- p + labs(x = NULL, y = ylabel)
  return (p)
  
}


#generates dotplots per source type
dotPlots <- function (d2,yli,xla){

  p <- ggplot(d2[d2$Part !="mean",], aes(x=Source, y=Dat, fill=Source, color = Source, group = Part)) 
  p <-  p + geom_point(size = 3)
  if (!is.null(xla) )p <- p + labs(x = xla,y =NULL)
  else p <- p + labs(x = NULL,y =NULL)#+ labs(x = "Participant code", y = "Classification accuracy") 
  p <- p+ geom_line(color="grey",alpha=0.7)
  # p<- p+geom_boxplot(aes(x=Type, y=Accuracy, alpha = 0.01, group =Type) )
  p <- p + geom_hline(yintercept = 0.5,linetype="dashed")
  p <- p+geom_text_repel(data= d2[d2$Part !="mean" & d2$Source == unique(d2$Source)[1],], aes(label=Part), nudge_x = -0.2,segment.alpha=0.7,segment.color ="grey",color="black")
  

  #add Mean info
  p <- p + geom_point(data=d2[d2$Part == "mean",], aes(x=Source,y=Dat),size = 20,alpha=0.5)
  p <- p + geom_line(data=d2[d2$Part == "mean",], aes(x=Source,y=Dat),size = 2,alpha=0.2,color = "black")
  
  p <- p+ guides(color=FALSE, fill =FALSE)
  
  if (!is.null(yli)) p <- p + ylim(yli)
  
  
  return(p)
}




#generates dotplots per source type with superimposed boxplot
dotPlotsSuperImp <- function (d2,yli,xla,yla,plotOverall=FALSE){
  #define plot
  p <- ggplot(d2[!(d2$Part %in% c("mean","overall")),], aes(x=Source, y=Dat, fill=Source, color = Source, group = Part)) 
  #if defined, set the y_axis breaks
  if (!is.null(yli)) {
    # p <- p + ylim(yli)
    p <- p + scale_y_continuous (limits=yli,breaks = round(seq(yli[1],yli[2], by = 0.1),1))
  }
  #plot dots
  p <-  p + geom_point(size = 3)
  #add connecting line
  p <- p+ geom_line(color="grey",alpha=0.4)
  #add horizontal line at 0.5 (indicating random)
  p <- p + geom_hline(yintercept = 0.5,linetype=3,size=0.7,color="black")
  #overlay boxplot
  p <- p + geom_boxplot(data = d2[!(d2$Part %in% c("mean","overall")),],aes(x=Source, y=Dat, fill = Source, color = Source, group = Source),alpha=0.2,width=0.3)
  #add smal point for means
  p <- p + geom_point(data=d2[d2$Part == "mean",], aes(x=Source,y=Dat),size = 2, alpha = 0.7, color="black",shape=20)
  #add connecting line for means
  p <- p + geom_line(data=d2[d2$Part == "mean",], aes(x=Source,y=Dat),alpha=0.8,color="black",linetype=2)
  if(plotOverall == TRUE) {
    #add smal point for overall
    p <- p + geom_point(data=d2[d2$Part == "overall",], aes(x=Source,y=Dat),size = 4, alpha = 0.7, color="black",shape=20)
    #add connecting line for overall
    p <- p + geom_line(data=d2[d2$Part == "overall",], aes(x=Source,y=Dat),alpha=0.8,size = 2, color="black",linetype=2)
  }
  #add participants labels
  p <- p+geom_text_repel(data= d2[!(d2$Part %in% c("mean","overall")) & d2$Source == unique(d2$Source)[1],], aes(label=Part), nudge_x = -0.4,segment.alpha=0.7,segment.color ="grey",color="black")
  p <- p+geom_text_repel(data= d2[!(d2$Part %in% c("mean","overall")) & d2$Source == tail(unique(d2$Source),1),], aes(label=Part), nudge_x = 0.4,segment.alpha=0.7,segment.color ="grey",color="black")
  #remove legend
  p <- p+ guides(color=FALSE, fill =FALSE)
  #add plot labels
  p <- p + labs(x = xla, y = yla)
  #turn background in balck and white
  p <- p + theme_bw()
  #remove grid lines
  p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

  #return plot
  return(p)
}





main <- function(){
############MAIN


###############AllPhys

# # Add sources to DataFrame for plotting
# dat <- list("DF"=c(),"RAW"=c())
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/across/predictionTable_rf_17.csv","Across","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/within/predictionTable_rf_17.csv","Within","roc")
# 
# xlab <- NULL
# ylab <- "Area Under the ROC Curve"
# p <- dotPlotsSuperImp(dat$DF,NULL,xlab,ylab)
# print(p)
# p <<- dotPlotsSuperImp(dat$DF,NULL,xlab,ylab,TRUE)
# # print(p)
# ###############
# 
# 
# ################IndividualPhys

###Across
##Add sources to DataFrame for plotting
dat <- list("DF"=c(),"RAW"=c())
dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/across/predictionTable_rf_17.csv","All","roc")
dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/EDA/across/predictionTable_rf_13.csv","EDA","roc")
dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/EEG/across/predictionTable_rf_4.csv","EEG","roc")
dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/ECG/across/predictionTable_rf_3.csv","ECG","roc")

ylim <- c(0.3,1)
xlab <- "Across participant prediction"
ylab <- "Area Under the ROC Curve"
p1 <- dotPlotsSuperImp(dat$DF,ylim,xlab,ylab)
p1 <<- dotPlotsSuperImp(dat$DF,ylim,xlab,ylab,TRUE)
# print(p1)
#add horizontal line at 0.777 (indicating video+phys)
p1 <- p1 + geom_hline(yintercept = 0.777,linetype=3,size=0.7,color="black")
#add horizontal line at 0.747 (indicating videos)
p1 <- p1 + geom_hline(yintercept = 0.747,linetype=3,size=0.7,color="black")

###Within
dat <- list("DF"=c(),"RAW"=c())
dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/within/predictionTable_rf_17.csv","All","roc")
dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/EDA/within/predictionTable_rf_13.csv","EDA","roc")
dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/EEG/within/predictionTable_rf_4.csv","EEG","roc")
dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/ECG/within/predictionTable_rf_3.csv","ECG","roc")

##Generate dotplots+boxplot superimposed
ylim <- c(0.3,1)
xlab <- "Within participant prediction"
ylab <- "Area Under the ROC Curve"
p2 <- dotPlotsSuperImp(dat$DF,ylim,xlab,ylab)
p2 <<- dotPlotsSuperImp(dat$DF,ylim,xlab,ylab,TRUE)
# print(p2)
#add horizontal line at 0.876 (indicating video+phys)
p2 <- p2 + geom_hline(yintercept = 0.876,linetype=3,size=0.7,color="black")
#add horizontal line at 0.875 (indicating video)
p2 <- p2 + geom_hline(yintercept = 0.875,linetype=3,size=0.7,color="black")
###############

p3 <- grid.arrange(p1, p2, ncol=2, widths = c(1,1))
print(p3)
# p3a <- grid.arrange(p1a, p2a, ncol=2, widths = c(1,1))
# print(p3a)

########################


################PhysVsAffectiva
# 
# ###Across
# ##Add sources to DataFrame for plotting
# dat <- list("DF"=c(),"RAW"=c())
# 
# #using the original prediction for physiology
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/across/predictionTable_rf_17.csv","Physiology","roc")
# #  #using the outcomes of physiology trained with subset of valid video-participants
# # dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Physiology/across/predictionTable_rf_9.csv","Physiology","roc")
# 
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Affectiva/across/predictionTable_rf_80.csv","Video","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Both/across/predictionTable_rf_81.csv","Both","roc")
# ylim <- c(0.47,1)
# xlab <- "Across participant prediction"
# ylab <- "Area Under the ROC Curve"
# p1 <- dotPlotsSuperImp(dat$DF,ylim,xlab,ylab)
# # p3 <<- dotPlotsSuperImp(dat$DF,ylim,xlab,ylab,TRUE)
# # print(p1)
# ###
# 
# ##Within
# dat <- list("DF"=c(),"RAW"=c())
# 
# #using the original prediction for physiology
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/within/predictionTable_rf_17.csv","Physiology","roc")
# #  #using the outcomes of physiology trained with subset of valid video-participants
# # dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Physiology/within/predictionTable_rf_9.csv","Physiology","roc")
# 
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Affectiva/within/predictionTable_rf_80.csv","Video","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Both/within/predictionTable_rf_81.csv","Both","roc")
# ylim <- c(0.47,1)
# xlab <- "Within participant prediction"
# ylab <- "Area Under the ROC Curve"
# p2 <- dotPlotsSuperImp(dat$DF,ylim,xlab,ylab)
# # p4 <<- dotPlotsSuperImp(dat$DF,ylim,xlab,ylab,TRUE)
# # print(p2)
# ###
# 
# p3 <- grid.arrange(p1, p2, ncol=2, widths = c(1,1))
# print(p3)
# # p3aa <<- grid.arrange(p1a, p2a, ncol=2, widths = c(1,1))
# # print(p3aa)
# ####################
# ####################


# ##############Allphys WithinOnline
# 
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/withinOnline/predictionTable_rf_17_3.csv","3","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/withinOnline/predictionTable_rf_17_4.csv","4","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/withinOnline/predictionTable_rf_17_5.csv","5","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/withinOnline/predictionTable_rf_17_10.csv","10","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/withinOnline/predictionTable_rf_17_15.csv","15","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/withinOnline/predictionTable_rf_17_20.csv","20","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/withinOnline/predictionTable_rf_17_25.csv","25","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/withinOnline/predictionTable_rf_17_30.csv","30","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/withinOnline/predictionTable_rf_17_35.csv","35","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/withinOnline/predictionTable_rf_17_40.csv","40","roc")
# 
# 
# d2 <- dat$DF
# p <- ggplot(d2[d2$Part != "mean",], aes(x=Source, y=Dat, fill=Part, group = Part))
# # p <-  p + geom_point(size = 3)
# p <- p + labs(x = NULL,y =NULL)+ labs(x = "Minimum number of datapoints for each class (N)", y = "Area Under the ROC Curve")
# p <- p+ geom_line(color="black",alpha=0.2)
# # p<- p+geom_boxplot(aes(x=Type, y=Accuracy, alpha = 0.01, group =Type) )
# p <- p + geom_hline(yintercept = 0.5,linetype="dashed")
# p <- p+geom_text_repel(data= d2[d2$Part != "mean" & d2$Source == unique(d2$Source)[1],], aes(label=Part), nudge_x = -0.2,segment.alpha=0.7,segment.color ="grey",color="black")
# 
# 
# #add Mean info
# # p <- p + geom_point(data=d2[d2$Part == "mean",], aes(x=Source,y=Dat),size = 20,alpha=0.5)
# p <- p + geom_line(data=d2[d2$Part == "mean",], aes(x=Source,y=Dat),size = 2,alpha=0.5,color = "black")
# 
# p <- p+ guides(color=FALSE, fill =FALSE)
# 
# print(p)

################


}
main()


# 
# ####Togenerate table with mean and medians accuracy
# addToTab <- function(t_a,path,so,se) {
#   dat <- read.csv(path)
#   dacc <- unlist(lapply(split(dat,dat$Participant),function(x){acc(x$Pred,x$Class,"roc")}))
#   dacc <- c(dacc,mean(dacc,na.rm=T))
#   t_a <- rbind(t_a,data.frame("Source"=so,"Setup"=se,"Mean"=mean(dacc,na.rm=T),"Median"=median(dacc,na.rm=T)))
#   return (t_a)
# }
# dat <- data.frame()
# dat <- addToTab(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/across/predictionTable_rf_17.csv","All Phys.","Across")
# dat <- addToTab(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/within/predictionTable_rf_17.csv","All Phys.","Within")
# dat <- addToTab(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/EDA/across/predictionTable_rf_13.csv","EDA","Across")
# dat <- addToTab(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/EEG/across/predictionTable_rf_4.csv","EEG","Across")
# dat <- addToTab(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/ECG/across/predictionTable_rf_3.csv","ECG","Across")
# dat <- addToTab(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/EDA/within/predictionTable_rf_13.csv","EDA","Within")
# dat <- addToTab(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/EEG/within/predictionTable_rf_4.csv","EEG","Within")
# dat <- addToTab(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/ECG/within/predictionTable_rf_3.csv","ECG","Within")
# dat <- addToTab(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Affectiva/across/predictionTable_rf_80.csv","Video","Across")
# dat <- addToTab(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Both/across/predictionTable_rf_81.csv","Phys. + Video","Across")
# dat <- addToTab(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Affectiva/within/predictionTable_rf_80.csv","Video","Within")
# dat <- addToTab(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Both/within/predictionTable_rf_81.csv","Phys. + Video","Within")
# 
# dat[with(dat, order(-Mean)), ]
# 
# 












######
######OLD STUF
#####

# Add sources to DataFrame for plotting
# dat <- list("DF"=c(),"RAW"=c())

#AllPhys
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/across/predictionTable_rf_17.csv","Across","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/within/predictionTable_rf_17.csv","Within","roc")

# ## One can even add other measures of performance, these will be plotted as separate Bars
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/across/predictionTable_rf_17.csv","AllphysAccross","acc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/within/predictionTable_rf_17.csv","AllphysWithin","acc")

# #IndividualPhys
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/EDA/across/predictionTable_rf_13.csv","EDA","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/EDA/within/predictionTable_rf_13.csv","EDA","roc")

# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/EEG/across/predictionTable_rf_4.csv","EEG","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/EEG/within/predictionTable_rf_4.csv","EEG","roc")

# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/ECG/across/predictionTable_rf_3.csv","ECG","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/IndividualPhysiology/ECG/within/predictionTable_rf_3.csv","ECG","roc")


#Phys vs Affect
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Both/across/predictionTable_rf_78.csv","BothAcross","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Affectiva/across/predictionTable_rf_73.csv","AffectAcross","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Physiology/across/predictionTable_rf_9.csv","PhysAcross","roc")

# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Both/within/predictionTable_rf_78.csv","BothWithin","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Affectiva/within/predictionTable_rf_73.csv","AffectWithin","roc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Physiology/within/predictionTable_rf_9.csv","PhysWithin","roc")

# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Both/across/predictionTable_rf_78.csv","BothAcross","acc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Affectiva/across/predictionTable_rf_73.csv","AffectAcross","acc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Physiology/across/predictionTable_rf_9.csv","PhysAcross","acc")
# 
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Both/within/predictionTable_rf_78.csv","BothWithin","acc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Affectiva/within/predictionTable_rf_73.csv","AffectWithin","acc")
# dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Physiology/within/predictionTable_rf_9.csv","PhysWithin","acc")






#
# #print the means
# print(dat$DF[dat$DF$Part=="mean",])
#
#
# #########Generate BarPlot
# pbar <- generateBarPlot(dat$DF)
# print(pbar)
#
# ###########


# ##########Generate dotplots+boxplot-type graphs
# 
# ylim <- c(0.3,0.95)
# xlab <- "Within participant predicition"
# 
# p1 <- boxplots(dat$DF,"Area Under the ROC Curve",ylim,xlab)
# 
# p2 <- dotPlots(dat$DF,ylim,xlab)
# 
# grid.arrange(p1, p2, ncol=2, widths = 1:2)
# 
# 


# d <- dcast(dat$DF,Part + Mode ~ Source, value.var=Dat)

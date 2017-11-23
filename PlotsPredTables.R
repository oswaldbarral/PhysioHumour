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

# Add sources to DataFrame for plotting
dat <- list("DF"=c(),"RAW"=c())
dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/across/predictionTable_rf_17.csv","Across","roc")
dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/within/predictionTable_rf_17.csv","Within","roc")

xlab <- NULL
ylab <- "Area Under the ROC Curve"
p <- dotPlotsSuperImp(dat$DF,NULL,xlab,ylab)
print(p)
p <<- dotPlotsSuperImp(dat$DF,NULL,xlab,ylab,TRUE)
# print(p)
###############


################IndividualPhys

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
p1 <- dotPlotsSuperImp(dat$DF,ylim,xlab,ylab,TRUE)
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
p2 <- dotPlotsSuperImp(dat$DF,ylim,xlab,ylab,TRUE)
# print(p2)
#add horizontal line at 0.876 (indicating video+phys)
p2 <- p2 + geom_hline(yintercept = 0.876,linetype=3,size=0.7,color="black")
#add horizontal line at 0.875 (indicating video)
p2 <- p2 + geom_hline(yintercept = 0.875,linetype=3,size=0.7,color="black")
###############

p3 <- grid.arrange(p1, p2, ncol=2, widths = c(1,1))
print(p3)


########################


################PhysVsAffectiva

###Across
##Add sources to DataFrame for plotting
dat <- list("DF"=c(),"RAW"=c())

#using the original prediction for physiology
dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/across/predictionTable_rf_17.csv","Physiology","roc")

dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Affectiva/across/predictionTable_rf_80.csv","Video","roc")
dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Both/across/predictionTable_rf_81.csv","Both","roc")
ylim <- c(0.47,1)
xlab <- "Across participant prediction"
ylab <- "Area Under the ROC Curve"
p1 <- dotPlotsSuperImp(dat$DF,ylim,xlab,ylab)
###

##Within
dat <- list("DF"=c(),"RAW"=c())

#using the original prediction for physiology
dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/AllPhysiology/within/predictionTable_rf_17.csv","Physiology","roc")

dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Affectiva/within/predictionTable_rf_80.csv","Video","roc")
dat <- addToDF(dat,"/Users/barralme/Documents/MindSee/Physio9GAG/Final-Results-Test-Data/PhysiologyVsAffectiva/Both/within/predictionTable_rf_81.csv","Both","roc")
ylim <- c(0.47,1)
xlab <- "Within participant prediction"
ylab <- "Area Under the ROC Curve"
p2 <- dotPlotsSuperImp(dat$DF,ylim,xlab,ylab)
###

p3 <- grid.arrange(p1, p2, ncol=2, widths = c(1,1))
print(p3)

####################
####################

}
main()
















